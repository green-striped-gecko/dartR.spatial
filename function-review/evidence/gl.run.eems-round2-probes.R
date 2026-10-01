# Round 2 probes for gl.run.eems (YFT bug report, 2026-10-01).
# Run from the package root with the R 4.4 install:
#   /usr/local/bin/Rscript function-review/evidence/gl.run.eems-round2-probes.R
# Needs runeems_snps in ~/programs (or DARTR_EEMS_BIN).
# Writes run directories under tempdir(); nothing in the package is modified.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dartR.base)
})
set.seed(17)
bin_dir <- normalizePath(Sys.getenv("DARTR_EEMS_BIN", "~/programs"))
grDevices::pdf(tempfile(fileext = ".pdf"))

x <- dartR.data::bandicoot.gl
ll <- data.frame(lon = x@other$latlon$lon, lat = x@other$latlon$lat)
cat("bandicoot.gl:", nInd(x), "individuals,", nLoc(x), "SNPs\n")
cat("latitude span:", round(diff(range(ll$lat)), 1), "deg; longitude span:",
    round(diff(range(ll$lon)), 1), "deg\n")
site_km <- sp::spDists(as.matrix(ll), longlat = TRUE)
cat("max great-circle distance between samples (km):",
    round(max(site_km)), "\n\n")

# --- P1: current function -------------------------------------------------
out1 <- tempfile("round2-current-"); dir.create(out1)
res1 <- gl.run.eems(x, eems.path = bin_dir, out.dir = out1, nDemes = 50,
                    numMCMCIter = 2000, numBurnIter = 1000, numThinIter = 9,
                    seed = 17, dpi = 50, cleanup = FALSE, verbose = 0)
run1 <- Sys.glob(file.path(out1, "eems-run-*"))
coord1 <- as.matrix(read.table(file.path(run1, "eems.coord")))
cat("P1 eems.coord first row (as written):", coord1[1, ], "\n")
cat("P1 input lon/lat first row:          ", unlist(ll[1, ]), "\n")

# What rdist03 plots: reemsplots2 always computes sp::spDists(longlat = TRUE)
# on the observed-deme coordinates, i.e. it reads them as degrees.
od1 <- as.matrix(read.table(file.path(run1, "data_eems", "rdistoDemes.txt")))
plotted <- res1$rdist03$data$fitted
# Truth: the same deme centres converted back from Mercator to lon/lat.
od1_ll <- dismo::Mercator(od1[, 1:2], inverse = TRUE)
true_km <- sp::spDists(od1_ll, longlat = TRUE)
true_km <- true_km[upper.tri(true_km)]
# rdist03 drops pairs involving singleton demes; apply the same filter.
sz <- outer(od1[, 3], od1[, 3], pmin)
keep <- sz[upper.tri(sz)] > 1
cat("P1 rdist03 x-axis range (km, as plotted): ",
    round(range(plotted)), "\n")
cat("P1 true great-circle range (km, same demes):",
    round(range(true_km[keep])), "\n")
cat("P1 correlation plotted vs true distance:",
    round(stats::cor(plotted, true_km[keep]), 3), "\n")
xr <- ggplot2::ggplot_build(res1$mrates01)$layout$panel_params[[1]]$x.range
cat("P1 mrates01 x-axis range:", format(xr, big.mark = ","), "\n")
cat("P1 params.ini has a distance line:",
    any(grepl("^distance", readLines(file.path(run1, "params.ini")))), "\n")

# Buffer units: Mercator metres equal ground metres only at the equator.
scale <- 1 / cos(mean(ll$lat) * pi / 180)
cat("P1 Mercator scale factor at mean latitude", round(mean(ll$lat), 1),
    "deg:", round(scale, 3), "-> buffer = 10000 is",
    round(10000 / scale), "ground metres there\n\n")

# --- P2: longlat = FALSE (the suggested minimal alternative) ---------------
res2 <- reemsplots2::make_eems_plots(file.path(run1, "data_eems"),
                                     longlat = FALSE, dpi = 50)
xr2 <- ggplot2::ggplot_build(res2$mrates01)$layout$panel_params[[1]]$x.range
cat("P2 longlat = FALSE mrates01 x-axis range:", format(xr2, big.mark = ","),
    "(axes swapped: x now shows northing)\n")
cat("P2 longlat = FALSE rdist03 range (km):",
    round(range(res2$rdist03$data$fitted)), "\n\n")

# --- P3: prototype of the proposed fix (lon/lat + greatcirc) ---------------
# Hull and buffer in a local equal-area projection centred on the samples,
# so `buffer` is in ground metres; edges densified before returning to
# lon/lat so the outline follows the projected shape.
lonlat_habitat <- function(ll, buffer) {
  crs_local <- sprintf("+proj=laea +lat_0=%f +lon_0=%f +datum=WGS84 +units=m",
                       mean(ll$lat), mean(ll$lon))
  pts <- sf::st_transform(sf::st_as_sf(ll, coords = c("lon", "lat"),
                                       crs = 4326), crs_local)
  hull <- sf::st_convex_hull(sf::st_union(pts))
  poly <- sf::st_buffer(hull, buffer)
  poly <- sf::st_segmentize(poly, dfMaxLength = 50000)
  sf::st_coordinates(sf::st_transform(poly, 4326))[, 1:2]
}
run_proto <- function(ll, outer, tag) {
  run <- tempfile(paste0("round2-", tag, "-")); dir.create(run)
  dp <- file.path(run, "eems"); mp <- file.path(run, "data_eems")
  D <- local({
    G <- as.matrix(x); m <- colMeans(G, na.rm = TRUE)
    G[is.na(G)] <- m[col(G)[is.na(G)]]
    S <- G %*% t(G) / ncol(G); d <- diag(S)
    outer(d, rep(1, nrow(G))) + outer(rep(1, nrow(G)), d) - 2 * S
  })
  utils::write.table(D, paste0(dp, ".diffs"), col.names = FALSE,
                     row.names = FALSE, quote = FALSE)
  utils::write.table(ll, paste0(dp, ".coord"), col.names = FALSE,
                     row.names = FALSE, quote = FALSE)
  utils::write.table(outer, paste0(dp, ".outer"), col.names = FALSE,
                     row.names = FALSE, quote = FALSE)
  writeLines(c(paste0("datapath = ", dp), paste0("mcmcpath = ", mp),
               paste0("nIndiv = ", nInd(x)), paste0("nSites = ", nLoc(x)),
               "nDemes = 50", "diploid = TRUE", "distance = greatcirc",
               "numMCMCIter = 2000", "numBurnIter = 1000",
               "numThinIter = 9"), file.path(run, "params.ini"))
  st <- system2(file.path(bin_dir, "runeems_snps"),
                c("--params", shQuote(file.path(run, "params.ini")),
                  "--seed", "17"),
                stdout = file.path(run, "eems.log"),
                stderr = file.path(run, "eems.log"))
  list(status = st, mcmc = mp, log = file.path(run, "eems.log"))
}

p3 <- run_proto(ll, lonlat_habitat(ll, 10000), "lonlat")
cat("P3 EEMS exit status (distance = greatcirc):", p3$status, "\n")
cat("P3 eemsrun.txt distance line:",
    grep("distance", readLines(file.path(p3$mcmc, "eemsrun.txt")),
         value = TRUE), "\n")
res3 <- reemsplots2::make_eems_plots(p3$mcmc, longlat = TRUE, dpi = 50)
od3 <- as.matrix(read.table(file.path(p3$mcmc, "rdistoDemes.txt")))
cat("P3 rdist03 x-axis range (km):",
    round(range(res3$rdist03$data$fitted)), "\n")
cat("P3 deme coordinates are within the sample lon/lat box (+/- 5 deg):",
    all(od3[, 1] > min(ll$lon) - 5 & od3[, 1] < max(ll$lon) + 5 &
          od3[, 2] > min(ll$lat) - 5 & od3[, 2] < max(ll$lat) + 5), "\n")
xr3 <- ggplot2::ggplot_build(res3$mrates01)$layout$panel_params[[1]]$x.range
cat("P3 mrates01 x-axis range (degrees):", round(xr3, 2), "\n")
cat("P3 observed demes:", nrow(od3), "for", nrow(unique(ll)),
    "distinct sample locations\n\n")

# --- P4: a user-supplied non-convex habitat (proposed `habitat =`) ---------
# Cut the Great Australian Bight out of the hull: a concave, single ring.
hull_ll <- lonlat_habitat(ll, 10000)
hull_sf <- sf::st_sfc(sf::st_polygon(list(hull_ll)), crs = 4326)
bight <- sf::st_sfc(sf::st_polygon(list(rbind(
  c(124, -45), c(136, -45), c(136, -32.5), c(124, -32.5), c(124, -45)))),
  crs = 4326)
suppressMessages(sf::sf_use_s2(FALSE))
concave <- suppressMessages(suppressWarnings(sf::st_difference(hull_sf, bight)))
cat("P4 concave habitat geometry type:",
    as.character(sf::st_geometry_type(concave)), "; rings:",
    length(concave[[1]]), "\n")
p4 <- run_proto(ll, sf::st_coordinates(concave)[, 1:2], "concave")
cat("P4 EEMS exit status with concave user habitat:", p4$status, "\n")
cat("P4 EEMS habitat check passed:",
    any(grepl("Habitat::initialize\\] Done", readLines(p4$log))), "\n\n")

# --- P5: clip.land on the motivating YFT geometry --------------------------
# YFT inputs were written by the current function (Mercator); invert them.
yft <- "~/YFT/outputs/work/eems_demes100/EEMS_demes100.coord"
if (file.exists(yft)) {
  yft_ll <- unique(dismo::Mercator(as.matrix(read.table(yft)),
                                   inverse = TRUE))
  cat("P5 YFT sites:", nrow(yft_ll), "; lon", round(range(yft_ll[, 1]), 1),
      "; lat", round(range(yft_ll[, 2]), 1), "\n")
  yft_hull <- sf::st_convex_hull(sf::st_union(sf::st_as_sf(
    data.frame(lon = yft_ll[, 1], lat = yft_ll[, 2]),
    coords = c("lon", "lat"), crs = 4326)))
  land <- sf::st_union(rnaturalearth::ne_countries(scale = 50,
                                                  returnclass = "sf"))
  sea <- suppressMessages(suppressWarnings(sf::st_difference(yft_hull, land)))
  pieces <- sf::st_cast(sea, "POLYGON")
  areas <- sort(as.numeric(sf::st_area(pieces)), decreasing = TRUE) / 1e6
  cat("P5 hull minus land:", length(pieces), "polygons; largest two (km^2):",
      format(round(areas[1:2]), big.mark = ","), "\n")
} else {
  cat("P5 skipped: YFT coordinates not found\n")
}
grDevices::dev.off()
