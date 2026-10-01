# Round 2 integration run for gl.run.eems after changes 11-13.
#   /usr/local/bin/Rscript function-review/evidence/gl.run.eems-round2-integration.R
suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dartR.base)
})
bin_dir <- normalizePath(Sys.getenv("DARTR_EEMS_BIN", "~/programs"))
grDevices::pdf(tempfile(fileext = ".pdf"))
x <- dartR.data::bandicoot.gl
ll <- cbind(x@other$latlon$lon, x@other$latlon$lat)

# I1: default habitat, verbose = 3, same settings as round-2 probe P1.
out <- tempfile("round2-after-"); dir.create(out)
res <- gl.run.eems(x, eems.path = bin_dir, out.dir = out, nDemes = 50,
                   numMCMCIter = 2000, numBurnIter = 1000, numThinIter = 9,
                   seed = 17, dpi = 50, cleanup = FALSE, verbose = 3)
run <- Sys.glob(file.path(out, "eems-run-*"))
od <- as.matrix(read.table(file.path(run, "data_eems", "rdistoDemes.txt")))
true_km <- sp::spDists(od[, 1:2], longlat = TRUE)
sz <- outer(od[, 3], od[, 3], pmin)
keep <- sz[upper.tri(sz)] > 1
true_km <- true_km[upper.tri(true_km)][keep]
cat("\nI1 rdist03 range (km):", round(range(res$rdist03$data$fitted)), "\n")
cat("I1 rdist03 equals great-circle distance between observed demes:",
    isTRUE(all.equal(res$rdist03$data$fitted, true_km)), "\n")
cat("I1 max distance between samples (km):",
    round(max(sp::spDists(ll, longlat = TRUE))), "\n")
xr <- ggplot2::ggplot_build(res$mrates01)$layout$panel_params[[1]]$x.range
cat("I1 mrates01 x-axis range (degrees):", round(xr, 1), "\n")
cat("I1 observed demes line in log:",
    grep("observed demes", readLines(file.path(run, "eems.log")),
         value = TRUE), "\n")

# I2: user habitat = hull minus the Great Australian Bight (concave ring).
hull <- sf::st_sfc(sf::st_polygon(list(as.matrix(
  read.table(file.path(run, "eems.outer"))))), crs = 4326)
bight <- sf::st_sfc(sf::st_polygon(list(rbind(
  c(124, -45), c(136, -45), c(136, -32.5), c(124, -32.5), c(124, -45)))),
  crs = 4326)
suppressMessages(sf::sf_use_s2(FALSE))
concave <- suppressMessages(suppressWarnings(sf::st_difference(hull, bight)))
out2 <- tempfile("round2-habitat-"); dir.create(out2)
res2 <- gl.run.eems(x, eems.path = bin_dir, out.dir = out2, nDemes = 50,
                    numMCMCIter = 2000, numBurnIter = 1000,
                    numThinIter = 9, seed = 17, dpi = 50,
                    habitat = concave, cleanup = FALSE, verbose = 2)
run2 <- Sys.glob(file.path(out2, "eems-run-*"))
cat("\nI2 concave habitat run returned", length(res2), "plots\n")
cat("I2 EEMS outer.txt rows:",
    nrow(read.table(file.path(run2, "data_eems", "outer.txt"))), "\n")
grDevices::dev.off()
