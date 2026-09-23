suppressMessages(devtools::load_all(quiet = TRUE))
p <- function(label, expr) {
  cat("\n=== ", label, " ===\n")
  r <- tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
  if (is.character(r)) print(r)
  invisible(r)
}
possums <- possums.gl
keep <- unlist(lapply(c("A", "B", "C"), function(p) head(which(pop(possums) == p), 10)))
x <- possums[keep, 1:100]; x@other$xy <- possums@other$xy[keep, ]
fric <- raster::aggregate(readRDS(system.file("extdata", "landscape.sim.rdata", package = "dartR.data")), 5)
ft <- terra::rast(fric)
glc <- function(...) gl.genleastcost(x, fric.raster = ft, plotpath = FALSE, verbose = 0, ...)
cp <- apply(x@other$xy, 2, function(a) tapply(a, pop(x), mean))

# P1 RasterLayer input (documented type)
p("P1 RasterLayer input", gl.genleastcost(x, fric, plotpath = FALSE, verbose = 0))
cat("raster::raster(fric) hasValues:", raster::hasValues(raster::raster(fric)), "\n")

# P2 kosman
p("P2 kosman", glc(gen.distance = "kosman"))

# P3 propShared direction
r <- glc(gen.distance = "propShared")
ps <- gl.propShared(x)
cat("\n=== P3 propShared ===\ngen.mat[2,1] =", r$gen.mat[2, 1], " gl.propShared[2,1] =", ps[2, 1],
    " 1 - propShared =", 1 - ps[2, 1], "\n")
cat("within-pop mean gen.mat:", mean(r$gen.mat[1:10, 1:10][lower.tri(diag(10))]),
    " between A-C mean:", mean(r$gen.mat[21:30, 1:10]), "\n")

# P4 theta ignored
a <- tryCatch(glc(gen.distance = "Gst.Nei", pathtype = "rSPDistance", theta = 1)$cost.mats[[1]],
              error = function(e) { cat("theta = 1:", conditionMessage(e)); NULL })
if (is.null(a)) a <- glc(gen.distance = "Gst.Nei", pathtype = "rSPDistance", theta = 0.0001)$cost.mats[[1]]
b <- glc(gen.distance = "Gst.Nei", pathtype = "rSPDistance", theta = 0.001)$cost.mats[[1]]
cat("\n=== P4 theta ===\nidentical (theta=1, or 0.0001 if 1 errors) vs theta=0.001:", identical(a, b), "\n")
cat("gl.costdistances theta=0.001 [2,1]:", gl.costdistances(fric, cp, "rSPDistance", 8, verbose = 0, theta = 0.001)[2, 1],
    " vs glc:", b[2, 1], "\n")

# P5 invalid pathtype / NN
p("P5 pathtype='least'", glc(pathtype = "least"))
p("P5b NN=5", glc(NN = 5))

# P6 no coordinates at all
x6 <- x; x6@other$xy <- NULL; x6@other$latlon <- NULL
p("P6 no coordinates", gl.genleastcost(x6, ft, plotpath = FALSE, verbose = 0))

# P7/P8 cost matrices vs corrected gl.costdistances
for (pt in c("leastcost", "commute")) {
  g <- glc(gen.distance = "Gst.Nei", pathtype = pt)$cost.mats[[1]]
  cd <- gl.costdistances(fric, cp, pt, 8, verbose = 0)
  cat(sprintf("P7 %s: glc[2,1]=%.4f  gl.costdistances[2,1]=%.4f  max rel diff=%.4f\n",
              pt, g[2, 1], cd[2, 1], max(abs(g - cd) / pmax(cd, 1e-12))))
}
# asymmetry of 1/x[2] on a two-value landscape
tr <- gdistance::transition(fric, function(v) 1 / v[2], 8)
m <- as.matrix(gdistance::transitionMatrix(tr)); cat("P7b transition(1/x[2]) symmetric:", isSymmetric(m), "\n")

# P9 multi-layer SpatRaster
ft2 <- c(ft, ft * 2); names(ft2) <- c("base", "double")
r9 <- p("P9 two-layer SpatRaster", gl.genleastcost(x, ft2, plotpath = FALSE, verbose = 0))
if (is.list(r9)) cat("cost.mats:", length(r9$cost.mats), names(r9$cost.mats), "\n")
rs <- raster::stack(fric, fric * 2)
p("P9b RasterStack", gl.genleastcost(x, rs, plotpath = FALSE, verbose = 0))

# P10 plotting with plotpath = FALSE
f <- tempfile(fileext = ".pdf"); grDevices::pdf(f); glc(gen.distance = "D"); grDevices::dev.off()
cat("\n=== P10 plotpath=FALSE: PDF bytes written:", file.size(f), "\n")

# P11 genetic matrix alignment when individual order differs from level order
x11 <- x[c(21:30, 1:20), ]; x11@other$xy <- x@other$xy[c(21:30, 1:20), ]
pop(x11) <- factor(as.character(pop(x11)), levels = c("B", "C", "A"))
r11 <- gl.genleastcost(x11, ft, gen.distance = "Gst.Nei", plotpath = FALSE, verbose = 0)
g_ref <- as.matrix(mmod::pairwise_Gst_Nei(gl2gi(x11, verbose = 0)))
cat("\n=== P11 ===\nmmod labels:", rownames(g_ref), " glc labels:", rownames(r11$gen.mat), "\n")
print(round(r11$gen.mat, 4)); print(round(r$gen.mat[1, 1], 4))
base <- glc(gen.distance = "Gst.Nei")$gen.mat
cat("A-B original:", round(base["A", "B"], 4), " A-B reordered:", round(r11$gen.mat["A", "B"], 4), "\n")

# P12 SilicoDArT
xs <- testset.gs[1:30, 1:100]; xs@other$xy <- x@other$xy; pop(xs) <- pop(x)
p("P12 SilicoDArT", print(round(gl.genleastcost(xs, ft, plotpath = FALSE, verbose = 0)$gen.mat, 4)))

# P13 verbosity and colours
cat("\n=== P13 verbose=0 output ===\n"); invisible(glc(gen.distance = "D"))

# P14 lon/lat raster with latlon fallback: CRS overwritten
ll <- raster::raster(nrows = 50, ncols = 50, xmn = 145, xmx = 150, ymn = -38, ymx = -33, crs = "+proj=longlat +datum=WGS84")
raster::values(ll) <- 1
x14 <- x; x14@other$xy <- NULL
x14@other$latlon <- data.frame(lon = rep(c(146, 147, 149), each = 10), lat = rep(c(-37, -35, -34), each = 10))
r14 <- p("P14 lonlat raster", gl.genleastcost(x14, terra::rast(ll), gen.distance = "Gst.Nei", plotpath = FALSE, verbose = 0))
if (is.list(r14)) {
  cd14 <- gl.costdistances(ll, x14, "leastcost", 8, verbose = 0)
  cat("glc cost A-B:", r14$cost.mats[[1]][2, 1], " gl.costdistances (metres):", cd14[2, 1],
      " geodesic km:", round(terra::distance(cbind(146, -37), cbind(147, -35), lonlat = TRUE) / 1000, 1), "\n")
}
