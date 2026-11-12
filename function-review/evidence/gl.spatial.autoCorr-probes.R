suppressMessages(devtools::load_all(quiet = TRUE))
grDevices::pdf(NULL)
p <- function(label, expr) {
  cat("\n=== ", label, " ===\n")
  r <- tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
  if (is.character(r)) print(r)
  invisible(r)
}
first_r <- function(res) round(res[[1]]$r[1:3], 4)

# Simulated isolation by distance: 80 individuals on a 1-degree transect,
# allele frequencies follow a smooth gradient, so near neighbours are more
# similar and r in the first class must be clearly positive.
set.seed(42)
n <- 80; L <- 400
lon <- sort(runif(n, 150, 151))
slope <- rnorm(L, 0, 3)
p0 <- runif(L, 0.2, 0.8)
freq <- sapply(seq_len(L), function(l) plogis(qlogis(p0[l]) + slope[l] * (lon - 150.5)))
g <- matrix(rbinom(n * L, 2, freq), n)
sim <- new("dartR", g, ploidy = 2)
indNames(sim) <- paste0("i", 1:n); locNames(sim) <- paste0("l", 1:L)
pop(sim) <- rep("P", n)
sim@other$latlon <- data.frame(lat = -30, lon = lon)
sim <- gl.compliance.check(sim, verbose = 0)

cat("=== P1 sign of r in first three classes (strong IBD, expect positive) ===\n")
for (m in c("Euclidean", "Simple", "Absolute", "propShared", "grm")) {
  r <- gl.spatial.autoCorr(sim, Dgen_method = m, bins = 5, permutation = FALSE,
                           bootstrap = FALSE, plot.out = FALSE, verbose = 0)
  cat(sprintf("%-11s r = %s\n", m, paste(first_r(r), collapse = "  ")))
}
# reference: GenAlEx-style squared Euclidean genetic distance, not reversed
geo <- as.matrix(dist(dismo::Mercator(sim@other$latlon[, c("lon", "lat")])))
sqE <- as.matrix(dist(g))^2
E <- as.matrix(dist(g))
ref <- function(D) gl.spatial.autoCorr(x = NULL, Dgeo = geo, Dgen = D, bins = 5,
                     permutation = FALSE, bootstrap = FALSE, plot.out = FALSE, verbose = 0)
cat(sprintf("%-11s r = %s\n", "sqEuclid", paste(first_r(ref(sqE)), collapse = "  ")))
cat(sprintf("%-11s r = %s\n", "Euclid raw", paste(first_r(ref(E)), collapse = "  ")))
cat(sprintf("%-11s r = %s\n", "1-Euclid", paste(first_r(ref(1 - E)), collapse = "  ")))
# PopGenReport reference on squared Euclidean with the same (0-based) bins
if (requireNamespace("PopGenReport", quietly = TRUE)) {
  pg <- PopGenReport:::spautocor(sqE, geo, shuffle = FALSE, bins = 5)
  cat("PopGenReport spautocor (sqEuclid) r.uc:", round(pg$r[1:3], 4), "\n")
  ours <- utils.spautocor(sqE, geo, bins = 5)
  cat("utils.spautocor (sqEuclid) r.uc:        ", round(ours$r.uc[1:3], 4), "\n")
}

# P2 automatic bins when distances do not start near zero
platy <- platypus.gl[pop(platypus.gl) == "TENTERFIELD", 1:300]
r2 <- gl.spatial.autoCorr(platy, Dgeo_trans = "log(Dgeo + 1)", bins = 5,
                          permutation = FALSE, bootstrap = FALSE, plot.out = FALSE, verbose = 0)
cat("\n=== P2 log(Dgeo + 1), bins = 5 ===\n"); print(r2[[1]][, c("Bin", "N")])
lg <- log(as.matrix(dist(dismo::Mercator(platy@other$latlon[, c("lon", "lat")]))) + 1)
cat("range of log distances:", round(range(lg[lower.tri(lg)]), 3),
    " total pairs:", sum(lower.tri(lg)), "\n")

# P3 genlight and matrices both supplied
cat("\n=== P3 genlight + matrices ===\n")
D0 <- as.matrix(dist(matrix(runif(nInd(platy) * 2), ncol = 2)))
r3 <- gl.spatial.autoCorr(platy, Dgeo = D0, Dgen = D0, bins = 3, permutation = FALSE,
                          bootstrap = FALSE, plot.out = FALSE, verbose = 1)
print(r3[[1]][, c("Bin", "N")])

# P4 / P5 list and missing-x input
p("P5 matrices without x", gl.spatial.autoCorr(Dgeo = D0, Dgen = D0, bins = 3, plot.out = FALSE, verbose = 0))
p("P4 named list, x = NULL", gl.spatial.autoCorr(x = NULL, Dgeo = list(a = D0), Dgen = list(a = D0),
                                                 bins = 3, plot.out = FALSE, verbose = 0))
p("P9 matrix input, plot.out = TRUE", gl.spatial.autoCorr(x = NULL, Dgeo = D0, Dgen = D0, bins = 3,
                                                          reps = 19, verbose = 0))

# P6 docs say distances > 1000 are divided by 1000
r6 <- gl.spatial.autoCorr(platy, bins = 3, permutation = FALSE, bootstrap = FALSE,
                          plot.out = FALSE, verbose = 0)
cat("\n=== P6 returned Bin values ===\n"); print(r6[[1]]$Bin)

# P11 Mercator vs geodesic distance
ll <- platy@other$latlon[, c("lon", "lat")]
merc <- as.matrix(dist(dismo::Mercator(ll)))
geod <- as.matrix(terra::distance(as.matrix(ll), lonlat = TRUE))
ratio <- merc[lower.tri(merc)] / geod[lower.tri(geod)]
cat("\n=== P11 Mercator / geodesic distance ratio (TENTERFIELD, lat ~ -29) ===\n")
print(round(summary(ratio), 4))
cat("expected 1/cos(29 deg) =", round(1 / cos(29 * pi / 180), 4), "\n")

# P12 one-tail p-value floor
set.seed(1)
r12 <- gl.spatial.autoCorr(sim, bins = 5, reps = 19, bootstrap = FALSE, plot.out = FALSE, verbose = 0)
cat("\n=== P12 p.one.tail with reps = 19 ===\n"); print(r12[[1]]$p.one.tail)

# P13 dependency guard: dartR.popgen is only checked, never used
cat("\n=== P13 dartR.popgen references in code ===\n")
print(grep("dartR.popgen", readLines("R/gl.spatial.autoCorr.r"), value = TRUE))

# P14 files written to tempdir per call
before <- list.files(tempdir())
invisible(gl.spatial.autoCorr(platy, bins = 3, reps = 5, plot.out = FALSE, verbose = 0))
cat("\n=== P14 new tempdir files:", setdiff(list.files(tempdir()), before), "\n")
