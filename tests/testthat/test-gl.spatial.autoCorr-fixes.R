# Approved changes from function-review/reports/dartR.spatial/gl.spatial.autoCorr.md

# Simulated isolation by distance: neighbours are more related, so r in the
# first distance class must be positive for every method
make_ibd <- function(ploidy = 2) {
  set.seed(42)
  n <- 80
  L <- 400
  lon <- sort(runif(n, 150, 151))
  slope <- rnorm(L, 0, 3)
  p0 <- runif(L, 0.2, 0.8)
  freq <- sapply(seq_len(L), function(l)
    plogis(qlogis(p0[l]) + slope[l] * (lon - 150.5)))
  g <- matrix(rbinom(n * L, 2, freq), n)
  if (ploidy == 1) g <- (g > 0) * 1
  x <- new("dartR", g, ploidy = ploidy)
  indNames(x) <- paste0("i", 1:n)
  locNames(x) <- paste0("l", 1:L)
  pop(x) <- rep("P", n)
  x@other$latlon <- data.frame(lat = -30, lon = lon)
  gl.compliance.check(x, verbose = 0)
}
quick <- function(x, ...) {
  gl.spatial.autoCorr(x, bins = 5, permutation = FALSE, bootstrap = FALSE,
                      plot.out = FALSE, verbose = 0, ...)[[1]]
}

test_that("every method gives positive r for close neighbours (change 1)", {
  snp <- make_ibd(2)
  for (m in c("Euclidean", "Simple", "Absolute", "propShared", "grm")) {
    r <- quick(snp, Dgen_method = m)$r
    expect_gt(r[1], 0)
    expect_true(all(abs(r) <= 1))
  }
  pa <- make_ibd(1)
  for (m in c("Euclidean", "Simple", "Jaccard", "Bray-Curtis")) {
    expect_gt(quick(pa, Dgen_method = m)$r[1], 0)
  }
})

test_that("Euclidean is squared and grm equals its implied distance (changes 1, 2)", {
  snp <- make_ibd(2)
  g <- as.matrix(snp)
  geo <- as.matrix(terra::distance(as.matrix(snp@other$latlon[, c("lon", "lat")]),
                                   lonlat = TRUE))
  ref <- utils.spautocor(as.matrix(dist(g))^2, geo, bins = 5)
  res <- quick(snp, Dgen_method = "Euclidean")
  expect_equal(res$r.uc, ref$r.uc)
  G <- as.matrix(gl.grm2(snp, plotheatmap = FALSE, verbose = 0))
  refg <- utils.spautocor(outer(diag(G), diag(G), "+") - 2 * G, geo, bins = 5)
  expect_equal(quick(snp, Dgen_method = "grm")$r.uc, refg$r.uc)
})

test_that("automatic classes start at the minimum distance (change 3)", {
  set.seed(3)
  xy <- cbind(runif(30, 50000, 60000), runif(30, 0, 1))
  Dgeo <- as.matrix(dist(xy)) + 50000
  diag(Dgeo) <- 0
  Dgen <- as.matrix(dist(matrix(rbinom(30 * 50, 2, 0.4), 30)))
  out <- utils.spautocor(Dgen, Dgeo, bins = 5)
  expect_true(all(out$N > 0))
  expect_equal(sum(out$N), 435)
})

test_that("lon/lat distances are geodesic (change 4)", {
  x <- dartR.data::platypus.gl[pop(dartR.data::platypus.gl) == "TENTERFIELD",
                               1:100]
  res <- quick(x)
  ll <- as.matrix(x@other$latlon[, c("lon", "lat")])
  geod <- as.matrix(terra::distance(ll, lonlat = TRUE))
  # class width is rounded to 4 significant digits
  expect_equal(max(res$Bin), max(geod), tolerance = 1e-3)
})

test_that("matrix and list inputs work without x (change 5)", {
  set.seed(1)
  xy <- matrix(runif(40, 0, 100), 20)
  Dgeo <- as.matrix(dist(xy))
  Dgen <- as.matrix(dist(matrix(rbinom(20 * 50, 2, 0.4), 20)))
  run <- function(...) gl.spatial.autoCorr(..., bins = 4, reps = 9,
                                           plot.out = FALSE, verbose = 0)
  expect_named(run(Dgeo = Dgeo, Dgen = Dgen), "Pop1")
  expect_named(run(Dgeo = list(a = Dgeo, b = Dgeo),
                   Dgen = list(a = Dgen, b = Dgen)), c("a", "b"))
  expect_named(run(Dgeo = list(a = as.dist(Dgeo)),
                   Dgen = list(a = as.dist(Dgen))), "a")
  expect_error(run(Dgeo = Dgeo), "Provide either a genlight")
})

test_that("one-tail p is never below 1 / (reps + 1) (change 6)", {
  res <- gl.spatial.autoCorr(make_ibd(2), bins = 5, reps = 19,
                             bootstrap = FALSE, plot.out = FALSE,
                             verbose = 0)[[1]]
  expect_true(all(res$p.one.tail >= 1 / 20))
})

test_that("genlight plus matrices warns that matrices are ignored (change 7)", {
  x <- dartR.data::platypus.gl[pop(dartR.data::platypus.gl) == "TENTERFIELD",
                               1:100]
  D <- as.matrix(dist(matrix(runif(nInd(x) * 2), ncol = 2)))
  expect_output(gl.spatial.autoCorr(x, Dgeo = D, Dgen = D, bins = 3,
                                    permutation = FALSE, bootstrap = FALSE,
                                    plot.out = FALSE, verbose = 1),
                "Dgeo and Dgen are ignored")
})

test_that("plotting gives no deprecation warnings or scale messages (change 9)", {
  x <- dartR.data::platypus.gl[pop(dartR.data::platypus.gl) == "TENTERFIELD",
                               1:100]
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(gl.spatial.autoCorr(x, bins = 3, reps = 9, verbose = 0))
})

test_that("a coordinates data.frame works with several populations (A1)", {
  x <- dartR.data::platypus.gl[, 1:100]
  cc <- x@other$latlon
  quiet <- function(...) gl.spatial.autoCorr(..., bins = 3, permutation = FALSE,
                                             bootstrap = FALSE, plot.out = FALSE,
                                             verbose = 0)
  expect_equal(quiet(x, coordinates = cc), quiet(x))
  expect_error(quiet(x, coordinates = cc[1:5, ]), "one row per individual")
})
