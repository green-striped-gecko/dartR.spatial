local_edition(3)
ibd_distances <- function() {
  stats::dist(matrix(c(0, 1, 4, 6, 10, 17), ncol = 1,
                     dimnames = list(LETTERS[1:6], "position")))
}
ibd_run <- function(...) {
  set.seed(2026)
  gl.ibd(..., permutations = 19, plot.out = FALSE, verbose = 0)
}
ibd_individuals <- function() {
  x <- bandicoot.gl[1:6, 1:100]
  indNames(x) <- LETTERS[1:6]
  x@other$xy <- data.frame(x = c(0, 1, 4, 6, 10, 17),
                         y = c(0, 1, 0, 4, 6, 2), row.names = indNames(x))
  x
}
ibd_populations <- function() {
  x <- bandicoot.gl[, 1:100]
  groups <- split(seq_len(nInd(x)), pop(x))
  x <- x[unlist(lapply(groups[1:4], head, 4), use.names = FALSE), ]
  pop(x) <- droplevels(pop(x))
  x
}

test_that("labelled pairs align without changing their identities (F1)", {
  D <- ibd_distances()
  M <- as.matrix(D)
  perm <- c(3, 6, 1, 5, 2, 4)
  result <- ibd_run(Dgen = D, Dgeo = M[perm, perm])
  expect_equal(as.numeric(result$Dgeo), as.numeric(D))
  expect_equal(unname(result$mantel$statistic), 1)
  expect_equal(result$mantel$signif, 0.05)
  expect_identical(attr(result$Dgen, "Labels"), attr(result$Dgeo, "Labels"))
  # Independently shuffled matrix columns are matched to row identities first.
  expect_equal(as.numeric(ibd_run(Dgen = M[, perm], Dgeo = D)$Dgen),
               as.numeric(D))
  bad <- D
  attr(bad, "Labels")[6] <- "Z"
  expect_error(ibd_run(Dgen = D, Dgeo = bad), "same observations")
  attr(bad, "Labels")[6] <- "A"
  expect_error(ibd_run(Dgen = D, Dgeo = bad), "unique")
  colnames(M)[6] <- "Z"
  expect_error(ibd_run(Dgen = M, Dgeo = D), "row and column labels")
  U <- D
  attr(U, "Labels") <- NULL
  expect_equal(as.numeric(ibd_run(Dgen = U, Dgeo = D)$Dgeo), as.numeric(D))
})

test_that("explicit coordinate identities align and invalid coordinates fail (F1 F2)", {
  x <- ibd_individuals()
  coords <- x@other$xy
  before <- serialize(x, NULL)
  base <- ibd_run(x, distance = "euclidean", coordinates = "xy")
  shuffled <- coords[c(3, 6, 1, 5, 2, 4), ]
  result <- ibd_run(x, distance = "euclidean", coordinates = shuffled)
  expect_equal(result$Dgeo, base$Dgeo)
  expect_equal(result$mantel$statistic, base$mantel$statistic)
  expect_identical(serialize(x, NULL), before)
  rownames(shuffled)[6] <- "Z"
  expect_error(ibd_run(x, distance = "euclidean", coordinates = shuffled),
               "row names must match")
  rownames(coords) <- NULL
  expect_equal(ibd_run(x, distance = "euclidean", coordinates = coords)$Dgeo,
               base$Dgeo)
  expect_equal(as.numeric(base$Dgeo)[2], 4)
  for (bad in c(NA, Inf, NaN)) {
    coords$x[3] <- bad
    expect_error(ibd_run(x, distance = "euclidean", coordinates = coords),
                 "Non-finite coordinates for individuals: C")
  }
  coords$x <- letters[1:6]
  expect_error(ibd_run(x, distance = "euclidean", coordinates = coords),
               "must be numeric")
  geographic <- ibd_run(x, distance = "euclidean")
  x@other$latlon$metadata <- letters[1:6]
  expect_equal(ibd_run(x, distance = "euclidean")$Dgeo, geographic$Dgeo)
  # geodesic distances accept the poles; out-of-range degrees are rejected
  x@other$latlon <- data.frame(lon = 1:6, lat = c(0, 10, -90, 20, 30, 40))
  expect_true(all(is.finite(ibd_run(x, distance = "euclidean")$Dgeo)))
  x@other$latlon$lat[3] <- -95
  expect_error(ibd_run(x, distance = "euclidean"), "must be WGS84 degrees")
  p <- ibd_populations()
  p@other$latlon <- p@other$latlon
  p@other$latlon[1, 1] <- NA
  expect_error(ibd_run(p), "Non-finite coordinates")
})

test_that("distance forms and Mantel preconditions are checked (F3)", {
  D <- ibd_distances()
  M <- as.matrix(D)
  base <- ibd_run(Dgen = D, Dgeo = D)
  expect_equal(as.matrix(ibd_run(Dgen = M, Dgeo = M)$Dgen),
               as.matrix(base$Dgen))
  expect_equal(as.matrix(ibd_run(Dgen = D, Dgeo = D,
                       Dgen_trans = "as.matrix(Dgen)")$Dgen), as.matrix(base$Dgen))
  x <- ibd_individuals()
  expect_equal(as.matrix(ibd_run(x, distance = "euclidean", coordinates = "xy",
                       Dgen = M)$Dgen), as.matrix(base$Dgen))
  expect_error(ibd_run(), "Provide both")
  expect_error(ibd_run(Dgen = D), "Provide both")
  expect_error(ibd_run(x, distance = "unknown"), "distance must be")
  expect_error(ibd_run(Dgen = 1:15, Dgeo = D), "numeric square matrix")
  M[1, 2] <- 25
  expect_error(ibd_run(Dgen = M, Dgeo = D), "symmetric")
  expect_error(ibd_run(Dgen = D, Dgeo = dist(1:5)), "same number")
  expect_error(ibd_run(Dgen = dist(1:2), Dgeo = dist(1:2)), "at least three")
  expect_error(ibd_run(Dgen = as.dist(matrix(1, 6, 6)), Dgeo = D), "must vary")
  expect_error(ibd_run(Dgeo = as.dist(matrix(1, 6, 6)), Dgen = D), "must vary")
  for (bad in c(NA, NaN, Inf, -Inf)) {
    badD <- D
    badD[1] <- bad
    expect_error(ibd_run(Dgen = badD, Dgeo = D), "Dgen contains non-finite")
    expect_error(ibd_run(Dgen = D, Dgeo = badD), "Dgeo contains non-finite")
  }
  expect_error(ibd_run(Dgen = D, Dgeo = D, Dgen_trans = "Dgen * NA"),
               "Dgen contains non-finite")
  expect_s3_class(ibd_run(Dgen = -D, Dgeo = D)$mantel, "mantel")
  expect_s3_class(ibd_run(Dgen = D, Dgeo = D,
                          Dgeo_trans = "log(as.matrix(Dgeo))")$Dgeo, "dist")
})

test_that("provided distances bypass irrelevant genlight inputs (F4)", {
  D <- ibd_distances()
  base <- ibd_run(Dgen = D, Dgeo = D)
  expect_equal(ibd_run(x = "ignored", Dgen = D, Dgeo = D), base)
  x <- ibd_individuals()
  pop(x) <- factor(rep("one", nInd(x)))
  expect_equal(ibd_run(x = x, Dgen = D, Dgeo = D), base)
  # A supplied geographic distance does not require coordinates.
  x@other$latlon <- x@other$latlong <- x@other$xy <- NULL
  expect_s3_class(ibd_run(x, Dgeo = D, distance = "euclidean")$mantel, "mantel")
})

test_that("unused plotting and projection dependencies are bypassed (F5 F7)", {
  D <- ibd_distances()
  f <- gl.ibd
  environment(f) <- list2env(list(
    ggplot = function(...) stop("plot was constructed"),
    requireNamespace = function(package, ...) {
      if (package == "terra") FALSE else base::requireNamespace(package, ...)
    }), parent = environment(f))
  expect_silent(result <- f(Dgen = D, Dgeo = D, permutations = 19,
                             plot.out = FALSE, verbose = 0))
  expect_s3_class(result$mantel, "mantel")
  x <- ibd_individuals()
  expect_silent(f(x, distance = "euclidean", coordinates = "xy",
                  permutations = 19, plot.out = FALSE, verbose = 0))
  expect_error(f(x, distance = "euclidean", plot.out = FALSE, verbose = 0),
               "Install package terra")
})

test_that("display and saving preserve numerical results (F5)", {
  x <- ibd_individuals()
  base <- ibd_run(x, distance = "euclidean", coordinates = "xy")
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  for (colours in list(NULL, "ind", "pop")) {
    set.seed(2026)
    shown <- gl.ibd(x, distance = "euclidean", coordinates = "xy",
                    paircols = colours, permutations = 19, verbose = 0)
    expect_equal(shown, base)
  }
  directory <- tempfile("ibd-plot-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  set.seed(2026)
  saved <- gl.ibd(x, distance = "euclidean", coordinates = "xy",
                  permutations = 19, verbose = 0, plot.out = FALSE,
                  plot.dir = directory, plot.file = "isolation")
  expect_equal(saved, base)
  files <- list.files(directory, pattern = "[.]RDS$", full.names = TRUE)
  expect_length(files, 1L)
  expect_s3_class(readRDS(files[1]), "ggplot")
})

test_that("verbosity and function-object calls follow the documented levels (F8 F9)", {
  D <- ibd_distances()
  expect_silent(ibd_run(Dgen = D, Dgeo = D))
  output <- capture.output(result <- do.call(gl.ibd, list(Dgen = D, Dgeo = D,
    permutations = 19, plot.out = FALSE, verbose = 1)))
  expect_true(any(grepl("Starting", output)))
  expect_true(any(grepl("Completed", output)))
  expect_false(any(grepl("Mantel statistic|Transformation|provided distance", output)))
  expect_s3_class(result$mantel, "mantel")
  x <- ibd_individuals()
  expect_silent(ibd_run(x, distance = "kosman"))
  output <- capture.output(gl.ibd(x, distance = "euclidean", permutations = 19,
                                   plot.out = FALSE, verbose = 3))
  expect_true(any(grepl("Mantel statistic", output)))
})

test_that("numerical engines retain independent Euclidean and Mantel results (F6)", {
  x <- testset.gl[1:8, 1:60]
  G <- as.matrix(x)
  hand <- matrix(0, nrow(G), nrow(G))
  for (i in seq_len(nrow(G))) for (j in seq_len(nrow(G))) {
    ok <- !is.na(G[i, ]) & !is.na(G[j, ])
    hand[i, j] <- sqrt(sum((G[i, ok] - G[j, ok])^2) * ncol(G) / sum(ok))
  }
  result <- ibd_run(x, distance = "euclidean")
  expect_equal(as.numeric(result$Dgen), as.numeric(as.dist(hand)))
  D <- ibd_distances()
  M <- as.matrix(D)
  Y <- dist(cbind(c(4, 1, 8, 2, 5, 9), c(1, 4, 0, 8, 2, 3)))
  set.seed(11)
  permutations <- t(replicate(19, sample.int(6)))
  r <- cor(as.numeric(D), as.numeric(Y))
  rperm <- apply(permutations, 1, function(i)
    cor(as.numeric(as.dist(M[i, i])), as.numeric(Y)))
  p <- (sum(rperm >= r - sqrt(.Machine$double.eps)) + 1) / 20
  result <- gl.ibd(Dgen = D, Dgeo = Y, permutations = permutations,
                   plot.out = FALSE, verbose = 0)
  expect_equal(unname(result$mantel$statistic), r)
  expect_equal(result$mantel$signif, p)
  x <- ibd_populations()
  expect_equal(as.numeric(ibd_run(x)$Dgen),
               as.numeric(as.dist(StAMPP::stamppFst(as(x, "genlight"), nboots = 1))))
  for (method in c("D", "propShared", "kosman")) {
    expect_s3_class(ibd_run(x, distance = method)$mantel, "mantel")
  }
})

test_that("small file-backed inputs preserve ordinary results and stay quiet (F8)", {
  skip_if_not_installed("bigsnpr")
  x <- ibd_populations()
  directory <- tempfile("ibd-fbm-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  backed <- gl.gen2fbm(x, backingfile = file.path(directory, "genotypes"), verbose = 0)
  before <- as.matrix(backed)
  for (method in c("euclidean", "Fst")) {
    expect_silent(result <- ibd_run(backed, distance = method))
    reference <- ibd_run(x, distance = method)
    expect_equal(as.numeric(result$Dgen), as.numeric(reference$Dgen))
    expect_equal(as.numeric(result$Dgeo), as.numeric(reference$Dgeo))
  }
  expect_equal(as.matrix(backed), before)
})

test_that("SilicoDArT refuses allele-frequency distances", {
  x <- gl.compliance.check(testset.gs[1:12, 1:200], verbose = 0)
  for (d in c("Fst", "D", "propShared")) {
    expect_error(ibd_run(x, distance = d), "requires SNP data.*euclidean.*kosman")
  }
  x <- x[1:6, ]
  x@other$xy <- data.frame(x = c(0, 1, 4, 6, 10, 17),
                         y = c(0, 1, 0, 4, 6, 2), row.names = indNames(x))
  expect_s3_class(ibd_run(x, distance = "euclidean", coordinates = "xy")$mantel,
                  "mantel")
})
