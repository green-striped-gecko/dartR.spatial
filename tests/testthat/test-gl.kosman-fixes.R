# Approved changes from function-review/reports/dartR.spatial/gl.kosman.md

# Independent Kosman & Leonard (2005): mean over loci called in both
# individuals of |a - b| / ploidy
kosman_reference <- function(x) {
  m <- as.matrix(x)
  pl <- unique(ploidy(x))
  n <- nrow(m)
  d <- matrix(NA_real_, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    ok <- !is.na(m[i, ]) & !is.na(m[j, ])
    d[i, j] <- if (i == j) 0 else mean(abs(m[i, ok] - m[j, ok]) / pl)
  }
  d
}

test_that("vectorised distances match an independent computation (change 1)", {
  for (x in list(dartR.data::testset.gl[1:15, 1:200],
                 dartR.data::testset.gs[1:15, 1:200],
                 dartR.data::platypus.gl[1:15, ])) {
    r <- gl.kosman(x, verbose = 0)
    lo <- lower.tri(r$kosman, diag = TRUE)
    expect_equal(r$kosman[lo], kosman_reference(x)[lo], tolerance = 1e-12)
    expect_true(all(is.na(r$kosman[upper.tri(r$kosman)])))
  }
})

test_that("tetraploid dosages use |a - b| / 4 (change 1)", {
  m <- rbind(a = c(0, 4, 2, NA), b = c(4, 4, 1, 3), c = c(1, 0, 2, 3))
  x <- new("genlight", m, ploidy = 4)
  r <- gl.kosman(x, verbose = 0)
  expect_equal(r$kosman["b", "a"], mean(c(4, 0, 1) / 4))
  expect_equal(r$kosman["c", "b"], mean(c(3, 4, 1, 0) / 4))
})

test_that("ploidy and input errors are clear (change 2)", {
  x <- dartR.data::testset.gl[1:6, 1:20]
  ploidy(x) <- c(2, 2, 2, 4, 4, 4)
  expect_error(gl.kosman(x, verbose = 0), "different ploidies")
  expect_error(gl.kosman(matrix(0, 3, 3), verbose = 0),
               "must be a genlight")
})

test_that("pairs without shared loci warn (change 4)", {
  m <- rbind(a = c(0, 1, NA, NA), b = c(NA, NA, 2, 0), c = c(1, 1, 1, 1))
  x <- new("genlight", m, ploidy = 2)
  expect_output(r <- gl.kosman(x, verbose = 1), "1 pair")
  expect_true(is.nan(r$kosman["b", "a"]))
  expect_equal(r$nloci["b", "a"], 0)
})

test_that("nloci diagonal counts loci called in the individual (change 5)", {
  x <- dartR.data::platypus.gl[1:6, 1:50]
  r <- gl.kosman(x, verbose = 0)
  expect_equal(unname(diag(r$nloci)), unname(rowSums(!is.na(as.matrix(x)))))
})

test_that("verbose prints the end flag (change 6)", {
  expect_output(gl.kosman(dartR.data::testset.gl[1:4, 1:10], verbose = 1),
                "Completed: gl.kosman")
})
