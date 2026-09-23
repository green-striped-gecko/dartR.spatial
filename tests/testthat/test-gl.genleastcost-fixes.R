# Approved changes from function-review/reports/dartR.spatial/gl.genleastcost.md
skip_if_not_installed("gdistance")
skip_if_not_installed("mmod")

possums <- dartR.data::possums.gl
keep <- unlist(lapply(c("A", "B", "C"), function(p)
  head(which(pop(possums) == p), 10)))
x <- possums[keep, 1:100]
fric <- raster::aggregate(readRDS(system.file(
  "extdata", "landscape.sim.rdata", package = "dartR.data")), 5)
cp <- apply(x@other$xy, 2, function(a) tapply(a, pop(x), mean))
glc <- function(...) gl.genleastcost(x, plotpath = FALSE, verbose = 0, ...)

test_that("every raster input type runs and all layers are used (change 1)", {
  ref <- glc(fric.raster = fric)$cost.mats[[1]]
  f <- tempfile(fileext = ".tif")
  raster::writeRaster(fric, f)
  expect_equal(glc(fric.raster = f)$cost.mats[[1]], ref,
               ignore_attr = TRUE)
  expect_equal(glc(fric.raster = terra::rast(fric))$cost.mats[[1]], ref,
               ignore_attr = TRUE)
  stk <- raster::stack(fric, fric * 2)
  names(stk) <- c("base", "double")
  r <- glc(fric.raster = stk)
  expect_named(r$cost.mats, c("base", "double"))
  expect_equal(r$cost.mats$double, 2 * r$cost.mats$base)
  expect_length(glc(fric.raster = terra::rast(stk))$cost.mats, 2)
  expect_error(glc(fric.raster = matrix(1, 3, 3)), "fric.raster must be")
})

test_that("cost distances equal gl.costdistances and theta is used (change 2)", {
  for (pt in c("leastcost", "commute")) {
    expect_equal(glc(fric.raster = fric, pathtype = pt)$cost.mats[[1]],
                 gl.costdistances(fric, cp, pt, 8, verbose = 0),
                 ignore_attr = TRUE)
  }
  a <- glc(fric.raster = fric, pathtype = "rSPDistance",
           theta = 0.001)$cost.mats[[1]]
  b <- glc(fric.raster = fric, pathtype = "rSPDistance",
           theta = 0.0001)$cost.mats[[1]]
  expect_equal(a, gl.costdistances(fric, cp, "rSPDistance", 8, verbose = 0,
                                   theta = 0.001), ignore_attr = TRUE)
  expect_false(isTRUE(all.equal(a, b)))
})

test_that("propShared is a distance (change 3)", {
  r <- glc(fric.raster = fric, gen.distance = "propShared")
  expect_equal(r$gen.mat, 1 - gl.propShared(x), ignore_attr = TRUE)
  expect_true(all(diag(r$gen.mat) == 0))
  within <- mean(r$gen.mat[1:10, 1:10][lower.tri(diag(10))])
  between <- mean(r$gen.mat[21:30, 1:10])
  expect_lt(within, between)
})

test_that("kosman runs on the genlight (change 4)", {
  r <- glc(fric.raster = fric, gen.distance = "kosman")
  expect_equal(r$gen.mat,
               as.matrix(as.dist(gl.kosman(x, verbose = 0)$kosman)),
               ignore_attr = TRUE)
})

test_that("plotpath = FALSE draws nothing (change 5)", {
  pdf_size <- function(expr) {
    f <- tempfile(fileext = ".pdf")
    grDevices::pdf(f)
    force(expr)
    grDevices::dev.off()
    file.size(f)
  }
  empty <- pdf_size(NULL)
  expect_equal(pdf_size(glc(fric.raster = fric, gen.distance = "D")), empty)
  r <- NULL
  drawn <- pdf_size(r <- gl.genleastcost(x, fric, gen.distance = "D",
                                         plotpath = TRUE, verbose = 0))
  expect_gt(drawn, empty)
  expect_equal(dim(r$pathlength.mats[[1]]), c(3, 3))
})

test_that("invalid inputs stop early with clear messages (change 7)", {
  expect_error(glc(fric.raster = fric, pathtype = "least"),
               "pathtype must be")
  expect_error(glc(fric.raster = fric, gen.distance = "Fst"),
               "gen.distance must be")
  expect_error(glc(fric.raster = fric, NN = 5), "NN must be")
  x6 <- x
  x6@other$xy <- NULL
  x6@other$latlon <- NULL
  expect_error(gl.genleastcost(x6, fric, plotpath = FALSE, verbose = 0),
               "No coordinates found")
  xs <- dartR.data::testset.gs[1:10, 1:20]
  expect_error(gl.genleastcost(xs, fric, plotpath = FALSE, verbose = 0),
               "SNP")
})

test_that("default colours are silent at verbose = 0 (change 9)", {
  expect_silent(gl.genleastcost(x, fric, gen.distance = "D", plotpath = FALSE,
                                verbose = 0))
})
