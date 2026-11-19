# Approved changes from function-review/reports/dartR.spatial/gl2shp.md
x <- dartR.data::testset.gl[1:20, 1:30]

test_that("NA in non-coordinate metrics keeps the individual (change 1)", {
  x1 <- x
  x1@other$ind.metrics$sex[1:5] <- NA
  v <- gl2shp(x1, outpath = tempdir(), verbose = 0)
  expect_equal(nrow(v), 20)
  expect_true(all(is.na(v$sex[1:5])))
})

test_that("missing coordinates remove individuals with a warning (changes 1, 7)", {
  x2 <- x
  x2@other$latlon$lat[1:2] <- NA
  expect_output(v <- gl2shp(x2, outpath = tempdir(), verbose = 1),
                "Removed 2 individual")
  expect_equal(nrow(v), 18)
  expect_equal(v$id, indNames(x)[-(1:2)])
  expect_silent(gl2shp(x, outpath = tempdir(), verbose = 0))
})

test_that("attribute table is written once with sample ids (change 2)", {
  out <- tempfile(); dir.create(out)
  v <- gl2shp(x, outfile = "pts", outpath = out, verbose = 0)
  expect_equal(v$id, indNames(x))
  back <- terra::vect(file.path(out, "pts.shp"))
  expect_false(any(grepl("\\.1$", names(back))))
  expect_equal(back$id, indNames(x))
  expect_equal(unname(terra::crds(back)[, "x"]), x@other$latlon$lon)
  expect_equal(unname(terra::crds(back)[, "y"]), x@other$latlon$lat)
})

test_that("invalid type and outpath error (changes 3, 6)", {
  expect_error(gl2shp(x, type = "gpkg", outpath = tempdir(), verbose = 0),
               "type must be")
  expect_error(gl2shp(x, type = "KML", outpath = tempdir(), verbose = 0),
               "type must be")
  expect_error(gl2shp(x, outpath = file.path(tempdir(), "nope-gl2shp"),
                      verbose = 0), "does not exist")
})

test_that("non-dartR input shapes are accepted (change 4)", {
  x3 <- x
  x3@other$ind.metrics <- NULL
  v <- gl2shp(x3, outpath = tempdir(), verbose = 0)
  expect_equal(v$id, indNames(x))
  x5 <- x
  x5@other$latlon <- as.matrix(x@other$latlon)
  expect_equal(nrow(gl2shp(x5, outpath = tempdir(), verbose = 0)), 20)
  x6 <- x
  names(x6@other$latlon) <- c("lat", "long")
  expect_equal(nrow(gl2shp(x6, outpath = tempdir(), verbose = 0)), 20)
  x7 <- x
  names(x7@other$latlon) <- c("y", "x")
  expect_error(gl2shp(x7, outpath = tempdir(), verbose = 0),
               "columns named lat and lon")
})

test_that("no complete coordinates errors; rename warning is gated (A1, A2)", {
  xz <- x
  xz@other$latlon$lat <- NA
  expect_error(gl2shp(xz, outpath = tempdir(), verbose = 0),
               "No individuals with complete coordinates")
  x6 <- x
  names(x6@other$latlon) <- c("lat", "long")
  expect_silent(gl2shp(x6, outpath = tempdir(), verbose = 0))
})
