suppressMessages(devtools::load_all(quiet = TRUE))
x <- testset.gl[1:20, 1:30]
o <- tempfile(); dir.create(o)
p <- function(label, expr) {
  cat("\n=== ", label, " ===\n")
  r <- tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
  if (inherits(r, "SpatVector")) cat("points:", nrow(r), "\n") else print(r)
  invisible(r)
}
# P1: NA in a non-coordinate ind.metrics column drops the individual
x1 <- x; x1@other$ind.metrics$sex[1:5] <- NA
p("P1 NA in sex only (coords complete)", gl2shp(x1, outpath = o, verbose = 2))
# P2: missing coordinates
x2 <- x; x2@other$latlon$lat[1:2] <- NA
p("P2 two NA lat", gl2shp(x2, outpath = o, verbose = 2))
# P3: no ind.metrics
x3 <- x; x3@other$ind.metrics <- NULL
p("P3 ind.metrics NULL", gl2shp(x3, outpath = o, verbose = 0))
# P4: unsupported type -> nothing written but success message
o4 <- tempfile(); dir.create(o4)
p("P4 type='gpkg'", gl2shp(x, type = "gpkg", outpath = o4, verbose = 2))
cat("files written:", length(list.files(o4)), "\n")
p("P4b type='KML'", gl2shp(x, type = "KML", outpath = o4, verbose = 0))
cat("files written:", length(list.files(o4)), "\n")
# P5: latlon as matrix
x5 <- x; x5@other$latlon <- as.matrix(x@other$latlon)
p("P5 latlon matrix", gl2shp(x5, outpath = o, verbose = 0))
# P6: 'long' naming
x6 <- x; names(x6@other$latlon) <- c("lat", "long")
p("P6 lat/long names", gl2shp(x6, outpath = o, verbose = 2))
# P7: nonexistent outpath
p("P7 missing outpath", gl2shp(x, outpath = file.path(o, "nope"), verbose = 0))
# P8: duplicated columns in written file
v <- terra::vect(file.path(o, "gl.shp")); print(names(v))
# P9: ind.metrics row order vs latlon: are coordinates attributes consistent?
r <- gl2shp(x, outpath = o, verbose = 0)
print(all.equal(terra::crds(r)[, "y"], r$lat))
# P10: ind.metrics has 'id' column in testset? and duplicated id meaning
print(head(as.data.frame(r)[, c("id", "id.1")], 3))
# P11: long column names truncated in shp
x11 <- x; x11@other$ind.metrics$a_very_long_column_name <- 1
r11 <- gl2shp(x11, outpath = o, outfile = "long", verbose = 0)
print(names(terra::vect(file.path(o, "long.shp"))))
# P12: verbose output at default
p("P12 verbose=3", gl2shp(x, outpath = o, verbose = 3))
# P13: KML with projected proj4
x13 <- x
p("P13 projected proj4 + kml", gl2shp(x13, type = "kml", proj4 = "+proj=utm +zone=55 +south +datum=WGS84", outpath = o, verbose = 0))
