# Characterisation captured before critical source review at 0bbc487.
# Snapshots record current behaviour, defects included.
# Approved change 2 snapshot diff: duplicated ".1" columns and the sp
# artefact column "optional" removed; coordinates and point counts unchanged.
test_that("gl2shp reference-data behaviour is unchanged", {
  local_edition(3)
  for (dataset in c("testset.gl", "testset.gs")) {
    x <- get(dataset, envir = asNamespace("dartR.data"))[1:20, 1:30]
    before <- serialize(x, NULL)
    for (type in c("shp", "kml")) {
      out <- tempfile("gl2shp")
      dir.create(out)
      result <- tryCatch(
        gl2shp(x, type = type, outfile = "pts", outpath = out, verbose = 0),
        error = identity)
      expect_identical(serialize(x, NULL), before)
      summary <- list(dataset = dataset, type = type,
                      result_class = class(result),
                      files = sort(list.files(out)))
      if (inherits(result, "error")) {
        summary$error <- conditionMessage(result)
      } else {
        summary$n_points <- nrow(result)
        summary$names <- names(result)
        summary$crs_lonlat <- terra::is.lonlat(result)
        summary$coords_head <- round(head(terra::crds(result), 3), 5)
        back <- terra::vect(file.path(out, paste0("pts.", type)))
        summary$file_rows <- nrow(back)
        summary$file_names <- names(back)
      }
      expect_snapshot(print(summary), variant = paste(dataset, type, sep = "-"))
    }
  }
})
