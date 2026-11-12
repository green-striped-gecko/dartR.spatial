# Characterisation at 0bbc487, captured before critical source review.
# Approved changes: F2 fixes genlight coordinates; F4/F6 correct commute;
# F5 replaces undefined RSP results with errors. Original snapshots are in
# function-review/evidence/gl.costdistances-before-snapshots.md.
test_that("cost distances retain their reference-data baseline", {
  skip_if_not_installed("gdistance")
  local_edition(3)
  for (dataset in c("testset.gl", "testset.gs")) {
    original <- get(dataset, envir = asNamespace("dartR.data"))
    groups <- split(seq_len(nInd(original)), pop(original))
    keep <- unlist(lapply(groups[1:3], head, 2), use.names = FALSE)
    x <- original[keep, 1:20]
    before <- serialize(x, NULL)
    coords <- x@other$latlon[, c("lon", "lat")]
    landscape <- raster::raster(nrows = 8, ncols = 8,
      xmn = min(coords$lon) - 1, xmx = max(coords$lon) + 1,
      ymn = min(coords$lat) - 1, ymx = max(coords$lat) + 1,
      crs = "+proj=longlat +datum=WGS84")
    raster::values(landscape) <- 1
    centres <- apply(coords, 2, function(a) tapply(a, pop(x), mean))
    for (method in c("leastcost", "rSPDistance", "commute")) {
      for (input in c("genlight", "matrix")) {
        locations <- if (input == "genlight") x else centres
        messages <- warnings <- character()
        set.seed(2026)
        output <- capture.output(result <- withCallingHandlers(
          tryCatch(gl.costdistances(landscape, locations, method = method,
                                    NN = 8, verbose = 0), error = identity),
          warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }, message = function(m) {
            messages <<- c(messages, conditionMessage(m))
            invokeRestart("muffleMessage")
          }))
        expect_identical(serialize(x, NULL), before)
        summary <- list(dataset = dataset, method = method, input = input,
          dimensions = c(nInd(x), nLoc(x)), ploidy = unique(ploidy(x)),
          metric_rows = c(nrow(x@other$loc.metrics), nrow(x@other$ind.metrics)),
          result_class = class(result), output = output,
          warnings = warnings, messages = messages)
        if (inherits(result, "error")) {
          summary$error <- conditionMessage(result)
        } else {
          summary$result_dimensions <- dim(result)
          summary$result_labels <- dimnames(as.matrix(result))
          summary$values <- signif(as.numeric(result), 9)
        }
        expect_snapshot(print(summary),
          variant = paste(dataset, method, input, sep = "-"))
      }
    }
  }
})
