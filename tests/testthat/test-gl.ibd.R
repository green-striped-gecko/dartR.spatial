# Characterisation captured before critical source review at 15a4c7c.
# Approved F3 snapshot changes: incomplete Fst distances now error.
# The four valid Euclidean snapshots retain their original numerical values.
# Original snapshots are preserved in function-review/evidence.
test_that("IBD reference-data behaviour is unchanged", {
  local_edition(3)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  for (dataset in c("testset.gl", "testset.gs")) {
    original_data <- get(dataset, envir = asNamespace("dartR.data"))
    for (method in c("euclidean", "Fst")) {
      keep <- if (method == "Fst") {
        unlist(lapply(split(seq_len(nInd(original_data)), pop(original_data)),
                      head, 3), use.names = FALSE)
      } else seq_len(8)
      x <- original_data[keep, 1:60]
      before <- serialize(x, NULL)
      for (display in c(FALSE, TRUE)) {
        set.seed(2026)
        result <- tryCatch(
          gl.ibd(x, distance = method, permutations = 19,
                 plot.out = display, verbose = 0), error = identity)
        expect_identical(serialize(x, NULL), before)
        summary <- list(
          dataset = dataset, method = method, plot = display,
          dimensions = c(nInd(x), nLoc(x)), ploidy = unique(ploidy(x)),
          metric_rows = c(nrow(x@other$loc.metrics), nrow(x@other$ind.metrics)),
          result_class = class(result))
        if (inherits(result, "error")) {
          summary$error <- conditionMessage(result)
        } else {
          summary$names <- names(result)
          summary$Dgen_size <- attr(result$Dgen, "Size")
          summary$Dgeo_size <- attr(result$Dgeo, "Size")
          summary$Dgen_head <- round(head(as.numeric(result$Dgen)), 8)
          summary$Dgeo_head <- round(head(as.numeric(result$Dgeo)), 4)
          summary$Mantel <- unlist(result$mantel[c("statistic", "signif")])
        }
        variant <- paste(dataset, method, display, sep = "-")
        expect_snapshot(print(summary), variant = variant)
      }
    }
  }
})
