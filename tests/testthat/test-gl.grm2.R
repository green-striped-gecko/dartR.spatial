# Characterisation captured before critical source review at c685f00.
# Snapshots record current behaviour, defects included.
# Approved snapshot diffs (sync with dartR.captive::gl.grm): SilicoDArT input
# now errors; plot.file with plotheatmap = FALSE no longer fails. SNP matrices
# unchanged.
test_that("gl.grm2 reference-data behaviour is unchanged", {
  local_edition(3)
  withr::local_dir(withr::local_tempdir())
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  cases <- list(
    snp = dartR.data::platypus.gl[1:12, 1:200],
    gs = dartR.data::testset.gs[1:12, 1:100]
  )
  for (nm in names(cases)) {
    x <- cases[[nm]]
    before <- serialize(x, NULL)
    for (heat in c(FALSE, TRUE)) {
      result <- tryCatch(gl.grm2(x, plotheatmap = heat, verbose = 0),
                         error = identity)
      expect_identical(serialize(x, NULL), before)
      summary <- list(case = nm, heatmap = heat, class = class(result))
      if (inherits(result, "error")) {
        summary$error <- conditionMessage(result)
      } else {
        summary$dim <- dim(result)
        summary$names <- head(rownames(result), 3)
        summary$values <- round(result[1:4, 1:4], 6)
      }
      expect_snapshot(print(summary),
                      variant = paste("grm2", nm, heat, sep = "-"))
    }
  }
  plot_file <- tryCatch(
    gl.grm2(cases$snp, plotheatmap = FALSE, plot.file = "g", verbose = 0),
    error = identity)
  expect_snapshot(print(if (inherits(plot_file, "error"))
    conditionMessage(plot_file) else class(plot_file)),
    variant = "grm2-plotfile-noheatmap")
})
