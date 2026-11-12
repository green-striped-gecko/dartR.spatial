# Characterisation captured before critical source review at 209ef9a.
# Approved snapshot changes: F1 reports the executable failure directly;
# F3 rejects diploid/ploidy mismatches. Successful numerical anchors stay fixed.
# Opt in because this invokes an external EEMS executable and spatial libraries.
test_that("EEMS reference-data baseline is unchanged", {
  skip_if(Sys.getenv("DARTR_EEMS_REVIEW") != "true",
          "Set DARTR_EEMS_REVIEW=true to run the external-tool baseline")
  skip_if_not_installed("reemsplots2")
  skip_if_not_installed("sf")
  binary_dir <- Sys.getenv("DARTR_EEMS_BIN", "~/programs")
  binary_dir <- normalizePath(binary_dir, mustWork = TRUE)
  skip_if_not(file.exists(file.path(binary_dir, "runeems_snps")))
  local_edition(3)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  for (dataset in c("testset.gl", "testset.gs")) {
    x <- get(dataset, envir = asNamespace("dartR.data"))[1:8, 1:60]
    original <- serialize(x, NULL)
    out <- tempfile(paste0("baseline-", dataset, "-"))
    dir.create(out)
    result <- tryCatch(
      gl.run.eems(x, eems.path = binary_dir, out.dir = out,
                  plot.dir = out, nDemes = 20, numMCMCIter = 30,
                  numBurnIter = 10, numThinIter = 1, seed = 17,
                  dpi = 20, cleanup = FALSE, verbose = 0),
      error = function(e) e
    )
    expect_identical(serialize(x, NULL), original)
    summary <- list(
      dataset = dataset, dimensions = c(nInd(x), nLoc(x)),
      ploidy = unique(ploidy(x)),
      metric_rows = c(nrow(x@other$loc.metrics), nrow(x@other$ind.metrics)),
      result_class = class(result),
      result = if (inherits(result, "error"))
        sub("See log: .*", "See log: <run>/eems.log", conditionMessage(result))
               else names(result)
    )
    expect_snapshot(print(summary), variant = dataset)
  }
})

# Added during targeted review, after the pre-review snapshots above.
test_that("the successful SNP path preserves its distance and return baseline", {
  skip_if(Sys.getenv("DARTR_EEMS_REVIEW") != "true")
  skip_if_not_installed("reemsplots2")
  skip_if_not_installed("sf")
  binary_dir <- normalizePath(Sys.getenv("DARTR_EEMS_BIN", "~/programs"),
                              mustWork = TRUE)
  skip_if_not(file.exists(file.path(binary_dir, "runeems_snps")))
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  x <- bandicoot.gl[1:12, 1:200]
  original <- serialize(x, NULL)
  out <- tempfile("eems-snp-baseline-")
  dir.create(out)
  result <- gl.run.eems(x, eems.path = binary_dir, out.dir = out,
                       plot.dir = out, plot.file = "snp_baseline",
                       nDemes = 20, numMCMCIter = 40, numBurnIter = 10,
                       numThinIter = 2, seed = 17, dpi = 10,
                       cleanup = FALSE, verbose = 0)
  expect_identical(serialize(x, NULL), original)
  expect_named(result, c("mrates01", "mrates02", "qrates01", "qrates02",
                         "rdist01", "rdist02", "rdist03", "pilogl01"))
  # F1/F2 retain each run's files in a unique directory under out.dir.
  distance <- as.matrix(read.table(
    Sys.glob(file.path(out, "eems-run-*", "eems.diffs"))
  ))
  expect_equal(dim(distance), c(12L, 12L))
  expect_equal(distance[lower.tri(distance)][1:6],
               c(0.8557326446, 0.8637326446, 0.8922314050,
                 0.7093690083, 0.7554599174, 0.8320962810),
               tolerance = 1e-9)
})

# Round 2 characterisation (2026-10-01), captured before any change for the
# YFT coordinate report: pins the units written for EEMS and those plotted.
test_that("coordinate units baseline is unchanged", {
  skip_if(Sys.getenv("DARTR_EEMS_REVIEW") != "true")
  skip_if_not_installed("reemsplots2")
  skip_if_not_installed("sf")
  binary_dir <- normalizePath(Sys.getenv("DARTR_EEMS_BIN", "~/programs"),
                              mustWork = TRUE)
  skip_if_not(file.exists(file.path(binary_dir, "runeems_snps")))
  local_edition(3)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  # Two states 1,000+ km apart, so at least two observed demes exist.
  x <- bandicoot.gl[pop(bandicoot.gl) %in% c("WA", "NSW"), 1:200]
  out <- tempfile("eems-units-baseline-")
  dir.create(out)
  result <- gl.run.eems(x, eems.path = binary_dir, out.dir = out,
                       nDemes = 30, numMCMCIter = 200, numBurnIter = 100,
                       numThinIter = 9, seed = 17, dpi = 20,
                       cleanup = FALSE, verbose = 0)
  run <- Sys.glob(file.path(out, "eems-run-*"))
  coord <- as.matrix(utils::read.table(file.path(run, "eems.coord")))
  outer <- as.matrix(utils::read.table(file.path(run, "eems.outer")))
  params <- readLines(file.path(run, "params.ini"))
  panel <- ggplot2::ggplot_build(result$mrates01)$layout$panel_params[[1]]
  summary <- list(
    coord_first_rows = signif(coord[1:2, ], 6),
    outer_x_range = signif(range(outer[, 1]), 4),
    outer_y_range = signif(range(outer[, 2]), 4),
    distance_line = grep("^distance", params, value = TRUE),
    rdist03_km_range = round(range(result$rdist03$data$fitted)),
    mrates01_x_range = signif(panel$x.range, 4)
  )
  expect_snapshot(print(summary))
})
