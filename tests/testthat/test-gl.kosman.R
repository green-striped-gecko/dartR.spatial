# Characterisation captured before critical source review at 0bbc487.
# Snapshots record current behaviour, defects included.
# Approved change 5 snapshot diff: nloci diagonal now counts the loci called
# in each individual; all kosman distances unchanged.
test_that("gl.kosman reference-data behaviour is unchanged", {
  local_edition(3)
  cases <- list(
    "testset.gl" = dartR.data::testset.gl[1:8, 1:40],
    "testset.gs" = dartR.data::testset.gs[1:8, 1:40],
    "possums.gl" = dartR.data::possums.gl[1:5, 14:21]
  )
  for (nm in names(cases)) {
    x <- cases[[nm]]
    before <- serialize(x, NULL)
    result <- tryCatch(gl.kosman(x, verbose = 0), error = identity)
    expect_identical(serialize(x, NULL), before)
    summary <- list(dataset = nm, dims = c(nInd(x), nLoc(x)),
                    ploidy = unique(ploidy(x)), result_class = class(result))
    if (inherits(result, "error")) {
      summary$error <- conditionMessage(result)
    } else {
      summary$names <- names(result)
      summary$kosman <- round(result$kosman, 6)
      summary$nloci <- result$nloci
    }
    expect_snapshot(print(summary), variant = paste0("kosman-", nm))
  }
})
