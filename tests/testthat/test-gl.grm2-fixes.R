# Approved sync with dartR.captive::gl.grm
# (function-review/reports/dartR.spatial/gl.grm2.md)
x <- dartR.data::platypus.gl[1:12, 1:200]

test_that("SNP matrix equals rrBLUP::A.mat", {
  G <- gl.grm2(x, plotheatmap = FALSE, verbose = 0)
  expect_equal(G, rrBLUP::A.mat(as.matrix(x) - 1))
})

test_that("SilicoDArT input stops with a clear error", {
  expect_error(gl.grm2(dartR.data::testset.gs[1:12, 1:100], verbose = 0),
               "not valid for SilicoDArT")
})

test_that("plot.file without a heatmap warns instead of failing", {
  withr::local_dir(withr::local_tempdir())
  expect_output(G <- gl.grm2(x, plotheatmap = FALSE, plot.file = "g",
                             verbose = 1), "nothing was saved")
  expect_equal(dim(G), c(12, 12))
})

test_that("palettes and legend options are used", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(gl.grm2(x, palette_discrete = c("red", "blue", "green"),
                        palette_convergent = grDevices::heat.colors,
                        label.size = 0.5, legend.title = "Rivers",
                        verbose = 0))
  expect_silent(gl.grm2(x, palette_convergent = grDevices::heat.colors(10),
                        verbose = 0))
})
