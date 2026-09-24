# Approved sync with dartR.captive::gl.grm
# (function-review/reports/dartR.spatial/gl.grm2.md)
x <- dartR.data::platypus.gl[1:12, 1:200]

test_that("SNP matrix equals rrBLUP::A.mat with a tolerant min.MAF", {
  G <- gl.grm2(x, plotheatmap = FALSE, verbose = 0)
  expect_equal(G, rrBLUP::A.mat(as.matrix(x) - 1,
                                min.MAF = 1 / (2 * nInd(x)) - 1e-10))
  # a user-supplied min.MAF is passed through unchanged
  G2 <- gl.grm2(x, plotheatmap = FALSE, verbose = 0, min.MAF = 0.1)
  expect_equal(G2, rrBLUP::A.mat(as.matrix(x) - 1, min.MAF = 0.1))
})

test_that("single-copy loci are kept, so the matrix is platform independent", {
  # value from x86 Linux/Windows CI; arm64 macOS gave 0.943133 before the
  # tolerance because mean() put some single-copy loci below 1/(2n)
  G <- gl.grm2(x, plotheatmap = FALSE, verbose = 0)
  expect_equal(round(unname(G["T27", c("T27", "T35", "SDS4", "SDS12")]), 6),
               c(0.985509, -0.173415, -0.098468, -0.090907))
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
