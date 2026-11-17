# File-backed objects: gl.kosman and gl.ibd read the genotypes in blocks of
# loci (dartR.base::utils.fbm.pair.counts, utils.fbm.colwise, gl.fst.pop)
# instead of converting the object to a genlight. Results and messages must
# be identical to those of the gen-backed object for any block size, and no
# call may decode the whole FBM.

fbm_pair <- function() {
  x0 <- bandicoot.gl
  # three individuals of every population (Fst and D compare populations)
  keep <- unlist(lapply(split(seq_len(nInd(x0)), pop(x0)), head, 3),
                 use.names = FALSE)
  x <- gl.compliance.check(x0[keep, 1:150], verbose = 0)
  m <- as.matrix(x)
  m[, 5] <- NA                               # an all-NA locus
  m[2, 20:40] <- NA                          # a poorly scored individual
  x@gen <- new("genlight", m, ploidy = ploidy(x))@gen
  x <- gl.recalc.metrics(x, verbose = 0)
  list(x = x, f = suppressWarnings(gl.gen2fbm(x, verbose = 0)))
}

ibd <- function(distance) {
  function(x) gl.ibd(x, distance = distance, permutations = 19,
                     plot.out = FALSE, verbose = 3)
}

calls <- list(
  kosman = function(x) gl.kosman(x, verbose = 3),
  ibd.fst = ibd("Fst"),
  ibd.d = ibd("D"),
  ibd.euclidean = ibd("euclidean"),
  ibd.kosman = ibd("kosman"),
  ibd.propshared = ibd("propShared")
)

run <- function(FUN, x) {
  set.seed(1)
  out <- utils::capture.output(val <- suppressWarnings(FUN(x)))
  if (is.list(val) && !is.data.frame(val)) {
    val[vapply(val, inherits, logical(1), "ggplot")] <- NULL
  }
  list(val = val, out = out)
}

test_that("gen- and file-backed objects give identical results", {
  skip_if_not_installed("bigstatsr")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  old <- options(bigstatsr.block.sizeGB = getOption("bigstatsr.block.sizeGB"))
  on.exit(options(old), add = TRUE)
  xs <- fbm_pair()
  for (bs in c(1, 1e-6)) {                   # 1e-6: one locus per block
    options(bigstatsr.block.sizeGB = bs)
    for (nm in names(calls)) {
      g <- run(calls[[nm]], xs$x)
      f <- run(calls[[nm]], xs$f)
      expect_identical(f$val, g$val, label = paste(nm, bs))
      expect_identical(f$out, g$out, label = paste(nm, bs, "output"))
    }
  }
})

test_that("file-backed objects are never decoded as a whole", {
  skip_if_not_installed("bigstatsr")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  xs <- fbm_pair()
  local_mocked_bindings(.fbm_as_matrix = function(x) {
    stop("the whole FBM was decoded")
  }, .package = "dartR.base")
  for (nm in names(calls)) {
    set.seed(1)
    expect_no_error(utils::capture.output(suppressWarnings(
      calls[[nm]](xs$f))), message = nm)
  }
})
