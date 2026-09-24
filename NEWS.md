# dartR.spatial 1.2.3

* `gl.grm2()` passes `min.MAF = 1/(2n) - 1e-10` to `rrBLUP::A.mat()`
  unless `min.MAF` is given. With the default `1/(2n)`, a locus with a single
  minor-allele copy sat exactly on the cut-off and was kept on Linux and
  Windows but dropped on Apple Silicon Macs, because `mean()` rounds
  differently there. Results on Apple Silicon change (on
  `platypus.gl[1:12, 1:200]` 5 of 105 loci were dropped and the T27 diagonal
  goes from 0.943 to 0.986); Linux and Windows results are unchanged.
* `gl.propShared()` calculates its similarity with
  `dartR.base::gl.dist.ind(method = "manhattan")` instead of compiling its own
  C++ code in every R session, so it no longer needs Rcpp or a compiler. SNP
  values are unchanged (max difference 2.2e-16); pairs with no locus called in
  both return `NA` instead of `NaN`. It gains a `verbose` argument, and
  SilicoDArT input now stops with an error: before, it returned a similarity
  squeezed into 0.5-1. A missing Rcpp no longer returns `-1`.
* `gl.ibd()` calculates geodesic distances (package terra) for longitude/
  latitude instead of Euclidean distances on Mercator coordinates, which
  are inflated by 1/cos(latitude). For `testset.gl` the geographic
  distances are about 15% shorter and the Mantel statistic changes
  (0.2658 to 0.2638). Out-of-range degrees now stop with an error.
* `gl.grm2()` is synchronised with the reviewed `dartR.captive::gl.grm()`.
  SilicoDArT input now stops with an error, `plot.file` with
  `plotheatmap = FALSE` warns instead of failing, `palette_discrete` is used,
  `label.size` and `legend.title` are added, and gplots is required only
  for the heatmap. SNP matrices are unchanged.
* `gl.spatial.autoCorr()` uses genetic distances as distances: `Simple` and
  `Absolute` (and SilicoDArT `Simple`) were reversed, which flipped the sign
  of r, and `grm` is now converted to a distance instead of giving values
  outside [-1, 1]. Euclidean distances are squared, as in Smouse & Peakall
  (1999) and GenAlEx, so default r values roughly double. Lon/lat distances
  are geodesic instead of Mercator (about 13% shorter at 29 degrees), and
  one-tail p-values count the observed value, so they are never 0.
  `Dgeo`/`Dgen` input works without `x = NULL` and with named lists or lists
  of matrices, and a `coordinates` data.frame works with several
  populations.
* `utils.spautocor()` starts automatic distance classes at the minimum
  distance, as documented; before, classes started at 0 and could leave all
  pairs in the last class.
* `gl.genleastcost()` accepts a file path, RasterLayer, RasterStack,
  RasterBrick or SpatRaster and processes every layer. Raster objects had
  failed on every call since December 2024. Cost distances now come from
  `gl.costdistances()`, so they change (mean-resistance conductance, random-
  walk correction for commute) and `theta` is used; `rSPDistance` with the
  default `theta = 1` may now stop with an underflow message. `propShared`
  is returned as a distance (1 - proportion shared) instead of a
  similarity, and `kosman` no longer errors. With `plotpath = FALSE`
  nothing is plotted. Invalid arguments, missing coordinates and
  SilicoDArT data stop early with clear messages.
* `gl.kosman()` computes distances with matrix cross-products instead of one
  matrix per locus, so large datasets no longer exhaust memory (300
  individuals x 20,000 loci: 2.8 s, 422 MB). Distances are unchanged. The
  diagonal of `nloci` now holds the loci called in each individual instead
  of the total number of loci. Mixed or invalid ploidy stops with a clear
  error, and pairs without shared loci trigger a warning.
* `gl2shp()` removes individuals only when their coordinates are missing.
  Previously an NA in any `ind.metrics` column also removed the individual,
  so outputs can now contain more points. The attribute table is written
  once: duplicated `.1` columns and the `optional` column are gone, and `id`
  holds sample names instead of row numbers. Invalid `type`, a missing
  `outpath`, missing terra and data with no complete coordinates now error.
* `gl.costdistances()` now uses mean cell resistance for symmetric edges.
  This changes distances on heterogeneous landscapes. Genlight coordinates
  are selected by lon/lat names, interpreted as WGS84, and their population
  centres are transformed to the raster CRS. Geographic commute distances
  now use random-walk correction, and a sparse solver supports locations in
  the grounded cell while retaining commute-time scaling.
* `gl.costdistances()` validates locations, resistance, population assignments
  and calculation settings. Invalid locations are no longer omitted or
  recycled into misleading matrices. NA/Inf cells are barriers; zero/negative
  resistance is rejected. Explicit x/y on unreferenced rasters still uses
  local grid units. Disconnected least-cost pairs retain Inf; disconnected
  commute/RSP analyses error. Optional `theta = 1` is appended after `verbose`;
  it exposes the RSP parameter without changing its default. Non-finite RSP
  results now error with scale/theta guidance. Dependency errors, verbosity,
  function-object invocation and method/units documentation are corrected.

* `gl.ibd()` aligns labelled distances and explicitly named coordinate tables.
  Misaligned inputs can therefore produce different results; mismatched or
  duplicate identities now error. Stored coordinates retain individual order.
* `gl.ibd()` rejects incomplete coordinates, missing/non-finite pairwise
  distances, fewer than three observations and constant distance vectors.
  Square matrices and matrix-valued transformations are handled consistently.
  Supplying both distance matrices ignores `x`; partial inputs calculate only
  the missing distance. Negative Fst estimates remain supported.
* `gl.ibd()` constructs plots only for display or saving, reports projection
  dependency failures as errors, honours verbosity and accepts function-object
  `do.call()` invocations. Documentation now describes the unchanged identity
  transformation defaults and `stats::dist()` scaling for missing loci.

* `gl.run.eems()` now errors on failed or incomplete EEMS runs instead of
  reading earlier output. Each call writes to a unique `eems-run-*` directory
  under `out.dir`; raw results and logs survive cleanup, which removes only
  that run's intermediate files after success. Relative output paths use the
  caller's directory, and paths containing spaces are supported.
* `gl.run.eems()` checks `diploid` against input ploidy and raises errors for
  missing dependencies. Extra plotting arguments are now honoured and
  validated. `plot.file = NULL` disables RDS saving; named saves remain
  available. Routine output follows verbosity, with plots displayed at level
  3 or above and returned at every level. The `dpi` documentation now
  distinguishes contour-grid sampling from exported-image resolution.
