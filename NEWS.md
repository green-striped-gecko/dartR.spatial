# dartR.spatial (development)

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
