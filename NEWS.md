# dartR.spatial (development)

* `gl.kosman()` computes distances with matrix cross-products instead of one
  matrix per locus, so large datasets no longer exhaust memory (300
  individuals x 20,000 loci: 2.8 s, 422 MB). Distances are unchanged. The
  diagonal of `nloci` now holds the loci called in each individual instead
  of the total number of loci. Mixed or invalid ploidy stops with a clear
  error, and pairs without shared loci trigger a warning.
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
