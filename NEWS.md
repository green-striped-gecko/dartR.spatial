# dartR.spatial (development)

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
