# Review: gl.genleastcost (dartR.spatial)

## 1. Header

- Family mode: analysis.
- Date: 2026-09-23.
- Reviewer: Claude (claude-opus-5-5), dartr-function-review v2.0.0.
- Package commit: `f2cfb52` (`origin/dev`, after PRs #38–#40 merged). Work is on branch `review-gl.genleastcost` in worktree `../dartR.spatial-glc`.
- Datasets: `possums.gl`, populations A–C, 10 individuals each, loci 1–100, with `@other$xy`; `landscape.sim` from dartR.data aggregated by a factor of 5 (50 x 50 cells); `testset.gs` for the SilicoDArT probe; a synthetic lon/lat raster for P14.
- Baseline: `tests/testthat/test-gl.genleastcost.R`, captured before critical source review. There are 12 snapshots. Three pass a `RasterLayer` (all error), and nine pass a `terra` `SpatRaster` covering all five genetic distances, three path types and `plotpath` on/off.
- Probes: `function-review/evidence/gl.genleastcost-probes.R`, output in `function-review/evidence/gl.genleastcost-probes.log`.
- Runtime: R 4.4 (`/usr/local/bin/Rscript`) on macOS arm64.

## 2. Verdict

**Standards: Needs work** — dependency guards return `-1`, argument checks are missing, plotting cannot be switched off, and every example is commented out.

**Spec: Rework** — the documented input type (a raster or raster stack) has failed on every call since 2024-12-30. The cost distances do not match the corrected `gl.costdistances`, `theta` is ignored, `propShared` is returned as a similarity, and `kosman` always errors.

Population centres, Euclidean distances and the alignment of genetic-distance labels are correct: reordering individuals and population levels gives the same A–B `Gst.Nei` (0.1572, P11). A file path and a single-layer `SpatRaster` work.

## 3. Findings

**F1 [BLOCKER, confidence: high] — `RasterLayer` and `RasterStack` inputs always fail (FS6; documented input rejected)**
`R/gl.genleastcost.r:193` — `raster::raster(fric.raster)` returns an empty template with the same grid but no values when given a `Raster*` object. The line was added in `0d62ddb` (2024-12-30) so that file paths could be read, which the Shiny app relies on.
Failure scenario (P1, baseline): `gl.genleastcost(x, landscape.sim)` stops with "no values associated with this RasterLayer" for every distance and path type. The documented example is commented out, so R CMD check never caught it.
Proposed change: read `fric.raster` as a stack. Accept a file path, `RasterLayer`, `RasterStack`/`RasterBrick` or `SpatRaster`, convert with `raster::stack()`, and error clearly for anything else.

**F2 [HIGH, confidence: high] — cost distances disagree with `gl.costdistances`; `theta` ignored (API1 (proposed rule); FS6)**
`R/gl.genleastcost.r:229–247` — conductance is `1 / x[2]` (the resistance of one cell of each pair) instead of the mean of both cells. Commute distance uses geographic correction type `"c"` instead of `"r"`, and `rSPDistance` is called with `theta = 1` hard-coded.
Failure scenario (P4, P7): on the same landscape and population centres, least-cost distances differ from `gl.costdistances` by up to 52% (A–B 570.7 vs 670.9) and commute by up to 10%. `theta = 0.001` and `theta = 1` give identical output, and `gl.costdistances` gives 1456.4 at `theta = 0.001` against 573.2 here. PR #38 fixed exactly these issues in `gl.costdistances`.
Proposed change: compute each layer's cost matrix with `gl.costdistances(layer, cp, pathtype, NN, theta)`, and build least-cost paths from the same transition (mean conductance, type `"c"` correction).

**F3 [HIGH, confidence: high] — `propShared` returns a similarity as a distance (FS6)**
`R/gl.genleastcost.r:311` — `propShared(xx)` resolves to `adegenet::propShared()`, a similarity with 1 on the diagonal. `as.dist()` then drops the diagonal, so `gen.mat` has 0 on the diagonal and similarity everywhere else.
Failure scenario (P3): the mean within-population value is 0.728 and the A–C mean is 0.576, so individuals from the same population look more distant. A Mantel test or MMRR on `gen.mat` against cost distance gets the sign reversed.
Proposed change: use `1 - gl.propShared(x)`, as `gl.ibd` does.

**F4 [HIGH, confidence: high] — `kosman` always errors (FS6)**
`R/gl.genleastcost.r:307` — `gl.kosman()` is called on the genind conversion `xx`. `gl.kosman()` accepts only genlight objects, before and after PR #40.
Failure scenario (P2): `gen.distance = "kosman"` stops with "x must be a genlight or dartR object".
Proposed change: call `gl.kosman(x, verbose = 0)` on the genlight.

**F5 [MEDIUM, confidence: high] — only the first layer of a multi-layer raster is used (DOC5 (proposed rule))**
`R/gl.genleastcost.r:193` — `raster::raster()` keeps only layer 1. The description promises that each layer of a stack is processed.
Failure scenario (P9): a two-layer `SpatRaster` returns one cost matrix (`base`) and drops `double` silently. A `RasterStack` errors (F1).
Proposed change: covered by the stack conversion in change 1.

**F6 [MEDIUM, confidence: high] — raster map always plotted; plotting cannot be switched off (PLT3; PLT1)**
`R/gl.genleastcost.r:205–226` — the raster and points are drawn on every call. `plotpath` only controls the path lines.
Failure scenario (P10): `plotpath = FALSE` still writes an 8.5 KB plot. In batch or Shiny runs over many layers, one plot is drawn per layer.
Proposed change: draw the map, points and paths only when `plotpath = TRUE`.

**F7 [MEDIUM, confidence: high] — all examples commented out (DOC3)**
`R/gl.genleastcost.r:66–93`.
Failure scenario: R CMD check runs nothing, which is how F1, F3 and F4 went unnoticed.
Proposed change: add a runnable `\donttest{}` example on three possum populations with an aggregated landscape (about 1 s).

**F8 [LOW, confidence: high] — argument and data checks missing or broken (FS5; DAT5)**
`R/gl.genleastcost.r:126–163`.
Failure scenario (P5, P6, P12): an invalid `pathtype` gives "object 'cd.mat' not found". Missing coordinates give "$ operator is invalid for atomic vectors", and the intended guard calls `stats::step()` instead of `stop()`. The `gen.distance` error points to a non-existent `?landgenreport`. SilicoDArT fails deep inside `gl2gi()`.
Proposed change: validate `gen.distance`, `pathtype` and coordinates up front with `stop(error())`; check for SNP data with `utils.check.datatype()`; remove the `stats::step` import.

**F9 [LOW, confidence: high] — dependency guards return `-1` (DEP1)**
`R/gl.genleastcost.r:106–124`.
Failure scenario: without gdistance or mmod, the call returns `-1` and later code fails on it. mmod is required even for individual distances, which do not use it.
Proposed change: standard `stop(error())` guards; require mmod only for `D`, `Gst.Nei` and `Gst.Hedrick`.

**F10 [LOW, confidence: high] — messages ignore `verbose` (VRB1; FS3)**
Default `plot.colors.pop = gl.colors("dis")` prints "Starting gl.colors … Completed: gl.colors" even at `verbose = 0` (P13); `build = "Jody"` is outdated.
Proposed change: call `gl.colors("dis", verbose = 0)` in the default; remove `build`.

**F11 [LOW, confidence: high] — roxygen inaccurate or incomplete (DOC1; DOC2; DOC5 (proposed rule); DOC7 (proposed rule))**
`R/gl.genleastcost.r:1–65` — `@return` says four matrices, but the list has six elements. The `plotpath` default is undocumented, the `"dist"` option is undocumented, and `theta` is documented but ignored (F2). Also missing: accepted raster types, the use of `@other$xy` before `@other$latlon`, the CRS requirement, `@family`, a custodian and the standard `verbose` text.
Proposed change: rewrite the header in house order. Docs only.

## 4. Proposed changes

1. Read `fric.raster` as a stack from a path, `Raster*` or `SpatRaster`, processing every layer (F1, F5). **Consequence: `RasterLayer`/`RasterStack` calls now run instead of erroring; multi-layer `SpatRaster` calls return one cost matrix per layer instead of one.**
2. Compute cost matrices with `gl.costdistances()` and paths from the same transition (F2). **Consequence: `cost.mats` change for every call (least-cost up to 52% and commute up to 10% on `landscape.sim`); `theta` now changes `rSPDistance` results; `rSPDistance` with the default `theta = 1` now errors with a theta hint wherever `gl.costdistances` reports underflow (it does on `landscape.sim`).**
3. Use `1 - gl.propShared(x)` for `propShared` (F3). **Consequence: `gen.mat` values for `propShared` change from similarity to distance.**
4. Call `gl.kosman()` on the genlight (F4). `gen.distance = "kosman"` now returns results instead of erroring.
5. Plot only when `plotpath = TRUE` (F6). **Consequence: calls with `plotpath = FALSE` no longer draw the raster map.**
6. Runnable `\donttest{}` example (F7).
7. Input validation and SNP datatype check (F8).
8. Standard dependency guards; mmod only for population distances (F9).
9. Silent default colours; remove `build` (F10).
10. Roxygen rewrite (F11). Docs only.

## 5. Coverage

- Standards walk: FS, DOC, VRB, DAT, DEP, PLT, STY, API — run.
- Spec: every `gen.distance` x `pathtype` combination on `SpatRaster` input, `RasterLayer`, `RasterStack`, multi-layer `SpatRaster` and file-path input — run.
- Cost distances compared with the corrected `gl.costdistances` — run (least-cost, commute; `rSPDistance` compared at `theta = 0.001` because `theta = 1` underflows there).
- Genetic-distance label alignment under reordered individuals and levels — run; correct.
- Lon/lat raster with the `@other$latlon` fallback (P14) — run; equals `gl.costdistances`, so the `+proj=merc` override on line 234 has no effect on the cost values tested.
- Path lengths (`pathlength.mats`) on lon/lat rasters: not checked; `SpatialLinesLengths()` may return degrees.
- Callers: no sibling `dartR.*` package calls it. `dartr_shiny` (`Fun_gl.genleastcost.R`) passes a file path plus named arguments, including `theta`. Change 1 keeps file paths working; change 2 makes its `theta` input take effect.
- dartR Google Group / GitHub issues: SKIPPED — not searched in this session.

## 6. Approval

| Change | Decision | By | Note |
|---|---|---|---|
| 1 | approved | Luis | |
| 2 | approved | Luis | |
| 3 | approved | Luis | |
| 4 | approved | Luis | |
| 5 | approved | Luis | |
| 6 | approved | Luis | |
| 7 | approved | Luis | |
| 8 | approved | Luis | |
| 9 | approved | Luis | |
| 10 | approved | Luis | |

Changes 1, 2, 3 and 5 approved with their stated consequences.

## 7. Outcome

All ten approved changes are implemented in `R/gl.genleastcost.r`. `devtools::document()` regenerated `man/gl.genleastcost.Rd`, added the shared family link to `man/gl.costdistances.Rd`, and removed `importFrom(stats, step)` from `NAMESPACE`.

- Characterisation test (`test-gl.genleastcost.R`): all 12 snapshots changed, and each diff maps to an approved change. The `RasterLayer` inputs now run (change 1). `cost.mats` and path lengths now equal `gl.costdistances` (change 2): least-cost A–B went from 570.68 to 670.87 and commute from 23205.96 to 21028.09, and `rSPDistance` at `theta = 1` stops with the underflow message. `propShared` is now 1 − similarity (change 3), and `kosman` runs (change 4). `gen.mat` for `D`, `Gst.Nei`, `Gst.Hedrick` and `dist` and all `eucl.mat` values are unchanged. Snapshots updated; 24/24 pass.
- Fix tests (`test-gl.genleastcost-fixes.R`): 23/23 pass. They cover every input type, including a file path and two-layer stacks, exact agreement with `gl.costdistances` for all three path types, `theta` affecting output, `propShared` direction, `kosman`, plotting gated by `plotpath`, validation errors and silent defaults.
- Probes re-run (`function-review/evidence/gl.genleastcost-after.log`): P7 relative difference to `gl.costdistances` is 0.0000 for least-cost and commute. At `theta = 0.001` `rSPDistance` gives 1456.433 in both functions. A two-layer `SpatRaster` returns two cost matrices. `plotpath = FALSE` writes an empty PDF (3611 bytes, previously 8575).
- The roxygen example runs in 1.2 s, including `PopGenReport::wassermann()`.
- Full package test suite: 404 passed, 2 skipped (opt-in EEMS tests), 0 failed.
- Not changed (outside the approved scope): the "No projected coordinates in @other$xy" warning still prints regardless of `verbose`.

## 8. Machine block

```json
{
  "function": "gl.genleastcost",
  "package": "dartR.spatial",
  "family": "analysis",
  "skill_version": "2.0.0",
  "commit": "f2cfb52",
  "verdict_standards": "needs_work",
  "verdict_spec": "rework",
  "findings": [
    {"id": "F1", "severity": "BLOCKER", "confidence": "high", "rule": "FS6", "status": "approved", "change": 1},
    {"id": "F2", "severity": "HIGH", "confidence": "high", "rule": "API1", "status": "approved", "change": 2},
    {"id": "F3", "severity": "HIGH", "confidence": "high", "rule": "FS6", "status": "approved", "change": 3},
    {"id": "F4", "severity": "HIGH", "confidence": "high", "rule": "FS6", "status": "approved", "change": 4},
    {"id": "F5", "severity": "MEDIUM", "confidence": "high", "rule": "DOC5", "status": "approved", "change": 1},
    {"id": "F6", "severity": "MEDIUM", "confidence": "high", "rule": "PLT3", "status": "approved", "change": 5},
    {"id": "F7", "severity": "MEDIUM", "confidence": "high", "rule": "DOC3", "status": "approved", "change": 6},
    {"id": "F8", "severity": "LOW", "confidence": "high", "rule": "FS5", "status": "approved", "change": 7},
    {"id": "F9", "severity": "LOW", "confidence": "high", "rule": "DEP1", "status": "approved", "change": 8},
    {"id": "F10", "severity": "LOW", "confidence": "high", "rule": "VRB1", "status": "approved", "change": 9},
    {"id": "F11", "severity": "LOW", "confidence": "high", "rule": "DOC1", "status": "approved", "change": 10}
  ],
  "coverage_skipped": ["pathlength units on lon/lat rasters: not checked", "Google Group / GitHub issues: not searched"],
  "status": "pr-open",
  "pr": 41
}
```
