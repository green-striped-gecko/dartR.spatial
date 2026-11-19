# Review: gl2shp (dartR.spatial)

## 1. Header

- Family mode: io.
- Date: 2026-09-23.
- Reviewer: Claude (claude-opus-5-5), dartr-function-review v2.0.0.
- Package commit: `0bbc487947500b18a6adc2863e9c80e3499f72e9` (`dev_luis`, equal to `origin/dev`). `R/gl2shp.r` had no local changes. Unrelated uncommitted `gl.costdistances` work in the tree was left untouched.
- Datasets: `testset.gl[1:20, 1:30]`, `testset.gs[1:20, 1:30]`, plus modified copies of `testset.gl` for probes.
- Baseline: `tests/testthat/test-gl2shp.R`, captured before critical source review. Four snapshots: SNP/SilicoDArT x `shp`/`kml`, recording the returned `SpatVector`, files written and the attribute table read back from disk.
- Probes: `function-review/evidence/gl2shp-probes.R`, output in `function-review/evidence/gl2shp-probes.log`.
- Runtime: R 4.4 (`/usr/local/bin/Rscript`) on macOS arm64; terra, sp as installed.

## 2. Verdict

**Standards: Needs work** — structure follows the house template, but the dependency guard, input checks and output-path check are missing or non-standard, and the roxygen header is incomplete.

**Spec: Needs work** — individuals with complete coordinates are dropped whenever any `ind.metrics` column is `NA`, while the message reports zero removals; the attribute table duplicates every column and replaces the individual `id` with a row number.

On clean input the coordinates are correct: point geometry matches `latlon` (lon = x, lat = y) and the CRS is recorded.

## 3. Findings

**F1 [HIGH, confidence: high] — individuals dropped for NA in non-coordinate metrics (DOC5 (proposed rule); FS6)**
`R/gl2shp.r:106–107` — `complete.cases(glpoints)` runs on the full `ind.metrics` table, not on the coordinates. The count reported at line 95 uses `complete.cases(x@other$latlon)` instead, so the two disagree.
Failure scenario (P1): setting `sex` to `NA` for 5 of 20 individuals with complete coordinates returns 15 points and prints "Removed 0 individual(s) due to missing coordinates". DArT `ind.metrics` routinely contain `NA` in optional columns, so real datasets lose samples silently.
Proposed change: filter on coordinates only; keep `NA` attribute values in the output.

**F2 [MEDIUM, confidence: high] — duplicated attribute table; individual `id` replaced by row number (DOC5 (proposed rule); FS6)**
`R/gl2shp.r:109–118` — `glpoints` already holds `ind.metrics` plus lat/lon; line 118 binds `ind.metrics` again. Line 109 overwrites the `id` column with `1:n`.
Failure scenario (P8, P10, P11): the file holds 13 columns for 6 metrics (`id`, `pop`, ..., `id.1`, `pop.1`, ...). `id` is `1, 2, 3`; the sample names survive only as `id.1`. In a shapefile, names over 10 characters collide after truncation (`a_very_lo0`, `a_very_lo1`), so the user cannot tell which column is which.
Proposed change: build the attribute table once (`ind.metrics` + `lat` + `lon`), keep `id` as the individual name (add `id = indNames(x)` when absent), and create the vector directly with `terra::vect(df, geom = c("lon", "lat"), crs = proj4)`. This removes the `sp` round trip from this function (`sp` stays in Imports for `gl.genleastcost`).

**F3 [MEDIUM, confidence: high] — invalid `type` writes nothing but reports success (FS5)**
`R/gl2shp.r:119–133` — `type` is not validated.
Failure scenario (P4): `type = "gpkg"` or `type = "KML"` writes no file and prints "Shapefile saved as: gl.gpkg". The message also says "Shapefile" for KML output.
Proposed change: validate `type` against `c("shp", "kml")` with `stop(error())`; name the format correctly in the message.

**F4 [MEDIUM, confidence: high] — opaque errors on non-dartR input shapes (DAT5)**
`R/gl2shp.r:88–90, 103–104`.
Failure scenario (P3, P5): `ind.metrics = NULL` fails with "incorrect number of dimensions"; `latlon` stored as a matrix fails with "$ operator is invalid for atomic vectors". Both are legitimate genlight objects.
Proposed change: coerce `latlon` to a data frame, require `lat` and `lon` (or `long`) columns with a clear error, and work when `ind.metrics` is `NULL`.

**F5 [LOW, confidence: high] — dependency guard returns -1 instead of stopping (DEP1)**
`R/gl2shp.r:54–60`.
Failure scenario: without terra, `out <- gl2shp(x)` prints a message and assigns `-1`; downstream code fails later with an unrelated error.
Proposed change: use the standard `stop(error(...))` guard and drop the `else` wrapper around the body.

**F6 [LOW, confidence: high] — output folder not checked (FS7)**
Failure scenario (P7): a non-existent `outpath` fails inside GDAL with "[writeVector] Layer creation failed".
Proposed change: check `dir.exists(outpath)` up front and stop with a message naming the folder.

**F7 [LOW, confidence: high] — removal message is noise when nothing is removed and hidden when it matters (VRB3; VRB4 (proposed rule))**
`R/gl2shp.r:92–105` — "Removed 0 individual(s)" prints as a warning on every run at `verbose >= 2`; removed names print one `cat()` per name with a stray leading space.
Failure scenario: a user at `verbose = 1` loses individuals with no message.
Proposed change: warn only when individuals are removed, at `verbose >= 1`, with names on one line.

**F8 [LOW, confidence: high] — outdated start flag and dead code (FS3; STY1)**
`R/gl2shp.r:37, 44, 116` — `build = "Jody"` is an outdated argument; `outfilespec` is never used; a commented-out `project()` line remains.
Failure scenario: none at run time; misleads the next maintainer.
Proposed change: remove all three.

**F9 [LOW, confidence: high] — roxygen incomplete or inaccurate (DOC1; DOC2; DOC7 (proposed rule))**
`R/gl2shp.r:1–27` — no `@family`; `verbose` text is non-standard; `@author` misspells "Bernd Guber" and lacks a Custodian; `@return` says "SpatVector file"; description grammar ("if not proj4 string is provided"); the docs do not say that individuals without coordinates are dropped or which columns are written.
Proposed change: rewrite the header to the house order and state the drop rule and output columns. Docs only.

## 4. Proposed changes

1. Drop individuals only when coordinates are missing (F1). **Consequence: the number of points returned and written increases for any dataset with `NA` in a non-coordinate `ind.metrics` column.**
2. Build the attribute table once with `id` = individual name, via `terra::vect()` without `sp` (F2). **Consequence: the output file and returned `SpatVector` lose the duplicated `.1` columns, and `id` holds sample names instead of row numbers.**
3. Validate `type` and name the format correctly in the message (F3). Invalid values now error instead of silently writing nothing.
4. Accept matrix `latlon`, `long` naming and `NULL` `ind.metrics`, with clear errors otherwise (F4).
5. Standard terra dependency guard (F5). A missing terra now errors instead of returning `-1`.
6. Check that `outpath` exists (F6).
7. Removal warning only when individuals are removed, at `verbose >= 1` (F7).
8. Remove `build = "Jody"`, `outfilespec` and the commented-out line (F8).
9. Roxygen rewrite (F9). Docs only.

## 5. Coverage

- Standards walk: FS, DOC, VRB, DAT, DEP, PLT, STY, API — run. PLT not applicable (no plots).
- Spec: behaviour vs roxygen on `testset.gl` / `testset.gs`, both output types — run.
- Round trip: files re-read with `terra::vect()` and compared to the returned object — run.
- KML with a projected `proj4`: runs and writes; whether GDAL reprojects to WGS84 correctly was not verified against known coordinates.
- Callers: no calls in sibling `dartR.*` packages. `dartr_shiny` (`src/app/view/Fun_gl2shp.R`) calls it with named `type`, `proj4`, `outpath`, `outfile` and only zips the files, so no proposed change breaks it.
- dartR Google Group / GitHub issues: SKIPPED — not searched in this session.
- FBM path (DAT6): not applicable — genotypes are never read.

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

Changes 1 and 2 approved with their stated output consequences.

## 7. Outcome

All nine approved changes are implemented in `R/gl2shp.r`; `devtools::document()` regenerated `man/gl2shp.Rd` and removed five `sp` imports from `NAMESPACE` (the four used by `gl.genleastcost` remain).

- Characterisation test (`test-gl2shp.R`): the only diffs are the attribute names in all four snapshots. The `.1` duplicates and the `optional` column are gone (`optional` was a constant `TRUE` column created by the `sp` round trip). Point counts, coordinates and CRS are unchanged. All diffs map to change 2; snapshots updated. 8/8 pass.
- Fix tests (`test-gl2shp-fixes.R`): 20/20 pass, covering changes 1–4, 6, 7, A1 and A2.
- Probes re-run (`function-review/evidence/gl2shp-after.log`): P1 now returns 20 points (was 15); P2 warns with both removed names on one line; P3 and P5 succeed; P4 and P7 stop with clear messages; the file has 6 columns with `id` = sample names. `gl2shp(testset.gl, type = "kml", verbose = 3)` runs end to end (250 points). The roxygen example runs.
- Not run: full `devtools::check()`.
- Change 5 (terra guard) is not exercised by a test, because terra cannot be unloaded in-session.

### Addendum findings (found during apply; approved by Luis and applied)

**A1 [LOW, confidence: high] — all coordinates missing gives an empty output (FS5)**
If every individual lacks coordinates, the function warns, writes an empty file and returns an empty `SpatVector`.
Proposed change: stop with "No individuals with complete coordinates".

**A2 [LOW, confidence: high] — lat/long rename warning ignores `verbose` (VRB3)**
The "Names given as lat long ... Rectifying" warning prints even at `verbose = 0`, as it did before the review.
Proposed change: gate at `verbose >= 2`.

A1 and A2: approved by Luis, applied.

## 8. Machine block

```json
{
  "function": "gl2shp",
  "package": "dartR.spatial",
  "family": "io",
  "skill_version": "2.0.0",
  "commit": "0bbc487947500b18a6adc2863e9c80e3499f72e9",
  "verdict_standards": "needs_work",
  "verdict_spec": "needs_work",
  "findings": [
    {"id": "F1", "severity": "HIGH", "confidence": "high", "rule": "DOC5", "status": "approved", "change": 1},
    {"id": "F2", "severity": "MEDIUM", "confidence": "high", "rule": "DOC5", "status": "approved", "change": 2},
    {"id": "F3", "severity": "MEDIUM", "confidence": "high", "rule": "FS5", "status": "approved", "change": 3},
    {"id": "F4", "severity": "MEDIUM", "confidence": "high", "rule": "DAT5", "status": "approved", "change": 4},
    {"id": "F5", "severity": "LOW", "confidence": "high", "rule": "DEP1", "status": "approved", "change": 5},
    {"id": "F6", "severity": "LOW", "confidence": "high", "rule": "FS7", "status": "approved", "change": 6},
    {"id": "F7", "severity": "LOW", "confidence": "high", "rule": "VRB4", "status": "approved", "change": 7},
    {"id": "F8", "severity": "LOW", "confidence": "high", "rule": "FS3", "status": "approved", "change": 8},
    {"id": "F9", "severity": "LOW", "confidence": "high", "rule": "DOC1", "status": "approved", "change": 9}
  ],
  "addenda": [{"id": "A1", "severity": "LOW", "rule": "FS5", "status": "approved"}, {"id": "A2", "severity": "LOW", "rule": "VRB3", "status": "approved"}],
  "coverage_skipped": ["devtools::check(): not run", "Google Group / GitHub issues: not searched", "KML reprojection accuracy: not verified"],
  "status": "pr-open",
  "pr": 39
}
```
