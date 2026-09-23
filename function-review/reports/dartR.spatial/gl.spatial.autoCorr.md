# Review: gl.spatial.autoCorr + utils.spautocor (dartR.spatial)

## 1. Header

- Family mode: analysis.
- Date: 2026-09-23.
- Reviewer: Claude (claude-opus-5-5), dartr-function-review v2.0.0.
- Package commit: `c685f00` (`origin/dev`, after PR #41). Work is on branch `review-gl.spatial.autoCorr` in worktree `../dartR.spatial-sac`.
- Scope: `gl.spatial.autoCorr` and the exported helper `utils.spautocor`, which does the calculation for it.
- Datasets: `platypus.gl` (all three populations, and TENTERFIELD alone, loci 1–300); `testset.gs` (EmmacMaclGeor, loci 1–200); synthetic matrices; a simulated isolation-by-distance dataset (80 individuals on a 1° transect, 400 loci, allele frequencies on a gradient), as SNP and as presence/absence.
- Baseline: `tests/testthat/test-gl.spatial.autoCorr.R`, captured before critical source review. There are 13 snapshots: four SNP distance methods, populations plotted together, no resampling, SilicoDArT `Jaccard`, matrix and list inputs with and without `x`, and `utils.spautocor` with and without resampling.
- Probes: `function-review/evidence/gl.spatial.autoCorr-probes.R` and `-probes2.R`, with logs.
- Reference: `PopGenReport:::spautocor` (the original code). On the same squared-distance input, `utils.spautocor` reproduces its r exactly (0.1554, 0.0487, -0.0974).
- Runtime: R 4.4 (`/usr/local/bin/Rscript`) on macOS arm64.

## 2. Verdict

**Standards: Needs work** — dependency guards return `-1` (one for a package that is never used), there are ggplot2 deprecations, a message ignores `verbose`, and the roxygen has a truncated `verbose` entry and a wrong `@return`.

**Spec: Rework** — the handling of genetic distances gives the wrong sign or impossible values for three documented methods. Automatic distance classes fail when distances do not start near zero, Mercator distances are inflated, and the documented matrix and list inputs fail.

The core coefficient in `utils.spautocor` matches PopGenReport. Permutation shuffles individuals correctly, and the `propShared` and `Euclidean` paths give the expected sign.

## 3. Findings

**F1 [HIGH, confidence: high] — `Simple` and `Absolute` distances are reversed, flipping the sign of r (FS6; DOC5 (proposed rule))**
`R/gl.spatial.autoCorr.r:366–376` — `1 - Dgen` is applied to `Euclidean`, `Simple` and `Absolute`, which are distances, and to `propShared`, which is a similarity. The documented intent is to reverse only similarities. Operator precedence also makes the SNP condition apply only to `propShared`, so SilicoDArT `Simple` is reversed as well.
Failure scenario (P1, Q1, Q2): in a simulated population with strong isolation by distance, r in the first distance class is negative for `Simple` (-0.070) and `Absolute` (-0.044). The same distances without the reversal give +0.122 and +0.299. SilicoDArT `Simple` gives -0.068 against +0.119. A user would conclude that neighbours are less related than expected. For `Euclidean` the reversal only shifts r slightly (0.0891 vs 0.0846), because the diagonal is zeroed after reversing.
Proposed change: use distances as they are, and convert only similarities (see F2).

**F2 [HIGH, confidence: high] — `grm` is a similarity used as a distance (FS6)**
`R/gl.spatial.autoCorr.r:359–360` — the genomic relationship matrix is passed on unchanged and its diagonal is set to 0.
Failure scenario (P1): r = 13.67 in the first class. r is bounded by ±1, so the output is meaningless.
Proposed change: convert the relationship matrix G to a squared distance, d²ij = Gii + Gjj − 2Gij. This gives r = 0.168 on the simulated data, equal to squared Euclidean. `propShared` keeps `1 - propShared`.

**F3 [MEDIUM, confidence: high] — `Euclidean` is not squared, unlike Smouse & Peakall (1999) and GenAlEx (FS6)**
`R/gl.spatial.autoCorr.r:362` — `gl.dist.ind(method = "Euclidean")` returns unsquared distances. The r statistic double-centres −½ d², which is a covariance only when d is a squared distance. GenAlEx's codominant genetic distance is the sum of squared allele-dosage differences, that is, squared Euclidean.
Failure scenario (P1): the first-class r is 0.085 with the default method against 0.168 with squared Euclidean, so values are about half those reported by GenAlEx for the same data. Signs and permutation tests stay internally consistent.
Proposed change: square the Euclidean distance for SNP and SilicoDArT data and state this in the docs.

**F4 [HIGH, confidence: high] — automatic distance classes start at 0, not at the minimum distance (FS6)**
`R/utils.spautocor.r:83–87` — class breaks are `k * width` after the minimum, not `min + k * width`.
Failure scenario (Q3): distances between 50,023 and 58,948 with `bins = 5` give breaks at 1,785, 3,570, 5,355 and 7,140, then the maximum. All 435 pairs fall in the last class and the first four are empty. This happens with supplied between-site matrices and with any `Dgeo_trans` that shifts the minimum away from 0.
Proposed change: breaks at `min + k * width`, as documented.

**F5 [MEDIUM, confidence: high] — Mercator distances are inflated (FS6)**
`R/gl.spatial.autoCorr.r:302, 316` — lon/lat are projected with `dismo::Mercator()`, which scales distances by 1/cos(latitude).
Failure scenario (P11): TENTERFIELD distances are 14.3% longer than geodesic distances (1/cos 29° = 1.143). Distance classes are reported in inflated units, and with fixed breaks (for example `seq(0, 10000, 2000)`) pairs land in the wrong class. The distortion grows towards the poles and varies within populations that span a wide latitude range.
Proposed change: geodesic distances in metres with `terra::distance(lonlat = TRUE)`.

**F6 [MEDIUM, confidence: high] — documented matrix and list inputs fail (FS5; DOC5 (proposed rule))**
`R/gl.spatial.autoCorr.r:189, 413, 416`.
Failure scenario (P5, Q4, baseline): `gl.spatial.autoCorr(Dgeo = D, Dgen = D)` stops with 'argument "x" is missing'. A named list of `dist` objects stops with "object 'pop.names' not found". A list of matrices stops with "elements are of different classes", because `class()` of a matrix has length 2. Only `x = NULL` with an unnamed list of `dist` objects works.
Proposed change: default `x = NULL`; use list names as population names when present; compare the first class of each element.

**F7 [LOW, confidence: high] — one-tail p-value can be 0 (FS6)**
`R/gl.spatial.autoCorr.r:524–535` — p is the proportion of permutations as extreme as the observed value, without counting the observed value.
Failure scenario (P12): with `reps = 19`, every class reports `p.one.tail = 0`, which is not a valid permutation p-value.
Proposed change: p = (count + 1) / (reps + 1).

**F8 [LOW, confidence: high] — contradictory message when a genlight and matrices are both supplied (VRB2)**
`R/gl.spatial.autoCorr.r:251–265` — the message says the genlight is ignored, but the code uses the genlight and ignores the matrices (P3).
Proposed change: keep the behaviour and state it: warn that `Dgeo`/`Dgen` are ignored when `x` is a genlight.

**F9 [LOW, confidence: high] — dependency guards (DEP1)**
`R/gl.spatial.autoCorr.r:209–227` — the function requires dartR.popgen but never uses it (P13), and both guards return `-1`.
Failure scenario: a user without dartR.popgen gets `-1` instead of results.
Proposed change: remove the dartR.popgen guard; add a `stop(error())` guard for terra (needed by F5).

**F10 [LOW, confidence: high] — plotting and verbosity (PLT1; VRB1; FS3)**
`aes_string()` and `size` for lines are deprecated in ggplot2 and warn on every plot. "Scale for x is already present" prints for single populations at any `verbose`, and the full result table prints at `verbose = 1`. `build = "Jackson"` is outdated.
Proposed change: use `aes()` with `.data` pronouns and `linewidth`; add the secondary axis without a second scale; print the table at `verbose >= 3`; remove `build`.

**F11 [LOW, confidence: high] — roxygen inaccurate (DOC1; DOC2; DOC3; DOC5 (proposed rule))**
The `verbose` parameter text stops after "2,". `@return` describes a data frame, but the function returns a list of data frames, one per population. The details say distances over 1000 "are divided by 1000", but only the plot labels are divided. The `plot.theme` default is written as NULL. The reversal text (F1) is wrong, and the example is duplicated outside `\donttest{}`.
Proposed change: rewrite those parts and describe the distance conventions from F1–F3 and F5. Docs only.

## 4. Proposed changes

1. Use genetic distances as they are; convert `propShared` (1 − p) and `grm` (Gii + Gjj − 2Gij) (F1, F2). **Consequence: r changes sign for `Simple` and `Absolute` (SNP) and `Simple` (SilicoDArT); `grm` results change from impossible values to valid r; `Euclidean` shifts slightly (0.0891 → 0.0846 in P1); `propShared`, `Jaccard` and `Bray-Curtis` are unchanged.**
2. Square the Euclidean distance (F3). **Consequence: r for the default `Euclidean` method roughly doubles (0.085 → 0.168 in P1), matching GenAlEx.**
3. Automatic class breaks from the minimum distance (F4). **Consequence: class breaks and r change whenever the minimum distance is above 0 and `bins` is a single number.**
4. Geodesic distances instead of Mercator (F5). **Consequence: distance classes shrink (by 12.5% at latitude 29°), and with fixed breaks class membership and r change.**
5. Default `x = NULL`, named lists and lists of matrices accepted (F6).
6. p = (count + 1) / (reps + 1) (F7). **Consequence: `p.one.tail` values increase slightly and are never 0.**
7. Correct the genlight-plus-matrices message (F8).
8. Remove the dartR.popgen guard; `stop()` guard for terra (F9).
9. Plotting deprecations, message gating, remove `build` (F10).
10. Roxygen rewrite (F11). Docs only.

## 5. Coverage

- Standards walk: FS, DOC, VRB, DAT, DEP, PLT, STY, API — run.
- Spec: every documented SNP and SilicoDArT method on a simulated isolation-by-distance dataset with a known positive signal — run. Genlight, matrix, `dist`-list and matrix-list inputs — run. Automatic and fixed bins — run.
- Reference: `utils.spautocor` against `PopGenReport:::spautocor` on identical input — run; identical.
- GenAlEx itself: not run (not available). The squared-distance claim rests on Smouse & Peakall (1999) and the GenAlEx distance definition.
- Bootstrap intervals: checked only for their presence in the baseline, not for statistical coverage.
- Callers: no sibling `dartR.*` package calls either function. `dartr_shiny` (`Fun_gl.spatial.autoCorr.R`) calls `gl.spatial.autoCorr`; its argument use still needs checking before merge (grep below in Outcome).
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

Changes 1, 2, 3, 4 and 6 approved with their stated consequences.

## 7. Outcome

All ten approved changes are implemented in `R/gl.spatial.autoCorr.r` and `R/utils.spautocor.r`. `devtools::document()` regenerated `man/gl.spatial.autoCorr.Rd`. The new `@family` tag also adds cross-links to `man/gl.costdistances.Rd` and `man/gl.genleastcost.Rd`. `NAMESPACE` is unchanged.

- Characterisation test (`test-gl.spatial.autoCorr.R`): 12 of 13 snapshots changed, and every diff maps to an approved change:
  - squared Euclidean (change 2) and geodesic distances (change 4): `Euclidean` and no-resampling;
  - valid `grm` r instead of impossible values, and positive `Simple` r (change 1);
  - breaks from the minimum distance (change 3): matrix and `utils.spautocor` cases;
  - geodesic distances (change 4): classes shrink by a factor of 0.876 for `propShared`, with r unchanged in classes 1, 2 and 5;
  - p-values with the observed value counted (change 6);
  - matrix and list inputs now run (change 5).

  `utils-resampling` is unchanged because its snapshot records only the class of the output. Snapshots updated; 14/14 pass.
- Fix tests (`test-gl.spatial.autoCorr-fixes.R`): 28/28 pass (26 for changes 1–9, 2 for A1). They check that r is positive for every SNP and SilicoDArT method under simulated isolation by distance and that |r| <= 1. They also check exact agreement with squared Euclidean and with the distance implied by the GRM, breaks from the minimum, geodesic distances, all input forms, the p-value floor, the ignored-matrices warning and a silent plot.
- Probes re-run (`function-review/evidence/gl.spatial.autoCorr-after.log`): the first-class r is 0.168 for `Euclidean` and `grm` and 0.122 for `Simple`/`propShared`. For the 50–59 km matrix, classes hold 140/126/93/53/23 pairs, where all 435 were in one class before. All list inputs run. `p.one.tail` is 0.05 with 19 reps, where it was 0.
- The roxygen example runs without warnings.
- Full package test suite: 444 passed, 2 skipped (opt-in EEMS tests), 0 failed.

### Addendum findings (found during apply)

**A1 [LOW, confidence: high] — a coordinates data.frame fails for more than one population (FS6)**
With `coordinates` supplied as a data.frame, every population receives the whole table, so any object with several populations stops with "Cannot find coordinates for each individual" (`function-review/evidence/gl.spatial.autoCorr-addendum.log`). A single population works.
Proposed change: subset the data.frame rows by population, in the same order as `seppop()`.
Approved by Luis and applied; a data.frame now gives the same result as `x@other$latlon` (fix test A1), and one with the wrong number of rows stops with a clear error.

**A2 [INFO] — Mercator distances in sibling functions**
`gl.ibd` (line 268) and `gl.run.eems` (line 301) also use `dismo::Mercator()` for lon/lat input. `gl.ibd` was reviewed in PR #37, so this is for the custodian to decide.

## 8. Machine block

```json
{
  "function": "gl.spatial.autoCorr",
  "package": "dartR.spatial",
  "family": "analysis",
  "skill_version": "2.0.0",
  "commit": "c685f00",
  "verdict_standards": "needs_work",
  "verdict_spec": "rework",
  "findings": [
    {"id": "F1", "severity": "HIGH", "confidence": "high", "rule": "FS6", "status": "approved", "change": 1},
    {"id": "F2", "severity": "HIGH", "confidence": "high", "rule": "FS6", "status": "approved", "change": 1},
    {"id": "F3", "severity": "MEDIUM", "confidence": "high", "rule": "FS6", "status": "approved", "change": 2},
    {"id": "F4", "severity": "HIGH", "confidence": "high", "rule": "FS6", "status": "approved", "change": 3},
    {"id": "F5", "severity": "MEDIUM", "confidence": "high", "rule": "FS6", "status": "approved", "change": 4},
    {"id": "F6", "severity": "MEDIUM", "confidence": "high", "rule": "FS5", "status": "approved", "change": 5},
    {"id": "F7", "severity": "LOW", "confidence": "high", "rule": "FS6", "status": "approved", "change": 6},
    {"id": "F8", "severity": "LOW", "confidence": "high", "rule": "VRB2", "status": "approved", "change": 7},
    {"id": "F9", "severity": "LOW", "confidence": "high", "rule": "DEP1", "status": "approved", "change": 8},
    {"id": "F10", "severity": "LOW", "confidence": "high", "rule": "PLT1", "status": "approved", "change": 9},
    {"id": "F11", "severity": "LOW", "confidence": "high", "rule": "DOC1", "status": "approved", "change": 10}
  ],
  "addenda": [{"id": "A1", "severity": "LOW", "rule": "FS6", "status": "approved"}, {"id": "A2", "severity": "INFO", "rule": "FS6", "status": "not-actioned"}],
  "coverage_skipped": ["GenAlEx comparison: not available", "Bootstrap coverage: not assessed", "Google Group / GitHub issues: not searched"],
  "status": "pr-open",
  "pr": 42
}
```
