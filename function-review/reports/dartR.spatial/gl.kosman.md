# Review: gl.kosman (dartR.spatial)

## 1. Header

- Family mode: analysis.
- Date: 2026-09-23.
- Reviewer: Claude (claude-opus-5-5), dartr-function-review v2.0.0.
- Package commit: `0bbc487947500b18a6adc2863e9c80e3499f72e9` (`dev_luis`). `R/gl.kosman.r` had no local changes.
- Datasets: `testset.gl[1:8, 1:40]`, `testset.gs[1:8, 1:40]`, `possums.gl[1:5, 14:21]` (baseline); `testset.gl`, `testset.gs`, `possums.gl` subsets of 15 x 200 and `platypus.gl` (81 x 1000) for probes.
- Baseline: `tests/testthat/test-gl.kosman.R`, captured before critical source review. Three snapshots: SNP, SilicoDArT and the roxygen example, recording the full `kosman` and `nloci` matrices.
- Probes: `function-review/evidence/gl.kosman-probes.R`, output in `function-review/evidence/gl.kosman-probes.log`.
- Runtime: R 4.4 (`/usr/local/bin/Rscript`) on macOS arm64.

## 2. Verdict

**Standards: Needs work** — the ploidy check fails with an R-internal message, the start flag is outdated, there is no end flag, and the roxygen header is incomplete.

**Spec: Needs work** — the distances are correct, but memory grows with loci x individuals², so the function cannot run on typical DArT datasets, and `@return` describes a matrix while the function returns a list of two lower-triangular matrices.

Numerical output matches an independent Kosman & Leonard (2005) computation (mean over shared non-missing loci of |a - b| / ploidy) to within 2e-16 on SNP, SilicoDArT and `possums.gl`.

## 3. Findings

**F1 [HIGH, confidence: high] — memory scales with nLoc x nInd² (DAT6 (proposed rule); STY2)**
`R/gl.kosman.r:46–52` — one nInd x nInd matrix is built per locus, then copied twice (`missing`, `replaced`). All three lists are held at once.
Failure scenario: 20,000 loci x 300 individuals needs 20,000 x 300² x 8 bytes = 14.4 GB per list, about 43 GB in total, so the call fails on a normal workstation. `platypus.gl` (81 x 1000) already takes 1.04 s; a matrix-product version takes 0.03 s and gives identical distances (max difference 0).
Proposed change: compute the distance sums and shared-locus counts with `tcrossprod()` on dosage indicator matrices. Memory becomes nInd x nLoc plus a few nInd x nInd matrices. Distances are unchanged.

**F2 [MEDIUM, confidence: high] — mixed-ploidy check fails with an R-internal error (FS5; VRB2)**
`R/gl.kosman.r:33–43` — `uniqueploidy > 1` compares a vector, which errors in R >= 4.2. The intended message goes through `message()` and a separate `stop("Script stopped!")`. The `ploidy <= 0` branch is unreachable when ploidy is unique.
Failure scenario (P1): individuals with ploidy 2 and 4 give "the condition has length > 1" instead of the intended explanation.
Proposed change: one `stop(error(...))` for mixed ploidy and one for ploidy <= 0; replace the genlight check with `stop(error(...))` as well.

**F3 [MEDIUM, confidence: high] — `@return` does not match the output (DOC5 (proposed rule))**
`R/gl.kosman.r:6` — it says "a matrix of [dimensions nInd(x) x nInd(x)]". The function returns a list with `kosman` and `nloci`, both lower-triangular, with `NA` above the diagonal.
Failure scenario: a user following the docs calls `gl.kosman(x)[1, 2]` and gets an error or a list element instead of a distance.
Proposed change: document the list, its two elements, the `NA` upper triangle and `as.dist(result$kosman)` for downstream use. Docs only; `gl.ibd`, `gl.genleastcost` and `dartr_shiny` use `$kosman` and are unaffected.

**F4 [LOW, confidence: high] — pairs with no shared loci give NaN silently (VRB4 (proposed rule))**
`R/gl.kosman.r:60` — 0/0 when two individuals have no non-missing locus in common.
Failure scenario (P2): the pair returns `NaN`. Downstream, `gl.ibd` now stops on non-finite distances without saying which pair caused it.
Proposed change: warn at `verbose >= 1` with the number of such pairs. Values stay `NaN`.

**F5 [LOW, confidence: high] — `nloci` diagonal is nLoc, not the individual's called loci**
`R/gl.kosman.r:55` — `dist()` returns 0 on the diagonal even when the individual's genotype is missing, so missing loci are not counted there.
Failure scenario: in `platypus.gl[1:6, 1:50]`, T27 has 45 called loci but `nloci["T27", "T27"]` is 50.
Proposed change: set the diagonal to the number of non-missing loci per individual (falls out of F1's `tcrossprod`). **Consequence: diagonal values of `nloci` change for individuals with missing data; off-diagonal values and all distances are unchanged.**

**F6 [LOW, confidence: high] — outdated start flag, no end flag (FS3; FS9)**
`R/gl.kosman.r:22–24` — `build = "Jody"` is outdated; there is no "Completed" message, so `verbose = 3` prints only "Starting gl.kosman" (P3).
Proposed change: remove `build`; add the FS9 end flag.

**F7 [LOW, confidence: high] — roxygen incomplete (DOC1; DOC2; DOC7 (proposed rule))**
`R/gl.kosman.r:1–12` — no `@title`/`@family`/`@author`/`@references`; `verbose` text non-standard; "This script calculates…". The docs do not say how missing data are handled (per-pair shared loci) or that SilicoDArT is treated as haploid (simple mismatch proportion).
Proposed change: rewrite the header in house order; cite Kosman & Leonard (2005) Molecular Ecology 14:415–424; describe missing-data handling and SilicoDArT behaviour. Docs only.

## 4. Proposed changes

1. Vectorised computation with `tcrossprod()` (F1). Distances unchanged; memory and time drop.
2. Clear errors for mixed or invalid ploidy and non-genlight input (F2).
3. Correct `@return` (F3). Docs only.
4. Warn when pairs share no loci (F4).
5. `nloci` diagonal = called loci per individual (F5). **Consequence: diagonal values of `nloci` change for individuals with missing data.**
6. Remove `build = "Jody"`; add end flag (F6).
7. Roxygen rewrite (F7). Docs only.

## 5. Coverage

- Standards walk: FS, DOC, VRB, DAT, DEP, PLT, STY, API — run. PLT not applicable.
- Spec: independent recomputation on SNP, SilicoDArT and `possums.gl` — run; matches.
- Scaling: timed at 250 and 1000 loci on `platypus.gl`; memory for large data estimated analytically, not measured (running it would exhaust RAM).
- Callers: `gl.ibd` and `gl.genleastcost` (this package) and `dartr_shiny` (`Fun_gl.kosman.R`) use `$kosman` through `as.dist()` or export it as a table; none of the proposed changes alter `$kosman`.
- Polyploid data (ploidy > 2): not tested on real data; the formula |a - b| / ploidy is correct for biallelic dosages.
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

Change 5 approved with its stated output consequence.

## 7. Outcome

All seven approved changes are implemented in `R/gl.kosman.r`; `devtools::document()` regenerated `man/gl.kosman.Rd` (no `NAMESPACE` change).

- Characterisation test (`test-gl.kosman.R`): the only diffs are the diagonals of `nloci` in the `testset.gl` and `testset.gs` snapshots (for example AA010915: 40 -> 37), which is change 5. Every `kosman` value and the `possums.gl` snapshot are unchanged. Snapshots updated; 6/6 pass.
- Fix tests (`test-gl.kosman-fixes.R`): 15/15 pass. They cover agreement with an independent computation on SNP, SilicoDArT and `platypus.gl`, hand-checked tetraploid values, and changes 2, 4, 5 and 6.
- Probes re-run (`function-review/evidence/gl.kosman-after.log`): mixed ploidy now stops with "individuals have different ploidies…"; `platypus.gl` at 1000 loci runs in 0.01 s (was 1.04 s); results equal the matrix-product reference exactly, including `nloci`.
- Large data (`function-review/evidence/gl.kosman-large.R`): 300 individuals x 20,000 loci run in 2.8 s with 422 MB peak vector memory. The previous code needed about 43 GB for the same input (estimated, not run).
- The "peak MB" figures printed by probe P6 read the wrong `gc()` column and are not valid; timings are.
- Not run: full `devtools::check()`.

## 8. Machine block

```json
{
  "function": "gl.kosman",
  "package": "dartR.spatial",
  "family": "analysis",
  "skill_version": "2.0.0",
  "commit": "0bbc487947500b18a6adc2863e9c80e3499f72e9",
  "verdict_standards": "needs_work",
  "verdict_spec": "needs_work",
  "findings": [
    {"id": "F1", "severity": "HIGH", "confidence": "high", "rule": "DAT6", "status": "approved", "change": 1},
    {"id": "F2", "severity": "MEDIUM", "confidence": "high", "rule": "FS5", "status": "approved", "change": 2},
    {"id": "F3", "severity": "MEDIUM", "confidence": "high", "rule": "DOC5", "status": "approved", "change": 3},
    {"id": "F4", "severity": "LOW", "confidence": "high", "rule": "VRB4", "status": "approved", "change": 4},
    {"id": "F5", "severity": "LOW", "confidence": "high", "rule": "DOC5", "status": "approved", "change": 5},
    {"id": "F6", "severity": "LOW", "confidence": "high", "rule": "FS3", "status": "approved", "change": 6},
    {"id": "F7", "severity": "LOW", "confidence": "high", "rule": "DOC1", "status": "approved", "change": 7}
  ],
  "coverage_skipped": ["devtools::check(): not run", "Large-data memory: estimated, not measured", "Polyploid real data: not tested", "Google Group / GitHub issues: not searched"],
  "status": "applied",
  "pr": null
}
```
