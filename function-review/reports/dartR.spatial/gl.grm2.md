# Review: gl.grm2 (dartR.spatial)

## 1. Header

- Family mode: analysis.
- Date: 2026-09-23.
- Reviewer: Claude (claude-opus-5-5), dartr-function-review v2.0.0.
- Package commit: `c685f00` (`origin/dev`). Work is on branch `review-gl.grm2` in worktree `../dartR.spatial-grm2`.
- Scope: a sync review, not a full review. `gl.grm2` is a copy of `gl.grm` added in `e8d6686` (2023-07-10), so that `gl.spatial.autoCorr(Dgen_method = "grm")` does not need dartR.captive. `dartR.captive::gl.grm` has since been reviewed and fixed (`4e52794`, "improve gl.grm: SilicoDArT guard, plot-save crash fix, doc corrections"). This review ports those fixes. The findings are the differences between the two functions.
- Datasets: `platypus.gl[1:12, 1:200]` (SNP), `testset.gs[1:12, 1:100]` (SilicoDArT).
- Baseline: `tests/testthat/test-gl.grm2.R`, captured before the port. Five snapshots: SNP and SilicoDArT with the heatmap on and off, and `plot.file` without the heatmap.

## 2. Verdict

**Standards: Needs work** — dependency guards and plot saving lag behind the reviewed `gl.grm`, and `palette_discrete` is documented but ignored.

**Spec: Needs work** — SilicoDArT data give a matrix without any warning, and `plot.file` with `plotheatmap = FALSE` fails. The SNP computation is correct: it is identical to `dartR.captive::gl.grm` and to `rrBLUP::A.mat`.

## 3. Findings

**F1 [MEDIUM, confidence: high] — SilicoDArT accepted silently (DAT1)**
`rrBLUP::A.mat(as.matrix(x) - 1)` assumes 0/1/2 dosages. Presence/absence data give a matrix that is not an additive relationship matrix (baseline `grm2-gs-*`).
Proposed change: stop with the same error as `gl.grm`.

**F2 [MEDIUM, confidence: high] — `plot.file` without a heatmap fails (PLT3)**
Failure scenario (baseline): `gl.grm2(x, plotheatmap = FALSE, plot.file = "g")` stops with "object 'p3' not found", and the matrix is lost.
Proposed change: warn that nothing was saved and return the matrix, as `gl.grm` does.

**F3 [LOW, confidence: high] — `palette_discrete` ignored; no `label.size`/`legend.title` (DOC5 (proposed rule); PLT1)**
Population colours always come from `gl.select.colors()`. The Shiny app passes `palette_discrete` and it has no effect.
Proposed change: use `palette_discrete` (a function or a vector) and add `label.size` and `legend.title` with `gl.grm`'s defaults.

**F4 [LOW, confidence: high] — gplots required even without plotting (DEP1)**
Without gplots, `gl.grm2(x, plotheatmap = FALSE)` returns `-1`, which also breaks `gl.spatial.autoCorr(Dgen_method = "grm")`.
Proposed change: check for gplots only when `plotheatmap = TRUE`.

**F5 [LOW, confidence: high] — roxygen out of date (DOC1; DOC7 (proposed rule))**
No Author(s) line; the `legendy` default is documented as 1 (actual 0.5); no note that the genotype matrix is densified; no mention that the function mirrors `gl.grm`.
Proposed change: take `gl.grm`'s header, state the relationship to `gl.grm`, and document that `palette_convergent` accepts a function or a vector.

Kept on purpose: the default heatmap palette (RdYlBu; `gl.grm` uses cm.colors), so existing plots look the same. `palette_convergent` accepts a vector as well as a function, because the Shiny app passes a function and earlier `gl.grm2` users may pass a vector.

## 4. Proposed changes

1. Port `dartR.captive::gl.grm` into `gl.grm2` (F1–F5), keeping the name, the default palette and vector palettes. **Consequence: SilicoDArT input now errors; `plot.file` with `plotheatmap = FALSE` returns the matrix with a warning; `palette_discrete` changes the population colours in heatmaps (including in Shiny). SNP matrices are unchanged.**

## 5. Coverage

- The SNP matrix was compared with `rrBLUP::A.mat` (fix test) and `dartR.captive::gl.grm` (in chat, before the port; not kept as a test to avoid a test-only dependency): identical.
- Heatmap on/off, palettes as a function and as a vector, legend options, and `plot.file` without a heatmap — run.
- Callers: `gl.spatial.autoCorr` (`plotheatmap = FALSE`, SNP only) is unaffected. `dartr_shiny` (`Fun_gl.grm2.R`) passes `legendx`, `legendy`, `palette_discrete` and `palette_convergent`; all are still accepted.
- The gplots-missing path (F4) is not tested, because gplots cannot be unloaded in-session.
- FBM-backed input: not tested; densification is documented.

## 6. Approval

| Change | Decision | By | Note |
|---|---|---|---|
| 1 | approved | Luis | approved as "sync gl.grm2 with the reviewed gl.grm", after the difference table in chat |

## 7. Outcome

Change 1 is implemented in `R/gl.grm2.r`; `devtools::document()` regenerated `man/gl.grm2.Rd`.

- Characterisation test (`test-gl.grm2.R`): SNP snapshots are unchanged. The SilicoDArT snapshots now record the error (F1), and the `plot.file` snapshot now records a returned matrix (F2). Snapshots updated; 9/9 pass.
- Fix tests (`test-gl.grm2-fixes.R`): 6/6 pass. They check equality with `rrBLUP::A.mat`, the SilicoDArT error, the `plot.file` warning, and palettes as a function and as a vector with the legend options.
- Full package test suite: 419 passed, 2 skipped (opt-in EEMS tests), 0 failed. R CMD check: 0 errors; its 3 warnings and 5 notes predate this change.

## 8. Machine block

```json
{
  "function": "gl.grm2",
  "package": "dartR.spatial",
  "family": "analysis",
  "skill_version": "2.0.0",
  "commit": "c685f00",
  "verdict_standards": "needs_work",
  "verdict_spec": "needs_work",
  "findings": [
    {"id": "F1", "severity": "MEDIUM", "confidence": "high", "rule": "DAT1", "status": "approved", "change": 1},
    {"id": "F2", "severity": "MEDIUM", "confidence": "high", "rule": "PLT3", "status": "approved", "change": 1},
    {"id": "F3", "severity": "LOW", "confidence": "high", "rule": "DOC5", "status": "approved", "change": 1},
    {"id": "F4", "severity": "LOW", "confidence": "high", "rule": "DEP1", "status": "approved", "change": 1},
    {"id": "F5", "severity": "LOW", "confidence": "high", "rule": "DOC1", "status": "approved", "change": 1}
  ],
  "coverage_skipped": ["gplots-missing path: not testable in-session", "FBM input: not tested", "Sync review: full standards walk not repeated; gl.grm was reviewed in dartR.captive"],
  "status": "applied",
  "pr": null
}
```
