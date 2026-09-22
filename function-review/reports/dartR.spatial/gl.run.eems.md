# Review: gl.run.eems (dartR.spatial)

## 1. Header

- Family mode: analysis (external EEMS runner and plotting wrapper).
- Date: 2026-09-22.
- Reviewer: Codex (GPT-6; exact model variant not available in session metadata), dartr-function-review v2.0.0.
- Package commit: `209ef9aaef4e61dbbd9c82d789ff3f94e53d620e`, `dev_luis`.
- Reviewed file: `R/gl.run.eems.r`; SHA256 `a43e4b1fde8d064180372d70e7d01f3d6b978950e200db990c471a0d1f77d447`.
- Datasets: `testset.gl[1:8, 1:60]`, `testset.gs[1:8, 1:60]`, `bandicoot.gl[1:12, 1:200]`.
- Baseline: [test-gl.run.eems.R](../../../tests/testthat/test-gl.run.eems.R). The first two dataset snapshots preceded critical source review; the successful SNP numerical anchors were added during targeted review.
- Environment: macOS arm64, R 4.4.2, dartR.base 1.2.3, dartR.data 1.2.5, reemsplots2 0.1.0 (`fbc2de644d9f591c91b795489b5026f9851f7fd8`). The Homebrew R has no required packages; use `/Library/Frameworks/R.framework/Resources/bin/Rscript`.
- EEMS executable: `/Users/mijangos/programs/runeems_snps`; SHA256 `c8910d4f7cae07fa941a995c66777fa5fd7f202d576eddb703413af45df9fcf9`. Version string unavailable; banner reports Boost 1.89 and Eigen 3.4.0.
- Branch check: fetched `origin`; `origin/dev` (`19b73861a138541073393c6b6468dfc7210b64c9`) is already an ancestor of `dev_luis`. Reviewed function matches `origin/dev`. No campaign manifest existed on either branch; created a local claim for Luis. Existing unrelated working-tree changes were preserved.

## 2. Verdict

**Standards: Needs work** — process errors, cleanup ownership, path handling and dependency failures need bounded fixes.

**Spec: Needs work** — failed analyses can return earlier plots, input ploidy can disagree with the fitted model, and documented plotting arguments are ignored.

The distance matrix agrees with an independent mean-imputed squared-distance calculation within `1.11e-15`. A real SNP run returned all eight documented plots and left the input object unchanged.

## 3. Findings

**F1 [HIGH, confidence: high] — a failed process can return previous results (FS5; result provenance principle)**

`R/gl.run.eems.r:388` ignores the exit status; line 390 reads a reused `data_<plot.file>` directory.
Failure scenario: a successful 40-iteration bandicoot run followed by `numMCMCIter = 0`, the same seed and default name. EEMS rejected the second run, but the wrapper returned eight plots; every raw output file remained byte-for-byte unchanged.
Proposed change: stop on nonzero exit status, preserve the error log, and use a fresh run directory. Validate that expected outputs belong to that run before plotting or copying. FS5 covers error handling; the catalogue lacks a specific external-result provenance rule.

**F2 [HIGH, confidence: high] — cleanup deletes files it does not own (FS7; file ownership principle)**

`R/gl.run.eems.r:391` selects files by an unanchored regular expression; lines 443–445 delete the matches recursively.
Failure scenario: a disposable `unrelated_eems_notes.txt` placed in the R temporary directory was deleted by a default-named run. Matching directories are also eligible for recursive deletion. With default `out.dir` and cleanup, the raw EEMS results are deleted as well.
Proposed change: track exact generated paths inside a dedicated run directory, preserve final results in the resolved output directory, and remove only owned intermediate files. Apply the same exact-path selection to copying. The catalogue lacks an explicit deletion-ownership rule.

**F3 [HIGH, confidence: high] — input ploidy can disagree with the fitted model (FS4, DAT1)**

`R/gl.run.eems.r:235` accepts the datatype utility's default types but does not use `dt`; line 269 writes the independent `diploid` argument.
Failure scenario: `testset.gs[1:8, 1:60]`, with ploidy 1, completed the real SNP executable and returned plots with `diploid = TRUE`. Presence/absence calls do not establish diploid SNP dosage.
Proposed change: validate a scalar logical `diploid` against uniform input ploidy before writing files: TRUE requires ploidy 2; FALSE requires ploidy 1. Document that the model requires SNP allele dosages. This guard checks ploidy, not marker biology: setting FALSE does not establish support for dominant SilicoDArT markers, and valid haploid SNP use must remain possible.

**F4 [MEDIUM, confidence: high] — relative output directories are resolved from the wrong location (FS7)**

`R/gl.run.eems.r:379` changes the working directory before lines 392–399 copy files to `out.dir`. Copy results are unchecked.
Failure scenario: `out.dir = "results"` with an existing `results/` directory under the caller's working directory left that directory empty. Copying warned, then the wrapper continued. With cleanup enabled, the source results can subsequently be removed.
Proposed change: resolve and validate the absolute destination before changing directories; check every required copy and retain source results if copying fails.

**F5 [MEDIUM, confidence: high] — spaces in the plot name break execution (FS5; process argument handling principle)**

`R/gl.run.eems.r:349`–356 constructs an unquoted shell command from `plot.file`.
Failure scenario: `plot.file = "review output"` produced `./runeems_snps --params param_review output.ini --seed 17`. Executing this exact command against the generated file returned status 1.
Proposed change: invoke the executable with separately quoted arguments and an absolute parameter-file path; reject path separators in an argument documented as a base name. No Windows or Linux execution was tested.

**F6 [MEDIUM, confidence: high] — additional plotting parameters are discarded (DOC5, proposed rule)**

`R/gl.run.eems.r:54` promises forwarding through `...`, but lines 401–410 omit it.
Failure scenario: supplied `prob_level = 0.75` and `add_abline = TRUE` never reached a plotting-call recorder. The real plotting function supports both arguments, so the requested probability threshold and diagnostic line are ignored.
Proposed change: forward supported extra arguments and report unknown or duplicate arguments clearly.

**F7 [MEDIUM, confidence: high] — NULL cannot disable plot saving (PLT2 and DOC5, proposed rules)**

`R/gl.run.eems.r:239`–240 replaces NULL with `"eems"`; the save check at line 450 therefore always succeeds.
Failure scenario: the real successful run used the documented default `plot.file = NULL` and nevertheless wrote `eems.RDS`. Repeated calls use the same destination and can overwrite an existing plot file.
Proposed change: keep the requested plot-save name separate from the internal run identifier; save RDS only when the user supplies `plot.file`.

**F8 [MEDIUM, confidence: high] — missing dependencies return a success-like value (DEP1, FS5)**

`R/gl.run.eems.r:184`–214 prints a dependency error and returns `-1`.
Failure scenario: making the availability check report missing `sf` returned numeric `-1`; callers using `tryCatch(error = ...)` would not detect the failed analysis.
Proposed change: raise an actionable error for missing required packages before any work. Apply to the existing guards; no dependency installation was performed during review.

**F9 [MEDIUM, confidence: high] — verbose = 0 prints progress and results (VRB1, FS9)**

`R/gl.run.eems.r:388`, 401, 415 and 441 expose executable, plotting, palette and list-printing output regardless of verbosity. The return at line 456 has no completion flag.
Failure scenario: the real SNP run at `verbose = 0` printed EEMS iteration diagnostics, plotting progress, `gl.colors` messages and all eight list names.
Proposed change: capture process output in a run log, gate normal messages and printing by verbosity, and add the standard completion message. Fatal failures must remain visible. This proposal does not add a plot-display argument or change the returned plots.

**F10 [LOW, confidence: high] — dpi guidance confuses grid sampling with image export (DOC5, proposed rule)**

`R/gl.run.eems.r:72`–74 recommends 600 dpi for publication output, but line 403 passes `dpi` into reemsplots2's interpolation-grid construction; the wrapper saves R objects, not raster images.
Failure scenario: a user sets `dpi = 600` expecting a 600-dpi exported figure. No such image is exported; the parameter increases grid sampling and work instead.
Proposed change: describe this argument as contour-grid resolution and explain that image dimensions and export DPI are set when saving a rendered figure. Keep the argument name and default unchanged.

### Addendum A1 — function-object calls fail when verbosity is enabled

**A1 [LOW, confidence: high] — start-message formatting rejects a function object (FS3, catalogue edge case)**

The existing `match.call()[[1]]` idiom returns a closure for
`do.call(gl.run.eems, args)`; at `verbose = 1`, the start-message formatter
cannot convert it to text. Calling by name, including
`do.call("gl.run.eems", args)`, works. This was reproduced against the original
HEAD implementation; it was not introduced by these fixes.

Proposed change 11: when the call head is a function object, use
`"gl.run.eems"` as the message label; otherwise preserve the existing label.
**Consequence: function-object do.call invocations at verbose > 0 can run
instead of failing during the start message. Numerical output is unchanged.**
Evidence: [original-state reproduction](../../evidence/gl.run.eems-addendum.log).
Decision: pending; no addendum fix applied.

## 4. Proposed changes

Each number is independently approvable. Changes 1 and 2 should share one implementation of run-directory ownership if both are approved.

1. Check process status and isolate each run (F1). **Consequence: failed runs error instead of returning earlier results.**
2. Limit copy/cleanup to owned files and retain final raw results (F2). **Consequence: cleanup no longer deletes unrelated matching files or final EEMS output; default runs retain raw results for the R session.**
3. Validate the model setting against input ploidy (F3). **Consequence: incompatible diploid settings, including the current default call on testset.gs, error instead of producing model output. Ploidy-1 input with diploid = FALSE is not blanket-rejected.**
4. Resolve output paths early and check copying (F4). **Consequence: relative destinations refer to the caller's directory; failed exports error and retain source results.**
5. Quote executable arguments and validate the base name (F5). **Consequence: names containing spaces work; names containing path separators error explicitly.**
6. Honour extra plotting arguments (F6). **Consequence: supplied probability thresholds change plotted probability contours; requested diagnostic options take effect, and invalid extras error. EEMS inference is unchanged.**
7. Make NULL mean no RDS save (F7). **Consequence: the default call stops creating or overwriting `eems.RDS`; explicitly named saves remain available.**
8. Raise dependency errors (F8). **Consequence: a missing dependency raises an error rather than returning `-1`.**
9. Honour verbosity and record process diagnostics (F9). **Consequence: verbose = 0 suppresses routine output; fatal errors remain visible and plots remain returned.**
10. Correct the dpi documentation (F10; documentation only).

## 5. Coverage

Evidence is in [the probe script](../../evidence/gl.run.eems-probes.R), [recorded facts](../../evidence/gl.run.eems-probes.json), [probe log](../../evidence/gl.run.eems-probes.log) and [baseline log](../../evidence/gl.run.eems-baseline.log).

- **Baseline:** reference snapshots preserve dimensions, ploidy, metric row counts, result/error shape and unchanged input. Targeted bandicoot checks additionally pin the distance matrix dimensions, six numerical values and eight result names. Eight assertions passed; three warnings came from deprecated calls inside reemsplots2/tibble.
- **Real execution:** SNP bandicoot run, SilicoDArT baseline, failed-run reuse and executable handling of spaces. Runs used 20 demes and 30–40 iterations solely to exercise the wrapper. **MCMC convergence and biological inference were not assessed.**
- **Independent numerical check:** mean-impute genotypes by locus, then compute `as.matrix(dist(G))^2 / ncol(G)`; maximum difference from the generated `.diffs` matrix was `1.11e-15`.
- **Instrumented probes:** local function copies and a temporary mock of the plotter tested argument forwarding, cleanup sentinels, destinations and dependency errors. Cleanup used disposable files inside a fresh R-process temporary directory. No user-owned data was deleted. These checks do not claim the mocked paths completed real inference.
- **Standards walk:** FS, DOC, VRB, DAT, DEP, PLT and STY reviewed. Input serialization was unchanged; no returned genlight or inappropriate history append. The distance helper is computed once. Companion metadata handling is delegated to `gl.filter.allna` and was not independently audited beyond the fixtures.
- **Documentation:** no source/man edit was made, so regeneration was unnecessary. Missing campaign family/custodian metadata (DOC1/DOC7), formatting and optional plot-theme conventions are lower-priority catalogue notes, not additional behaviour findings. Custodian identity was not inferred from author names. No linter/R CMD check diagnostics were promoted into findings.
- **False-positive checks:** the small SNP `testset.gl` subset fails EEMS's full-rank distance requirement; this alone is not a wrapper arithmetic defect. Removing `@other$latlon` still succeeded on the bandicoot fixture through existing metadata handling, so no universal missing-coordinate claim is made. Identical coordinates cause an SF polygon error; this is recorded as a guard improvement opportunity, not a separate finding. `longlat = TRUE` was not labelled a proven coordinate-system bug without an independent map comparison.
- **External references:** distance construction agrees with [EEMS's bed2diffs documentation](https://github.com/dipetkov/eems/blob/master/bed2diffs/README.md); the result names match [reemsplots2](https://github.com/dipetkov/reemsplots2). A [2024 dartR Group path complaint](https://groups.google.com/g/dartr/c/FZLxCSQ2-14) was read; it does not prove the specific space-handling defect reproduced here. The dartR.spatial GitHub issue search found no EEMS matches.
- **Skipped:** native Windows/Linux runs (only macOS available); FBM-backed input and large-memory scaling (no fixture; dense conversion remains an unquantified DAT6 concern); visual/geospatial validation of rendered surfaces; full package check (production source unchanged). Standard optional-save semantics under PLT2 and documentation agreement under DOC5 remain explicitly proposed rules.
- **Before a behaviour-changing merge:** inspect sibling dartRverse/dartr2shiny callers and add NEWS under API1–API3. This is pending approval and implementation, not evidence of compatibility.

Rerun from `/Users/mijangos/dartR.spatial`:

```sh
DARTR_EEMS_REVIEW=true NOT_CRAN=true /Library/Frameworks/R.framework/Resources/bin/Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-gl.run.eems.R")'
/Library/Frameworks/R.framework/Resources/bin/Rscript function-review/evidence/gl.run.eems-probes.R
```

## 6. Approval

| Change | Decision | By | Note |
|---|---|---|---|
| 1 | approved | Luis | “go ahead with all the fixes” (2026-09-22); consequences presented individually. |
| 2 | approved | Luis | “go ahead with all the fixes” (2026-09-22); consequences presented individually. |
| 3 | approved | Luis | “go ahead with all the fixes” (2026-09-22); consequences presented individually. |
| 4 | approved | Luis | “go ahead with all the fixes” (2026-09-22); consequences presented individually. |
| 5 | approved | Luis | “go ahead with all the fixes” (2026-09-22); consequences presented individually. |
| 6 | approved | Luis | “go ahead with all the fixes” (2026-09-22); consequences presented individually. |
| 7 | approved | Luis | “go ahead with all the fixes” (2026-09-22); consequences presented individually. |
| 8 | approved | Luis | “go ahead with all the fixes” (2026-09-22); consequences presented individually. |
| 9 | approved | Luis | “go ahead with all the fixes” (2026-09-22); consequences presented individually. |
| 10 | approved | Luis | “go ahead with all the fixes” (2026-09-22); consequences presented individually. |

Luis approved all ten original changes after the individual behavioural consequences were presented. They are implemented and verified. Addendum 11 remains pending.

## 7. Outcome

All ten approved changes are applied locally. Finding locations above refer to the original reviewed commit, not the rewritten file. Luis approved the displayed commit message and PR title with “ok” on 2026-09-22. Publication is authorised; the PR identifier will be recorded after creation.

| Change | Applied result | Verification |
|---|---|---|
| 1 | Fresh output directory, process-status and required-file checks | Failed/incomplete runs never reach plotting; real failed rerun errors. |
| 2 | Exact intermediate-file cleanup; raw results/logs retained | Sentinels, previous runs and raw posterior traces survive. |
| 3 | Scalar diploid flag checked against every individual's ploidy | Mismatch errors before execution; FALSE with ploidy 1 remains possible. |
| 4 | Absolute destination resolved before running; no working-directory change or export copy needed | Results written directly under the caller's relative destination. |
| 5 | Direct executable invocation with quoted arguments and validated base name | Real runs with spaces in output paths, executable location and plot name pass. |
| 6 | Supported extra plotting arguments forwarded; unknown/duplicate/reserved names rejected | Call recorder verifies 0.75 threshold and diagnostic line; real plotting completes. |
| 7 | Internal run identifier separate from optional RDS name | NULL creates no RDS; an explicit name creates the expected RDS. |
| 8 | Missing dependencies raise actionable errors | All three dependency guards tested. |
| 9 | Routine diagnostics retained in logs; console and plot display follow verbosity | Level 0 has no routine output; level 3 completes with the real binary. |
| 10 | Grid-resolution and export-DPI documentation corrected | devtools::document() changed only man/gl.run.eems.Rd. |

Verification: the full package test suite passed, including **53 new regression assertions and eight EEMS baseline assertions**, with no test failures, warnings or skips. R printed four package-build-version notices during startup. The six numerical distance anchors and returned plot names remain unchanged. The two snapshot changes map exactly to F1 (direct process error with log) and F3 (ploidy mismatch error); original snapshots are retained in evidence. The original three plotting-library deprecation warnings are now retained in the run log at verbosity 0, as approved under F9.

The real integration run also compared the seeded posterior trace against the original `209ef9a` implementation: **byte-identical**. It verified repeated runs, failed-run isolation, unchanged input, relative paths, spaces, optional RDS saving, preserved sentinels, and verbosity 0/3. This is execution/regression evidence, not a convergence assessment.

Evidence: [regression tests](../../../tests/testthat/test-gl.run.eems-fixes.R), [full-suite log](../../evidence/gl.run.eems-suite.log), [real integration script](../../evidence/gl.run.eems-integration.R), [integration log](../../evidence/gl.run.eems-integration.log), [caller check](../../evidence/gl.run.eems-callers.md). NEWS records the approved behaviour changes. No production sibling-package callers were found. The generated Shiny caller uses named arguments and the returned plot list; its signature remains compatible. Its copied input-generator source was not refreshed, and the Shiny app was not run.

Remaining limitations: native Windows/Linux runs, large FBM data and geographic plot validation remain untested; full R CMD check was not run. Addendum 11 is a reproduced pre-existing call-formatting edge case and remains unmodified pending its own approval. No open PR from dev_luis was present when checked.

## 8. Machine block

```json
{
  "function": "gl.run.eems",
  "package": "dartR.spatial",
  "family": "analysis",
  "skill_version": "2.0.0",
  "model": "GPT-6; exact variant not available in session metadata",
  "commit": "209ef9aaef4e61dbbd9c82d789ff3f94e53d620e",
  "datasets": [
    "testset.gl[1:8,1:60]",
    "testset.gs[1:8,1:60]",
    "bandicoot.gl[1:12,1:200]"
  ],
  "verdict_standards": "needs_work",
  "verdict_spec": "needs_work",
  "findings": [
    {
      "id": "F1",
      "severity": "HIGH",
      "confidence": "high",
      "rule": "FS5; result provenance principle",
      "status": "applied",
      "change": 1
    },
    {
      "id": "F2",
      "severity": "HIGH",
      "confidence": "high",
      "rule": "FS7; file ownership principle",
      "status": "applied",
      "change": 2
    },
    {
      "id": "F3",
      "severity": "HIGH",
      "confidence": "high",
      "rule": "FS4; DAT1",
      "status": "applied",
      "change": 3
    },
    {
      "id": "F4",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "FS7",
      "status": "applied",
      "change": 4
    },
    {
      "id": "F5",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "FS5; process argument handling principle",
      "status": "applied",
      "change": 5
    },
    {
      "id": "F6",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "DOC5",
      "proposed_rule": true,
      "status": "applied",
      "change": 6
    },
    {
      "id": "F7",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "PLT2; DOC5",
      "proposed_rule": true,
      "status": "applied",
      "change": 7
    },
    {
      "id": "F8",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "DEP1; FS5",
      "status": "applied",
      "change": 8
    },
    {
      "id": "F9",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "VRB1; FS9",
      "status": "applied",
      "change": 9
    },
    {
      "id": "F10",
      "severity": "LOW",
      "confidence": "high",
      "rule": "DOC5",
      "proposed_rule": true,
      "status": "applied",
      "change": 10
    },
    {
      "id": "A1",
      "severity": "LOW",
      "confidence": "high",
      "rule": "FS3 (catalogue edge case)",
      "status": "pending",
      "change": 11
    }
  ],
  "coverage_skipped": [
    "Native Windows/Linux execution",
    "FBM and large-memory fixtures",
    "MCMC convergence and biological inference",
    "Visual/geospatial validation of rendered surfaces",
    "Full package check"
  ],
  "status": "approved-for-publication",
  "pr": null,
  "verification": {
    "regression_assertions": 53,
    "baseline_assertions": 8,
    "package_suite": "passed",
    "seeded_trace_vs_original": "byte-identical",
    "snapshot_changes": {
      "testset.gl": "F1",
      "testset.gs": "F3"
    }
  }
}
```
