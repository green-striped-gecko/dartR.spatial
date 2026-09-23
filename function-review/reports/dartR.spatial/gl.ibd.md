# Review: gl.ibd (dartR.spatial)

## 1. Header

- Family mode: analysis.
- Date: 2026-09-22.
- Reviewer: Codex (GPT-6; exact variant not available in session metadata), dartr-function-review v2.0.0.
- Package commit: `15a4c7c51f51d153add072324ccf78917601cf17`.
- Reviewed source: `R/gl.ibd.r`; SHA256 `e98fa6b6fb40b3be383be57b56aa35616528a4fe26daf1fcc6bcf45736bb17cc`.
- Datasets: `testset.gl`, `testset.gs`, `bandicoot.gl`, labelled synthetic distances/coordinates and a small FBM-backed bandicoot fixture.
- Baseline: [test-gl.ibd.R](../../../tests/testthat/test-gl.ibd.R), captured before critical source review. Eight scenarios cover SNP/SilicoDArT, Euclidean/Fst and plotting on/off. Initial snapshots were extended with the observed lowercase `mantel` component before source review.
- Runtime: R 4.4.2 on macOS arm64; dartR.base 1.2.3, dartR.data 1.2.5, vegan 2.7.3, StAMPP 1.6.3, dismo 1.3.16, ggplot2 4.0.2, testthat 3.3.2.
- Workflow: `dev_luis` was fast-forwarded to `origin/dev`, which now includes merged PR #36. At the start of Phase A, the reviewed function had no local changes. No other claim for `gl.ibd` was found in the package or supplied campaign manifest; claimed locally for Luis. Unrelated changes and EEMS review records were preserved.

## 2. Verdict

Initial Phase A verdicts follow. All nine approved findings are now implemented; verification is recorded in section 7. Finding locations refer to the reviewed commit, before implementation.

**Standards: Needs work** — input checks do not enforce sample correspondence, complete coordinates or valid distance inputs; plotting and messaging also need bounded fixes.

**Spec: Needs work** — reordering labelled inputs changes the statistical result, and the documented default transformations and Euclidean implementation do not match the code.

On complete, aligned inputs, the Euclidean calculation and a Mantel test with an explicit permutation matrix matched independent calculations exactly. All five distance routes completed on a small bandicoot fixture.

## 3. Findings

**F1 [HIGH, confidence: high] — labelled observations are compared by position (FS5; sample-correspondence principle)**

`R/gl.ibd.r:157`–158 converts supplied matrices without matching their labels; lines 322–328 only sort population matrices independently. Lines 197–205 also accept an explicitly named coordinate table in its supplied row order.
Failure scenario: two identical six-sample distance matrices give Mantel `r = 1`, `p = 0.05`. Reordering one matrix while keeping its row/column labels gives `r = -0.3290398`, `p = 0.95`. Replacing label F with Z is accepted despite different sample sets. Reordering a named coordinate table changes the bandicoot result from `r = -0.4720980` to `-0.1016332`.
Proposed change: validate unique matching identities and align matrices and explicitly named coordinates before pairing observations. Document positional order for unlabelled inputs. The catalogue has no dedicated distance-matrix identity rule; this is the same correspondence concern that DAT2 addresses for genotype metadata.

**F2 [HIGH, confidence: high] — incomplete coordinates produce invented geographic distances (FS5, DAT5)**

`R/gl.ibd.r:221`–225 checks row count but not finite values. Lines 249–252 average each coordinate independently with `na.rm = TRUE`; line 264 passes incomplete coordinates to `dist()`.
Failure scenario: sample A is at `(0,0)` and C at `(4,0)`. Making C's x coordinate missing changes their returned distance from 4 to 0 without an error. The synthetic-coordinate bandicoot Mantel result changes from `r = -0.4720980` to `-0.2875815`.
Proposed change: require two numeric, finite coordinates per used individual before projection/aggregation, and finite projected coordinates afterwards. Identify affected individuals in the error; do not infer a missing coordinate from the remaining axis.

**F3 [HIGH, confidence: high] — distance representations and statistical preconditions are unchecked (FS5)**

`R/gl.ibd.r:338`–363 evaluates transformations and starts the Mantel test after checking only infinite geographic values and NULL objects. Lines 387–388 then assume both objects are lower-triangle vectors.
Failure scenarios: a valid square matrix supplied as Dgen alongside x, or returned by `Dgen_trans = "as.matrix(Dgen)"`, reaches Mantel but fails afterwards with `arguments imply differing number of rows: 36, 15`. Two samples return `r = NA, p = NA`; constant distances do likewise. Infinite genetic distance fails later in plot regression. Missing distances are accepted through `na.rm = TRUE`, including without a plot to reveal omitted pairs. A bare call or a single distance without x fails with `object 'ta' not found`; an unknown distance name gives `argument is of length zero`.
Proposed change: validate the input mode and method first; normalise supported matrix/dist inputs and transformation results to a common dist representation; check sizes, finite off-diagonal values, at least three units and variation in both vectors before testing. Reject incomplete distances with an actionable explanation rather than silently dropping pairs. Retain valid negative Fst estimates and finite transformed values: this is not a blanket nonnegative-distance restriction.

**F4 [MEDIUM, confidence: high] — supplying both matrices does not actually ignore x (FS5; DOC5, proposed rule)**

`R/gl.ibd.r:147`–164 selects matrix mode, then overwrites it when x is a genlight. The population/coordinate checks still run even though both distances are supplied.
Failure scenario: the complete six-sample matrices above run by themselves, but adding a genlight with one population triggers the population-count error. The function's own message says x is ignored.
Proposed change: make the mode selection exclusive. When both matrices are supplied, do not validate or inspect the unused genlight; when one distance must be calculated, validate only the necessary genlight inputs.

**F5 [MEDIUM, confidence: high] — disabling plotting still constructs and fits the plot (PLT3)**

`R/gl.ibd.r:365`–443 builds the regression and ggplot regardless of `plot.out`; the flag at line 445 only suppresses printing.
Failure scenario: identical complete distance matrices at `plot.out = FALSE` emit a perfect-fit regression warning even though the Mantel result is valid. A deliberately injected plot-construction error prevents that same no-plot analysis from returning, confirming the control-flow coupling. This injection is a test of the failure path, not a claim that ordinary plotting always fails.
Proposed change: build the plot only when displaying or saving it. A call with `plot.out = FALSE` and `plot.file = NULL` should return distances and Mantel results without invoking plotting or its regression.

**F6 [MEDIUM, confidence: high] — documented transformations and Euclidean semantics are inaccurate (DOC5, proposed rule)**

`R/gl.ibd.r:14`–15 promises Fst/(1−Fst) against log(distance), but lines 105–106 default to identity transformations. Line 19 attributes the Euclidean option to gl.dist.ind, while line 316 uses stats::dist.
Failure scenario: the default population result equals untransformed StAMPP Fst. On `testset.gl[1:8, 1:60]`, the first Euclidean distance is 1.490711985 here but 1.414213562 in gl.dist.ind: stats::dist rescales for the fraction of loci compared. The maximum discrepancy is 0.1420804814. The docs also describe paircols' default as 'pop' rather than NULL, plot.out as returning a plot rather than displaying it, and the result component as Mantel rather than the actual lowercase mantel.
Proposed change: document the current identity defaults, explicit transformations, stats::dist missing-data scaling, display/save behaviour and actual result names. Preserve current numerical defaults. The documentation change does not assign a custodian or alter statistical defaults.

**F7 [LOW, confidence: high] — the dependency guard returns a number on failure (FS5)**

`R/gl.ibd.r:115`–123 prints a missing-dismo message and returns `-1`.
Failure scenario: a controlled unavailable-package check returns numeric `-1`, so an error handler does not detect the failed analysis. Dismo is already an Imports dependency; this is an injected guard test, not a claim that a correctly installed package currently lacks it.
Proposed change: raise an actionable error where the dependency is needed. Precomputed distances and projected xy coordinates do not require Mercator conversion. DEP1 applies specifically to Suggests, so it is not used to misclassify this Imports dependency.

**F8 [LOW, confidence: high] — verbosity is not propagated or gated consistently (VRB1)**

`R/gl.ibd.r:291`, 304 and 319 omit verbosity when calling conversion/Kosman helpers. Lines 449–454 print the full Mantel summary at verbosity 1.
Failure scenario: `distance = "kosman", verbose = 0` prints `Starting gl.kosman`; the FBM/Fst path prints conversion start/end messages. Small-distance calls also expose routine permutation-enumeration messages. Level 1 prints transformations and the complete results summary rather than just start/end.
Proposed change: forward verbosity to helpers, suppress routine informational output at 0, and gate progress/results at their documented levels. Fatal errors must remain visible; do not disguise invalid analyses as quiet successes.

**F9 [LOW, confidence: high] — function-object invocation breaks the start message (FS3, catalogue edge case)**

`R/gl.ibd.r:135`–138 passes the call head directly to the formatter.
Failure scenario: `do.call(gl.ibd, list(Dgen = D, Dgeo = D, plot.out = FALSE, verbose = 1))` fails with `cannot coerce type 'closure' to vector of type 'character'`. Invocation by name works.
Proposed change: use the stable function name when the call head is a function object, retaining the existing label for ordinary calls. The catalogue's match.call idiom needs this exception. This is included in the current review, not left for a later implementation addendum.

## 4. Proposed changes

1. Align and validate labelled matrices and coordinates (F1). **Consequence: numerical results change for currently misaligned inputs; mismatched/duplicate identities error. Unlabelled inputs retain documented positional order.**
2. Reject incomplete or non-finite coordinates (F2). **Consequence: calls that previously manufactured geographic distances from partial coordinates now error.**
3. Validate modes, methods, distance shapes and statistical preconditions (F3). **Consequence: supported square matrices work consistently; missing/non-finite distances, too few units, constant vectors and unsupported configurations error before testing. No pairs or individuals are silently dropped.**
4. Honour supplied-matrix precedence (F4). **Consequence: a supplied x is ignored when both matrices are supplied, including its population count and coordinates.**
5. Skip unused plot construction (F5). **Consequence: plotting is not invoked when both display and saving are disabled; numerical results and the three-component return shape remain unchanged.**
6. Correct the documentation while retaining numerical defaults (F6; documentation only).
7. Raise dependency errors where needed (F7). **Consequence: a missing required projection dependency errors rather than returning -1; paths not using projection do not run that guard.**
8. Honour verbosity across the wrapper and helpers (F8). **Consequence: routine output is suppressed at 0 and full results summaries move to the appropriate higher verbosity. Numerical results are unchanged.**
9. Handle function-object call labels (F9). **Consequence: function-object do.call invocations work at verbosity > 0; numerical results are unchanged.**

## 5. Coverage

- **Baseline:** [test and eight snapshots](../../../tests/testthat/test-gl.ibd.R) record dimensions, ploidy, metric row counts, six distances, Mantel statistic/p-value and unchanged input serialization. **16 checks passed.** Four plotting warnings expose omitted non-finite Fst pairs (one in the SNP subset and 15 in the SilicoDArT subset); these warnings are evidence, not hidden test failures. [Baseline log](../../evidence/gl.ibd-baseline.log).
- **Targeted tests:** [probe script](../../evidence/gl.ibd-probes.R), [facts](../../evidence/gl.ibd-probes.json), [log](../../evidence/gl.ibd-probes.log). All five methods completed on 16 bandicoots across four populations, using 100 loci. Label permutation, different sample sets, missing coordinates, supplied/hybrid matrices, matrix-valued transformations, too few units, constant/non-finite distances, no inputs, bad method names and verbosity were exercised.
- **Independent calculations:** the Euclidean pairwise sum of squares, adjusted for compared loci, agreed exactly. Independently computed Pearson correlation and a one-sided permutation p-value using an explicit 19-row permutation matrix both agreed exactly (`r = -0.0980001748`, `p = 0.55`). Numerical engines for population Fst/Nei D were exercised but not independently rederived; they delegate to StAMPP.
- **FBM:** a small converted six-bandicoot fixture matched ordinary-genlight genetic distances for Euclidean and Fst, and the source genotype matrix was unchanged. Dense materialisation is still used; large-memory behaviour was not tested. No claim of scalable FBM processing is made.
- **Standards walk:** FS, DOC, VRB, DAT, DEP, PLT, STY reviewed. The caller's object is preserved, no inappropriate history entry is returned, explicit output is present, ggplot/theme and utils.plot.save are already used. Phase A did not change production code or roxygen; Phase C regeneration and package-test results are recorded below. Formatting, unused imports and other checks handled by R CMD check were not promoted into findings. Missing custodian metadata under proposed DOC7 is a catalogue note; the role was not inferred from author names.
- **External verification:** [vegan's Mantel documentation](https://vegandevs.github.io/vegan/reference/mantel.html) confirms row/column permutation and warns that missing-value removal can bias permutation tests. A [2021 dartR Group report](https://groups.google.com/g/dartr/c/lULh7pMe3vM) concerned a now-removed KDE failure; it is not asserted to persist. A [2020 small-population report](https://groups.google.com/g/dartr/c/m9OW1BHFHAs) concerns permutation enumeration, which is expected for few populations and is not itself a defect. No matching dartR.spatial GitHub issue was found.
- **Skipped/limits:** native Windows/Linux execution; large FBM/memory workloads; independent derivation of StAMPP estimators; validation of biological interpretation or geographic projection choice; exhaustive visual assessment. A missing @other$latlon entry alone was not treated as a failure because existing metadata access recovered coordinates on the fixture. Asymmetric or mismatched-size inputs are proposed validation cases; the recorded wrong-result demonstration specifically uses symmetric, equal-sized matrices with misordered labels.
- **Before a behaviour-changing merge:** inspect sibling/dartr2shiny callers, add NEWS and map every baseline difference to an approved change under API1–API3. Completed in Phase C; see the caller review and NEWS below.

Verify the implemented version from `/Users/mijangos/dartR.spatial` (the Phase A probe script records behaviour at the reviewed commit):

```sh
NOT_CRAN=true /Library/Frameworks/R.framework/Resources/bin/Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-gl.ibd.R")'
/Library/Frameworks/R.framework/Resources/bin/Rscript function-review/evidence/gl.ibd-verification.R
```

## 6. Approval

| Change | Decision | By | Note |
|---|---|---|---|
| 1–9 | approved | Luis, 2026-09-22 | “go ahead with all the fixes”; approves the stated consequences of all nine changes. |

## 7. Outcome

All nine approved changes are implemented. No signature or statistical-default changes. The function now aligns identities before Mantel testing, rejects incomplete inputs, honours supplied-distance precedence, and constructs plots only for display or saving. Verbosity and function-object calls work as documented. The manual was regenerated with `devtools::document()`; no unrelated generated files changed.

### Verification and baseline accounting

- **95 gl.ibd assertions passed:** 79 regression assertions plus 16 characterisation assertions; zero test warnings/failures. [Regression tests](../../../tests/testthat/test-gl.ibd-fixes.R).
- **183 package assertions passed**, with two opt-in EEMS integration tests skipped because this change does not touch EEMS and `DARTR_EEMS_REVIEW` was not enabled. [Suite log](../../evidence/gl.ibd-suite.log), [per-test counts](../../evidence/gl.ibd-suite-counts.csv).
- **F1:** reordered six-sample geographic input now gives `r = 1`, `p = 0.05`, previously `r = -0.3290398`, `p = 0.95`. Reordered explicit coordinate rows match the reference `r = -0.4720980414931`, with zero geographic-distance difference.
- **F2:** sample C's missing coordinate now raises `Non-finite coordinates for individuals: C.` rather than turning the A-C distance from 4 into 0. Invalid coordinates are rejected before aggregation and after projection. Stored coordinates remain positional, preserving the packaged bandicoot data's existing metadata convention; explicit named tables are matched to individual names.
- **F3:** exactly four baseline snapshots changed: Fst on `testset.gl` and `testset.gs`, each with plotting on/off, now errors on incomplete pairwise distances. The original inputs contained one and 15 missing Fst pairs respectively. This is the approved replacement of silent omission. All four Euclidean snapshots are unchanged. [Original snapshots](../../evidence/gl.ibd-before-snapshots.md).
- **F4–F9:** regression checks cover unused x/coordinates, matrix-valued transformations, plot display/save/no-plot routes, unavailable projection dependency, quiet helper calls, function-object invocation, and malformed/statistically undefined distances. Negative finite distances remain accepted.
- **Numerical preservation:** all five methods on the original complete bandicoot fixture have exactly the same genetic/geographic distances, Mantel statistics and p-values as before. The independently computed Euclidean distances and explicit-permutation Mantel correlation/p-value also agree. [Original method values](../../evidence/gl.ibd-before-methods.R), [verification script](../../evidence/gl.ibd-verification.R), [facts](../../evidence/gl.ibd-verification.json), [verbose-3 run log](../../evidence/gl.ibd-verification.log).
- **Documentation:** all three documented examples completed using 19 permutations, including display. [Roxygen log](../../evidence/gl.ibd-document.log), [manual](../../../man/gl.ibd.Rd).
- **FBM:** ordinary and small file-backed fixtures agree for Fst and Euclidean distances; input genotypes remain unchanged and verbosity 0 is quiet.

### Verification-record correction before publication

The first text export of the original binary fixture used default `dput()` precision. It rounded the Fst fixture's geographic distances by up to `4.889444e-09`, causing the subsequent zero-tolerance comparison to fail. The previous chat summary missed that failed rerun; its passing claim relied on the earlier binary-fixture comparison. Production calculations and the package tests were unaffected. The text fixture now uses hexadecimal numeric literals, and its round-trip is identical to the original binary values. The verification script was rerun successfully: all five methods have zero differences, verbose-3 plotting completes, and all three examples complete. [Export check](../../evidence/gl.ibd-fixture-export.log), [successful rerun](../../evidence/gl.ibd-verification.log).

### Downstream impact

[Caller scan](../../evidence/gl.ibd-callers.log) searched R/tests under local `dartR.*` siblings and the `dartr2shiny` generator/runtime. No sibling-package runtime calls were found. `dartr2shiny/shiny_fun/Fun_gl.ibd.R:153` calls the unchanged signature and already passes errors through `run_data()`, which displays an error and returns NULL. Its generated command at line 182 also uses the unchanged signature. Existing invalid datasets will now show the deliberate validation error. `input_generator/dartR.spatial/gl.ibd.r` is a copied function definition with older documentation; no generator or application files were edited. [NEWS](../../../NEWS.md) records the changed input handling and preserved defaults.

### Limits and publication

Native Windows/Linux execution, large FBM workloads, independent derivation of StAMPP estimators and exhaustive visual/biological interpretation remain untested. The full package test suite ran; a full R CMD check was not run for this scoped change. Existing dependency build-version notices remain in startup logs, separate from test warnings.

Package and campaign manifests record `awaiting-commit`. Luis approved the displayed commit message and PR title with “ok” on 2026-09-22. Commit, push and PR creation are authorised; the PR number will be recorded after creation. The unrelated EEMS addendum remains outside this approval and unchanged.

## 8. Machine block

```json
{
  "function": "gl.ibd",
  "package": "dartR.spatial",
  "family": "analysis",
  "skill_version": "2.0.0",
  "model": "GPT-6; exact variant not available in session metadata",
  "commit": "15a4c7c51f51d153add072324ccf78917601cf17",
  "datasets": [
    "testset.gl",
    "testset.gs",
    "bandicoot.gl",
    "labelled synthetic distances and coordinates",
    "small bandicoot FBM fixture"
  ],
  "verdict_standards": "needs_work",
  "verdict_spec": "needs_work",
  "findings": [
    {
      "id": "F1",
      "severity": "HIGH",
      "confidence": "high",
      "rule": "FS5; sample-correspondence principle",
      "status": "applied",
      "change": 1
    },
    {
      "id": "F2",
      "severity": "HIGH",
      "confidence": "high",
      "rule": "FS5; DAT5",
      "status": "applied",
      "change": 2
    },
    {
      "id": "F3",
      "severity": "HIGH",
      "confidence": "high",
      "rule": "FS5",
      "status": "applied",
      "change": 3
    },
    {
      "id": "F4",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "FS5; DOC5 (proposed)",
      "status": "applied",
      "change": 4
    },
    {
      "id": "F5",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "PLT3",
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
      "severity": "LOW",
      "confidence": "high",
      "rule": "FS5",
      "status": "applied",
      "change": 7
    },
    {
      "id": "F8",
      "severity": "LOW",
      "confidence": "high",
      "rule": "VRB1",
      "status": "applied",
      "change": 8
    },
    {
      "id": "F9",
      "severity": "LOW",
      "confidence": "high",
      "rule": "FS3 (catalogue edge case)",
      "status": "applied",
      "change": 9
    }
  ],
  "coverage_skipped": [
    "Native Windows/Linux execution",
    "Large FBM/memory workloads",
    "Independent derivation of StAMPP estimators",
    "Biological/geographic interpretation",
    "Exhaustive visual assessment",
    "Full R CMD check; scoped validation used the package test suite",
    "Two opt-in EEMS integration tests; EEMS is unchanged"
  ],
  "baseline": {
    "assertions_passed": 16,
    "warnings": 4,
    "failures": 0
  },
  "status": "awaiting-commit",
  "pr": null,
  "verification": {
    "gl_ibd_assertions_passed": 95,
    "package_assertions_passed": 183,
    "test_failures": 0,
    "test_warnings": 0,
    "skipped_tests": 2,
    "changed_snapshots": {
      "F3": 4
    },
    "unchanged_euclidean_snapshots": 4,
    "all_five_method_numerics_unchanged": true,
    "documented_examples_completed": 3,
    "verbose_3_completed": true
  }
}
```
