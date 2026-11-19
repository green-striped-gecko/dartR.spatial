# Review: gl.costdistances (dartR.spatial)

## 1. Header

- Family mode: analysis.
- Date: 2026-09-23.
- Reviewer: Codex (GPT-6; exact variant not available in session metadata), dartr-function-review v2.0.0.
- Package commit: `0bbc487947500b18a6adc2863e9c80e3499f72e9`.
- Source SHA256: `078ddd5cee977c3c8591091f1232f7354dc748c611c199b684af3f3b0fcd80be`.
- Datasets: `testset.gl`, `testset.gs`, `possums.gl`, a small bandicoot FBM fixture, and synthetic projected/geographic resistance rasters.
- Baseline: [test-gl.costdistances.R](../../../tests/testthat/test-gl.costdistances.R), written and run before critical source review. Twelve snapshots cover both reference datasets, all three methods and genlight/matrix location inputs. Six individuals and 20 loci per dataset, three populations, uniform 8-by-8 geographic rasters, NN = 8.
- Environment: R 4.4.2 on macOS arm64; gdistance 1.6.5, raster 3.6.32, sp 2.2.1, igraph 2.2.2, Matrix 1.7.4, terra 1.9.1, testthat 3.3.2. See [runtime record](../../evidence/gl.costdistances-environment.log) for exact installed versions.
- Workflow: `dev_luis` fast-forwarded to `origin/dev`, including merged PR #37. No existing claim for this function was found; package, dartR.base campaign and dartRverse campaign manifests now claim it for Luis. Unrelated work and existing review records were preserved.

## 2. Verdict

These are the original Phase A verdicts. Implementation and verification are recorded in section 7.

**Standards: Needs work** — coordinates and raster inputs are insufficiently validated, the dependency guard returns a number, and verbosity/call-label handling are inconsistent.

**Spec: Needs work** — edge costs depend on an arbitrary ordering of cell values, genlight coordinate handling fails on valid inputs, and geographic commute/RSP calculations need explicit conventions and numerical checks.

Uniform projected least-cost distances match an independent shortest-path calculation exactly; commute times match an independent graph-Laplacian calculation within `2.14e-14`. The documented possum example completes, and small FBM and ordinary-genlight results agree.

## 3. Findings

**F1 [HIGH, confidence: high] — symmetric transitions use a noncommutative resistance function (numerical correctness; FS5)**

`R/gl.costdistances.r:84`–86 supplies `1 / x[2]` while gdistance's default transition matrix is symmetric. The function depends on which cell happens to be second, rather than a symmetric property of the two cells.
Failure scenario: a one-row, three-cell raster with resistance `1, 9, 1` gives neighbouring least-cost distances 9 and 1. The landscape is mirror-symmetric. The conventional arithmetic-mean edge resistance gives 5 and 5; endpoint distance is 10 in both cases. Independent arithmetic and a separate gdistance transition using `1 / mean(x)` agree.
Proposed change: explicitly use symmetric transitions with conductance `1 / mean(x)`. This adopts a stated edge model; other symmetric models exist, so the numerical consequence needs approval rather than being presented as a cosmetic correction.

**F2 [HIGH, confidence: high] — genlight coordinates use column order and are not transformed to the raster CRS (FS5, DAT5; DOC5, proposed rule)**

`R/gl.costdistances.r:77`–79 averages all coordinate columns in their stored order. Lines 90–97 then treat the result as x/y coordinates in the raster's coordinate reference system (CRS).
Failure scenario: `testset.gl` and `testset.gs` store columns as lat, lon. All six genlight baseline cases error after coordinate omission warnings, while explicit lon/lat population-centre matrices reach the calculations. Reordering the same synthetic-population genlight's metadata to lon, lat makes least-cost analysis complete. A projected raster still fails because geographic coordinates are used directly without transformation.
Proposed change: select longitude and latitude by name, calculate the documented population centres, and transform those centres from explicitly documented WGS84 longitude/latitude to the raster CRS. Numeric matrices/data.frames remain x/y in the raster CRS, with column order documented. Do not infer arbitrary source projections or rewrite the caller's metadata.

**F3 [HIGH, confidence: high] — invalid locations can return plausible but wrongly labelled distances (FS5, DAT5)**

`R/gl.costdistances.r:56`–82 and 90–99 delegate invalid inputs without checking retained point identities or output correspondence.
Failure scenario: replacing location B by an NA coordinate or an out-of-raster coordinate returns a labelled 3-by-3 matrix with A-B = 0 in one direction and B-A = 3 in the other, plus recycling warnings. A genlight individual with missing population membership is silently excluded from population means. Unknown method returns `object 'cd.mat' not found`; nonnumeric coordinates trigger a low-level C++ conversion error. Negative resistance reaches Dijkstra's internal error, and missing raster CRS is accepted.
Proposed change: validate the method, gdistance-supported neighbourhood specification, single-layer raster, a known CRS when transforming genlight coordinates, coordinate shape/type/finiteness, population membership, unique supplied labels and raster-cell membership before computing. Require strictly positive finite traversable resistance; define NA and positive Inf as barriers, reject zero/negative values and points on barriers. Preserve one-point results and require at least one point. Explicit x/y matrices on rasters without a CRS remain supported in local grid units, as used by the packaged synthetic example; do not infer metres. Do not silently omit individuals or locations. Preserve Inf for disconnected least-cost pairs; explain unsupported disconnected random-walk cases with an error rather than an opaque solver failure.

**F4 [HIGH, confidence: high] — geographic commute distances use least-cost correction (numerical correctness; DOC5, proposed rule)**

`R/gl.costdistances.r:87`–89 changes only the PROJ text to Mercator and calls `geoCorrection()` without selecting the random-walk correction. Gdistance defaults to type c; its documentation specifies the additional north-south correction for random walks.
Failure scenario: on a uniform 4-by-4 longitude/latitude raster spanning 40–80 degrees north, one commute pair is 41.03418 with this wrapper and 71.40384 with `geoCorrection(type = "r")`; another is 67.22255 versus 109.11881. Both use the same transition and commute engine.
Proposed change: preserve the raster's CRS metadata; use type c for least-cost, type r for commute, and explicitly document type c for the existing theta = 1 RSP convention. Gdistance does not provide a unique geographic correction for intermediate RSP regimes; no claim of a universally correct RSP geographic model is made.
False-positive boundary: the PROJ-text overwrite did **not** cause a metre-conversion error in the installed stack, even after removing the input raster's WKT comment. Geographic equatorial cells correctly returned 111319.490793 metres. A units-error claim is therefore not made; the demonstrated defect is the commute correction choice.

**F5 [MEDIUM, confidence: high] — fixed RSP theta silently returns NaN on connected landscapes (FS5; numerical correctness)**

`R/gl.costdistances.r:92`–94 hard-codes theta = 1 and returns the engine result without checking it.
Failure scenario: a connected, uniform 3-by-3 projected raster with 1000-metre cells returns NaN for every off-diagonal pair without warning. The same engine with theta = 0.001 gives finite values (for example 2047.242975 and 3046.177368). The two matrix-input RSP baseline cases also return NaN.
Proposed change: add optional `theta = 1` after the existing arguments, pass it to gdistance, validate its supported range and reject unexpected NA/NaN/non-finite RSP results with guidance about theta and scale. Preserve the default and do not silently rescale costs or change theta. A smaller theta changes the path model, so this remains a caller decision.

**F6 [MEDIUM, confidence: high] — commute fails when a location is the grounded graph cell (dependency integration; FS5)**

`R/gl.costdistances.r:95`–97 delegates to gdistance 1.6.5 without handling a reproducible solver limitation.
Failure scenario: on a connected uniform 3-by-3 raster, locations in cells 1, 5 and 9 produce `subscript out of bounds`. Using cell 8 instead of 9 succeeds. Calling gdistance::commuteDistance directly reproduces the error: its reduced Laplacian removes the final row, then attempts to index that row for the selected location. This is an upstream defect, not a defect introduced by dartR's matrix conversion.
Proposed change: use a tested sparse grounded-Laplacian commute calculation that supports the grounded cell, retaining graph-volume times effective-resistance scaling and original point order. Use selected right-hand sides rather than a dense all-cell pseudoinverse. Disconnected graphs must be diagnosed explicitly. This is a bounded compatibility implementation, not a dependency-version guess or an upstream package edit.

**F7 [MEDIUM, confidence: high] — method semantics and units are underspecified (DOC1/DOC2; DOC5, proposed rule)**

`R/gl.costdistances.r:3`–17 calls commute “Circuitscape type” without distinguishing commute time from effective resistance; omits coordinate-column/CRS requirements, resistance-to-edge conversion, barriers, RSP theta, and output units (including local grid units for the unreferenced synthetic example). Verbosity documentation promises results summaries which are not produced.
Failure scenario: the independent uniform graph gives effective resistance 1.2083333 but the wrapper returns commute time 29, a factor of the graph volume 24. Those quantities cannot be interchanged across landscapes with different graph volumes. The current examples alone do not explain this distinction.
Proposed change: document the actual three methods and their units, commute-time scaling, coordinate assumptions, neighbourhoods, barrier/disconnected conventions and RSP parameter/scale interaction. Add the required structural documentation tags and accurate verbosity text. Keep `commute` as commute time; do not silently rename it or change it to effective resistance. Author/custodian metadata is absent and cannot be inferred from the old monolithic package's manual; obtain confirmed attribution before adding names.

**F8 [LOW, confidence: high] — missing optional dependency returns -1 (DEP1, FS5)**

`R/gl.costdistances.r:45`–53 prints the missing-gdistance message and returns numeric -1.
Failure scenario: an injected unavailable-namespace check returns -1 rather than signalling failure to an error handler. Gdistance is in Suggests.
Proposed change: retain the dependency guard but raise an actionable error at all verbosity levels.

**F9 [LOW, confidence: high] — verbosity 0 still prints population fallback messages (VRB1, VRB3)**

`R/gl.costdistances.r:65`–71 prints the no-population message unconditionally. At levels 2 and 3 the wrapper otherwise only prints start/end.
Failure scenario: removing population assignments from a valid six-individual genlight prints two fallback-message lines at verbose = 0, although individual distances are returned.
Proposed change: gate routine fallback/progress at level 2 and provide a concise calculation/results summary at level 3. Preserve the existing no-population fallback and visible fatal errors; do not change numerical output.

**F10 [LOW, confidence: high] — function-object calls fail in the start message (FS3, catalogue edge case)**

`R/gl.costdistances.r:40`–43 passes a closure from the call head into the message formatter.
Failure scenario: `do.call(gl.costdistances, list(..., verbose = 1))` errors with `cannot coerce type 'closure' to vector of type 'character'` before computation. Invocation by name works.
Proposed change: use a stable gl.costdistances label when the call head is a function object, retaining ordinary call labels. This is the same catalogue exception established by the preceding reviews.

**F11 [LOW, confidence: high] — legacy test comparator crashes after geographic dependencies load (TST3; addendum)**

`tests/testthat/test-gl.ibd-fixes.R` uses testthat's default edition-2 comparison engine. Geographic cost-distance calculations load proxy, which registers `names.dist` returning a list of dimension labels. Base R's all.equal.character then attempts to compare lists as strings and errors in three existing gl.ibd assertions. This is a test-comparison dependency interaction, not a changed distance value. The original gl.ibd suite passed before those dependencies were loaded.
Proposed change: add `local_edition(3)` to the gl.ibd regression file, keeping all numerical and structural assertions intact. No production changes. A disposable copy with that one-line addition passes all 79 gl.ibd assertions after explicitly loading proxy; the original test file was unchanged during that diagnostic. [Candidate check](../../evidence/gl.costdistances-comparator-probe.R), [log](../../evidence/gl.costdistances-comparator-probe.log). Luis approved F11 on 2026-09-23 (“go ahead with F11”). The one-line addition is now applied; the complete suite passes.

## 4. Proposed changes

1. Adopt arithmetic-mean resistance on symmetric edges (F1). **Consequence: numerical distances change on heterogeneous landscapes; the 1/9/1 example changes its neighbouring costs from 9/1 to 5/5.**
2. Normalise genlight longitude/latitude and transform population centres to the raster CRS (F2). **Consequence: documented genlight input works regardless of lat/lon column order and on projected rasters; previously misinterpreted coordinates give different results. WGS84 is the explicit source assumption.**
3. Validate observations, rasters, resistance and method settings before calculation (F3). **Consequence: incomplete/outside/barrier locations, missing population assignments, duplicate labels, unknown CRS for genlight input and unsupported configurations now error; zero/negative traversable resistance is rejected. No observations are dropped. Legitimately disconnected least-cost pairs retain Inf.**
4. Preserve raster CRS and select the geographic correction by method (F4). **Consequence: geographic commute results change under random-walk correction; least-cost and the stated default RSP correction use type c.**
5. Expose optional theta and check RSP numerical results (F5). **Consequence: a new optional argument is added after existing arguments; theta stays 1 by default, but invalid numerical results error rather than returning NaN. User-selected theta changes results.**
6. Support grounded-cell locations in the commute solver (F6). **Consequence: valid inputs that currently fail return commute times; successful connected cases must match the independent reference within numerical tolerance.**
7. Correct and complete method/coordinate/units documentation (F7). **Documentation only: do not change commute into effective resistance or invent attribution.**
8. Raise missing-dependency errors (F8). **Consequence: callers receive an error instead of numeric -1.**
9. Honour the documented verbosity levels (F9). **Consequence: routine messages disappear at 0 and concise progress/results appear at 2/3; calculations are unchanged.**
10. Handle function-object invocation (F10). **Consequence: do.call with a function object works at verbosity above 0; calculations are unchanged.**

11. Use testthat edition 3 in the gl.ibd regression file (F11; addendum). **Test-only: comparison engine changes, assertions and function behaviour do not.**

## 5. Coverage

Phase A coverage follows; section 7 records subsequent implementation checks.

- **Baseline before review:** twelve snapshots; 24 assertions pass on repeat, with no test warnings/failures. Six genlight calls record errors; two matrix RSP calls record NaN; four least-cost/commute matrix calls return finite matrices. Warnings/output from the function are captured in the snapshots rather than hidden. [Initial capture](../../evidence/gl.costdistances-baseline.log), [confirmed baseline](../../evidence/gl.costdistances-baseline-confirmed.log).
- **Independent numerical checks:** a Floyd-Warshall all-pairs shortest-path computation matches uniform projected least-cost values exactly. A base-R eigen decomposition of the graph Laplacian independently gives effective resistance and commute times, agreeing within 2.14e-14 on successful selected cells. The heterogeneous three-cell example has an independent arithmetic reference and a separate gdistance mean-resistance reference. RSP was exercised at two scales/theta values but its full estimator was not independently rederived.
- **Probes:** all three methods; coordinate column order, projection mismatch, missing/outside points, duplicate labels, unknown method/NN, empty/one-point input, nonnumeric data, zero/negative/infinite/missing resistance, barriers/disconnection, missing CRS, multiple raster layers, dependency failure, verbosity and closure invocation. [Script](../../evidence/gl.costdistances-probes.R), [facts](../../evidence/gl.costdistances-probes.json), [log](../../evidence/gl.costdistances-probes.log).
- **Follow-up:** modern/legacy coordinate-system metadata, high-latitude random-walk correction, direct reproduction of the upstream grounded-cell failure and missing population assignments. [Script](../../evidence/gl.costdistances-followup.R), [facts](../../evidence/gl.costdistances-followup.json), [log](../../evidence/gl.costdistances-followup.log), [upstream engine](../../evidence/gl.costdistances-commute-engine.log).
- **Metadata/FBM:** reference inputs retain their serialised contents, ploidy and metadata row counts. A small FBM fixture agrees with ordinary genlight input. No genotype-matrix materialisation occurs in this wrapper; its scaling risk comes from the landscape graph and distance matrix, not FBM genotypes.
- **Standards walk:** FS, DOC, VRB, DAT, DEP, PLT, STY reviewed. There are no plots, file outputs, histories or returned modified genlights, so related rules are not applicable. Unused imports, style-only issues and checks already enforced by R CMD check are not promoted to defects. No production code/manual was changed.
- **External references:** gdistance's [transition contract](https://agrdatasci.github.io/gdistance/reference/transition.html) requires a commutative function for symmetric graphs; [geographic correction](https://agrdatasci.github.io/gdistance/reference/geoCorrection.html) distinguishes least-cost and random walks; [RSP parameters](https://agrdatasci.github.io/gdistance/reference/rSPDistance.html) define theta; [commute semantics](https://agrdatasci.github.io/gdistance/reference/commuteDistance-methods.html) distinguish graph-volume-scaled commute time from resistance. Searches found no matching dartR.spatial GitHub issue or indexed dartR Group complaint. Absence from search is not proof that no complaint exists.
- **Downstream reconnaissance:** no sibling R runtime caller found; `dartr2shiny/shiny_fun/Fun_gl.costdistances.R:143` calls the existing signature. Full compatibility assessment and NEWS are required before implementing/merging approved numerical or API changes.
- **Skipped/limits:** native Windows/Linux; large raster memory/performance workloads; full independent RSP derivation; biological suitability of arithmetic-mean resistance, WGS84 assumptions or projection choice; global/dateline raster behaviour; exhaustive custom neighbourhoods; full package suite/R CMD check and roxygen regeneration (production source unchanged). Proposed sparse-solver replacement is not yet implemented or validated at scale.

## 6. Approval

| Change | Decision | By | Note |
|---|---|---|---|
| 1–10 | approved | Luis, 2026-09-23 | “go ahead with all the fixes”; includes the stated numerical/API consequences. |
| 11 | approved | Luis, 2026-09-23 | “go ahead with F11”; one-line test-engine change, assertions unchanged. |

## 7. Outcome

All eleven approved changes are implemented, including the test-only F11 addendum. The optional theta argument is appended after verbose, preserving existing positional calls. Matrix and igraph are declared Imports for the sparse solver; terra is checked before coordinate transformation. No author/custodian names were invented because attribution was not supplied.

### Verification

- 101 targeted regression assertions pass, including independent shortest-path and eigen/Laplacian references, all selected grounded-cell positions, heterogeneous resistance, duplicate cell locations, scale invariance of commute time, coordinate projection, barriers, RSP failure/theta, dependency errors and verbosity. [Tests](../../../tests/testthat/test-gl.costdistances-fixes.R), [log](../../evidence/gl.costdistances-fixes.log), [counts](../../evidence/gl.costdistances-fixes-counts.csv).
- The 24 baseline assertions pass after accepting ten mapped snapshot changes: two genlight least-cost cases now return matrices (F2); two genlight commute cases now return matrices (F2/F4/F6); two matrix commute cases change under geographic random-walk correction (F4); four RSP cases now report an actionable numerical error (F2/F5). The two least-cost matrix snapshots are unchanged. [Original snapshots](../../evidence/gl.costdistances-before-snapshots.md).
- After the F11 test-engine change, the complete suite passes 308 assertions with zero test failures or warnings; two opt-in EEMS integration tests are skipped because DARTR_EEMS_REVIEW was not enabled. This includes 125 cost-distance assertions and 79 gl.ibd regression assertions. Package-startup notices about packages built under R 4.4.3 remain separate from test warnings. [Suite log](../../evidence/gl.costdistances-suite.log), [counts](../../evidence/gl.costdistances-suite-counts.csv).
- [Integration assertions](../../evidence/gl.costdistances-verification.R) pass at verbosity 3 for all three methods on reference genlight data, using explicitly selected theta = 1e-6 for the geographic RSP run. The 1/9/1 line gives edge costs 5/5 and commute times 4/8/4, agreeing with independent arithmetic. [Facts](../../evidence/gl.costdistances-verification.json), [log](../../evidence/gl.costdistances-verification.log).
- The documented possum example completes on its unreferenced 62,500-cell local-grid landscape. Its A-B least-cost value changes from 332.2497834 to 361.6995308 under approved mean resistance (F1). A 6,400-cell commute calculation with three locations, including the final cell, completes in 0.133 seconds on this machine; this is an observed runtime, not a scalability guarantee.
- Small SNP FBM input agrees with ordinary genlight and retains genotypes, ploidy and metadata. The installed gl.gen2fbm converter does not support SilicoDArT; ordinary SilicoDArT inputs are covered by the baseline. No converter changes were made.
- Roxygen regeneration changed only [gl.costdistances.Rd](../../../man/gl.costdistances.Rd), with [generation log](../../evidence/gl.costdistances-document.log). [NEWS](../../../NEWS.md) records numerical and validation changes.

### Compatibility and remaining limits

[Caller scan](../../evidence/gl.costdistances-callers.log) found no sibling-package R caller. The dartr2shiny runtime at Fun_gl.costdistances.R:143 and generated command use the original arguments, so appending theta preserves them. Its run_data handler displays new validation errors. The UI does not yet expose theta; adding that control is outside this function's approved scope. The generator's copied function is also unchanged.

The public gdistance documentation mentions custom neighbourhood matrices, but its installed transition implementation rejects them before construction. This wrapper now gives a clear supported-value error and retains working 4/8/16/bishop modes; no functioning custom-matrix route was removed.

Native Windows/Linux, substantially larger commute/RSP workloads, independent RSP estimator derivation, global/dateline behaviour and biological/projection modelling suitability remain untested. Full R CMD check was not run; package tests and the manual/examples were used for this scoped change. Implementation is complete. Luis approved the full commit message and PR title with “ok” on 2026-09-23. Publication is authorised; the PR number will be recorded after creation.

## 8. Machine block

```json
{
  "function": "gl.costdistances",
  "package": "dartR.spatial",
  "family": "analysis",
  "skill_version": "2.0.0",
  "model": "GPT-6; exact variant not available in session metadata",
  "commit": "0bbc487947500b18a6adc2863e9c80e3499f72e9",
  "datasets": [
    "testset.gl",
    "testset.gs",
    "possums.gl",
    "small bandicoot FBM fixture",
    "synthetic resistance rasters"
  ],
  "verdict_standards": "needs_work",
  "verdict_spec": "needs_work",
  "findings": [
    {
      "id": "F1",
      "severity": "HIGH",
      "confidence": "high",
      "rule": "numerical correctness; FS5",
      "status": "applied",
      "change": 1
    },
    {
      "id": "F2",
      "severity": "HIGH",
      "confidence": "high",
      "rule": "FS5; DAT5; DOC5 (proposed)",
      "status": "applied",
      "change": 2
    },
    {
      "id": "F3",
      "severity": "HIGH",
      "confidence": "high",
      "rule": "FS5; DAT5",
      "status": "applied",
      "change": 3
    },
    {
      "id": "F4",
      "severity": "HIGH",
      "confidence": "high",
      "rule": "numerical correctness; DOC5 (proposed)",
      "status": "applied",
      "change": 4
    },
    {
      "id": "F5",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "FS5; numerical correctness",
      "status": "applied",
      "change": 5
    },
    {
      "id": "F6",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "dependency integration; FS5",
      "status": "applied",
      "change": 6
    },
    {
      "id": "F7",
      "severity": "MEDIUM",
      "confidence": "high",
      "rule": "DOC1; DOC2; DOC5 (proposed)",
      "status": "applied",
      "change": 7
    },
    {
      "id": "F8",
      "severity": "LOW",
      "confidence": "high",
      "rule": "DEP1; FS5",
      "status": "applied",
      "change": 8
    },
    {
      "id": "F9",
      "severity": "LOW",
      "confidence": "high",
      "rule": "VRB1; VRB3",
      "status": "applied",
      "change": 9
    },
    {
      "id": "F10",
      "severity": "LOW",
      "confidence": "high",
      "rule": "FS3 (catalogue edge case)",
      "status": "applied",
      "change": 10
    },
    {
      "id": "F11",
      "severity": "LOW",
      "confidence": "high",
      "rule": "TST3; addendum",
      "status": "applied",
      "change": 11
    }
  ],
  "coverage_skipped": [
    "Native Windows/Linux",
    "Large raster memory/performance workloads",
    "Independent RSP estimator derivation",
    "Biological/geographic modelling suitability",
    "Global/dateline rasters and exhaustive custom neighbourhoods",
    "Full R CMD check; scoped package tests, documentation regeneration and examples were used",
    "Two opt-in EEMS external-tool tests; DARTR_EEMS_REVIEW was not enabled"
  ],
  "baseline": {
    "assertions_passed": 24,
    "test_warnings": 0,
    "failures": 0,
    "snapshots": 12
  },
  "status": "awaiting-commit",
  "pr": null,
  "verification": {
    "regression_assertions_passed": 101,
    "baseline_assertions_passed": 24,
    "changed_snapshots": 10,
    "unchanged_snapshots": 2,
    "integration_passed": true,
    "full_suite": {
      "assertions_passed": 308,
      "failures": 0,
      "test_warnings": 0,
      "skipped": 2
    },
    "F11": "local_edition(3) added; all gl.ibd assertions unchanged"
  }
}
```
