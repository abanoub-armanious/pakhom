# pakhom 1.0.0

## Sprint-4 phase 37: comprehensive audit pass (3 agents, 12+ findings, 1 CRITICAL bug fixed)

Three parallel audit subagents (UX surface review / AC1-AC10 cross-mode
compliance / dead code + technical debt) caught **one CRITICAL bug**
that would have killed first-impression adoption, plus a HIGH AC3
violation, plus several AC4 gaps. The audit pattern continues to
catch real issues that phase-by-phase audits hadn't surfaced.

### CRITICAL fix (would have made every Quick Start fail)

The README + methodology-modes vignette Quick Start examples called
`create_config(methodology = "...", framework_spec_path = "...",
database_path = "...", output_path = "...")` — but **none of those
kwargs existed** in the actual function. The pre-Sprint-4 signature
took `study_name`, `data_path`, `config_path`, etc. and produced a
config without any methodology block. A first-time user copy-pasting
the Quick Start hit `Configuration validation failed:` for every
kwarg they passed and could not recover without reading source.

`create_config()` rewritten to:
- Accept `methodology` as the first arg (mandatory; per AC3)
- Accept `framework_spec_path` (Mode 3) + validate it's set when
  `methodology = "framework_applied"`
- Accept `database_path` and `output_path` as the documented kwargs
  (matches the Quick Start examples + the methodology-modes vignette)
- Write a properly-structured `methodology = list(mode = ...,
  framework_spec_path = ...)` block into the YAML
- Allow Mode 1 to skip `research_focus` (corpus + theme_set are
  passed at `run_mode1()` time, not config time)

### HIGH fixes

- **`default_config()` AC3 violation** (caught by AC audit). The
  programmatic API was warn-and-defaulting to `codebook_collaborative`
  when methodology was NULL — exactly the silent-default failure mode
  AC3 ("no default mode; explicit declaration mandatory") commits the
  package against. Now hard-stops with a message pointing to the
  three valid modes + the decision aid. Symmetric with `validate_config`'s
  YAML-load behavior. Test updated to assert the error.
- **pkgdown reference index** (caught by UX audit) listed ~14
  unexported functions (`validate_class`, `find_latest_run`,
  `safe_progress_bar`, etc.) that pkgdown silently dropped or
  warned on every build. Trimmed to only-exported entries; collapsed
  the bloated "Internal Helpers" section into a focused "Pipeline
  Step Helpers" section + a "Package" section.
- **README architectural commitments** (caught by UX audit) was
  gatekeeping-shaped at line 80, before users had reason to care.
  Moved AC1-AC10 to a "For methodologists / reviewers" section near
  the bottom of the README. Quick Start now opens with a "Recommended:
  web-based config wizard" subsection (the wizards were previously
  buried).

### MEDIUM fixes (AC4 stamping gaps)

- **Mode 3 framework_applied.yaml/json archive AC4 stamp** (caught by
  AC audit). `archive_framework_spec()` previously copied the source
  spec verbatim with no methodology stamp on the archived file. A
  reviewer auditing the file in isolation couldn't tell it was a
  Mode 3 stamp. Fix: archive now carries a `# methodology: M3 -
  Framework Applied | run: <id>` comment header (YAML) or a
  `_methodology_stamp` envelope (JSON). The replay-equivalence sha256
  is anchored to the SOURCE bytes (computed pre-stamp), preserving
  the contract that `arch$hash == digest(source)`. The archived file
  remains parseable: yaml::yaml.load strips the comment header so
  the FrameworkSpec round-trip is identical.
- **Memo `.md` YAML frontmatter AC4 stamp** (caught by AC audit).
  `memo_to_markdown()` + `persist_memos()` now emit
  `methodology_mode:` + `run_id:` fields in each memo's frontmatter,
  defaulting to `"reflexive_scaffold"` (memos are a Mode 1
  construct). A memo file lifted out of its run dir still
  self-identifies.
- **Integrity sentinel string `framework_applied.{yaml|yml|json}`**
  (caught by AC audit) — the displayed sentinel string in
  `verify_run_integrity` only listed `yaml|json` while the
  accept-set actually covered `.yml` too. Fixed to match the
  accept-set + corresponding test refs updated.

### Audit findings deferred to a future phase (acknowledged but not
breaking)

- `compare_models()` is exported + documented but has no callers in
  R or tests. Audit recommended deletion. Deferred because it's
  user-facing public API per the README + vignette — deletion would
  be a breaking change at v1.0.0. Future phase: add a regression
  test that exercises it (gives it a meaningful caller) OR mark as
  superseded.
- Researcher-review CSVs (`codebook_review.csv`, `themes_review.csv`,
  IRR coding sheets) lack the methodology stamp. Same AC4 violation
  as the framework archive but in a different module (`R/19_researcher_review.R`,
  `R/11_human_verification.R`). Plumbing methodology_mode through
  these review functions touches multiple call sites. Deferred to a
  follow-up phase.
- Several dead-code findings (legacy `primary_emotion` fallback in
  sentiment; 8000-char text-truncation magic number; framework
  prompt block hot-loop hoist) are real but low-impact. Deferred.
- Vignette polish: `install_github("username/...")` placeholder,
  DBI/RSQLite prereq install line, Mode 1 README corpus shape note.
  Deferred.

Tests: 2396 pass / 0 fail (2391 -> 2396 = 5 new tests pinning the
phase 37 fixes including the framework-archive-stamp regression
test). R CMD check stays at 0 errors / 0 warnings / 1 routine NOTE
(only "unable to verify current time" remains).

## Sprint-4 phase 36: CRAN basics + Imports trim

Bounded scope-cut between phase 35 (docs) and a future real-data
validation phase. Aim: address CRAN-prep low-hanging fruit so the
package metadata + imports + machine-readable citation are all in
shape, without spending API credits before the package is in
submission-ready form.

- **DESCRIPTION**:
  * Title updated from "AI-Integrated Thematic Analysis Following
    Braun and Clarke" to "AI-Assisted Reflexive Thematic Analysis
    with Methodology-as-Architecture" -- the new title surfaces
    the package's load-bearing architectural commitment, not just
    the methodological tradition.
  * Description blurb rewritten to describe the three modes, the
    universal Tier-0 transparency commitments, and the empirical
    motivations (Sarkar 2024, Jowsey et al. 2025, Braun and Clarke
    2022).
  * Added URL field (GitHub repo + pkgdown site) and BugReports
    field per CRAN best practice.

- **CITATION.cff** (NEW): machine-readable CFF 1.2.0 citation file at
  the package root. Carries package metadata, repo links, abstract,
  keywords, and references to the three empirical citations.
  GitHub renders this as a "Cite this repository" widget on the repo
  home page; tools like Zotero / Zenodo / DataCite consume it
  automatically. .Rbuildignore'd so it doesn't ship in the CRAN
  tarball.

- **Imports -> Suggests** (clears the longstanding
  "Imports includes 21 non-default packages" CRAN NOTE):
  * `xml2` -> Suggests. Used only by `export_qdpx()`. Now guards
    with `requireNamespace("xml2", ...)` and emits a friendly
    install.packages error if missing.
  * `corrplot` -> Suggests. Used only by `create_correlation_plot()`.
    Falls through with a log_warn if not installed.
  * `progress` -> Suggests. Used only by `safe_progress_bar()`.
    Falls through to a logger-based progress indicator if not
    installed.
  Net: 21 non-default Imports -> 18, clearing the CRAN NOTE.

- **@examples on six main entry points**: `run_analysis`,
  `run_mode1`, `load_framework_spec`, `add_memo`,
  `archive_framework_spec`, `compute_mode1_coverage`. Examples
  requiring API keys / on-disk data are `\dontrun{}`-wrapped;
  self-contained ones run live.

R CMD check now: **0 errors / 0 warnings / 1 routine NOTE** (the
"unable to verify current time" environmental NOTE is the only
one remaining; the long-standing "21 imports" NOTE is cleared by
the trim above). 2391 tests still pass; both vignettes
(getting-started + methodology-modes) build cleanly.

## Sprint-4 phase 35: user-facing documentation pass (Sprint-4 architecture surfacing)

The README, vignette, package help page, and pkgdown reference index
all pre-dated Sprint-4 -- the architectural story (three methodology
modes + AC1-AC10 commitments + Tier-0 transparency + run_mode1 +
framework specs + memos) was invisible to users reading the docs.
Phase 35 brings the docs into alignment with the architecture.

- **R/00_package.R**: expanded `?pakhom` with sections covering the
  three methodology modes, AC1-AC10 architectural commitments, Tier-0
  universal transparency requirements (T0.1/T0.2/T0.3), main entry
  points (run_analysis, run_mode1, load_framework_spec, add_memo),
  and the Mode 3 + Anthropic Citations API bypass disclosure.
- **README.md**: added "Why pakhom?" opening with the methodology-as-
  architecture story (citing Sarkar 2024, Jowsey et al. 2025, Vikan
  et al. 2026), Three modes table, Architectural commitments list,
  Tier-0 transparency section, and Quick Start expanded from one
  Mode 2 example to one example per mode.
- **_pkgdown.yml**: 5 new reference sections (Methodology Modes /
  Tier-0 / Run State + Soft-Lock / Output Stamping / Methodology
  Rules) covering the Sprint-4 surface that previously had no
  pkgdown index entries.
- **vignettes/methodology-modes.Rmd** (NEW; ~330 lines): worked
  examples for each mode, decision rubric matrix, universal Tier-0
  explanation, and replay-equivalence narrative.
- **vignettes/getting-started.Rmd**: added "Which mode is this
  vignette for?" callout cross-linking the new vignette.

R CMD check 0/0/2 environmental, 2391 tests pass, both vignettes
build cleanly.

## Sprint-4 phase 34: end-to-end pipeline integration tests (+ 1 production bug fix)

Closes phase 30 audit MEDIUM #4: existing tests covered components in
isolation but had no end-to-end coverage of `run_analysis()` or
`run_mode1()` through to `finalize_run`. The phase 30 audit memo
explicitly noted this as the gap that "would have caught" silent
failures like the phase 29 `apply_framework_themes`-not-populating-
merge_history bug. Phase 34 fills the gap with 9 e2e tests + a
smart `ai_complete` mock, and -- per the audit pattern -- the new
e2e tests immediately surfaced one real production bug that no
component-level test had caught.

- **`R/tests/testthat/test-pipeline-e2e.R`** (new). 9 e2e tests pinning
  AC4 / AC5 / AC7 / AC8 against silent regression:
  * Mode 2 e2e produces the full Mode 2 artifact set + finalize_run
    + AC4 methodology stamp in `run_metadata.json`.
  * Mode 3 e2e archives `framework_applied.yaml` + stamps framework
    name + sha256 hash in `run_metadata.json` + renders the Framework
    Declaration section in the HTML report + the Mode 3 + Anthropic
    Citations API bypass footnote fires.
  * AC4 propagation: every CSV produced by `export_results` carries a
    methodology stamp comment header.
  * AC5 (cross-mode): `run_analysis(resume=TRUE)` against a finalized
    Mode 2 run with a Mode 3 config errors with a "FINALIZED |
    mismatch | fork" message rather than silently overwriting.
  * AC5 (same-mode): `run_analysis(resume=TRUE)` against a finalized
    Mode 2 run with a Mode 2 config errors rather than overwriting.
  * AC8 (cross-mode artifacts): Mode 2 produces no `framework_applied.yaml`;
    Mode 3 does. Both have universal Tier-0/Tier-1 artifacts. Run-dir
    suffix carries mode short-code (`_M2` / `_M3`).
  * AC8 (Mode 1 vs Mode 2/3): `run_mode1()` produces Mode 1-specific
    artifacts (`reflection_log.json`, `provocations.csv`,
    `provocation_attempts.csv`, `coverage_mode1.json`) and NOT
    Mode 2/3 artifacts (`sentiment_scores.csv`, `correlations.csv`,
    `theme_entries`).
  * AC9: `run_metadata.json` captures `config_hash` for replay-
    equivalence + the methodology rules markdown is written to
    `outputs/<run>/rules/methodology_rules.md`.
  * Mode 2 with `generate_report=TRUE` exercises the full Rmd render
    path (the path that surfaced the bug below).

- **Smart `ai_complete` mock** (`.smart_mock_ai_complete`): switches on
  the `task` argument to return shape-appropriate JSON for each
  pipeline call site (`coding`, `sentiment`, `theming`, `insight`,
  `synthesis`, `review`, `saturation_check`). Pragmatic minimal
  responses are enough to drive the pipeline through; production code's
  `parse_json_safely` + `tryCatch` guards handle minor schema gaps
  gracefully -- the e2e tests assert architectural invariants
  (artifact set + finalize_run + stamping), not AI output content.

- **Production bug fix** (`R/16_report_helpers.R`
  `aggregate_overall_statistics`): when no `theme_membership_*` columns
  exist BUT an `emerged_themes` column does (and is all-NA), the
  function fell into a branch that built `themes_df` via
  `tibble(theme_name = names(theme_tbl), ...)`. Because
  `names(table(character(0)))` returns `NULL`, and `tibble(theme_name =
  NULL, ...)` silently drops the column entirely, the downstream
  `pull(theme_name)` in `generate_report()` then errored with
  `"object 'theme_name' not found"`. The bug fired in any Mode 3 run
  where the AI's coded constructs didn't match any framework construct
  (`apply_framework_themes` produced an empty theme_set,
  `cascade_theme_assignments` left `emerged_themes` all-NA). Fix:
  coerce `names()` to `character(0)` so the column always exists.
  Regression test added in `test-pipeline-e2e.R` against
  `aggregate_overall_statistics` directly.

- **Audit-driven hardening (1 background subagent on the e2e tests +
  the production bug fix)**:
  * **H1** (CRITICAL same-pattern leak) -- the audit found the SAME
    NULL-vs-character(0) bug in the embedded Rmd plot code at
    `R/17_report.R:1051` (the `theme-distribution` chunk). Different
    location from the helper fix; same underlying R quirk
    (`names(table(character(0)))` returns `NULL`,
    `tibble(theme_name = NULL, ...)` drops the column). Would have
    crashed inside the rendered Rmd at knit time for any all-NA
    `emerged_themes` data. Fix: same `if (length(theme_tbl) > 0L)
    names(theme_tbl) else character(0)` guard, embedded in the
    string-built chunk.
  * **H2** -- the original Mode 3 e2e exercised only the empty-theme-
    set branch of `apply_framework_themes` because the smart mock
    returned `code = "NEW: mock_code"` for every coding call (which
    never matches a TPB construct id). The phase 29 silent failure
    that motivated phase 34 lived in the OPPOSITE branch: non-empty
    theme_set -> rebuild_code_to_theme_map -> cascade. Fix: smart
    mock now extracts a verbatim slice from the prompt's
    `Entry text: "..."` and accepts a `coding_code_name` parameter
    so a Mode 3 happy-path test can feed it `coding_code_name =
    "attitude"` to drive the construct-matching path. The mock now
    also includes both Mode 2 fields (`code` + `code_description`
    + `code_type`) and Mode 3 fields (`construct_id` +
    `anomaly_reason`) so a single mock works across both modes.
    Added Mode 3 happy-path test that asserts non-empty `theme_set`,
    populated `merge_history$code_to_theme_map`, and at least one
    entry assigned to a theme via `theme_membership_*`.
  * **L3** -- `verify_run_integrity` listed `analysis_report.Rmd` as
    unconditional but the writer lives inside `generate_report`'s
    body (only fires when `output$generate_report = TRUE`). Spurious
    "1 file(s) missing -- analysis_report.Rmd" warnings on every
    legitimate `generate_report = FALSE` run. Fix: moved the Rmd to
    the conditional block alongside the other report files.
  * **AC2 + AC3** negative tests -- two cheap regression pins:
    `run_analysis` rejects an unknown methodology mode (AC2: "no
    fourth mode") AND rejects a config without an explicit
    `methodology$mode` (AC3: "no default; explicit declaration
    mandatory").
  * **M1** -- AC4 propagation regex was too permissive
    (`methodology|M2|codebook_collaborative` could match unrelated
    content). Tightened to `^# methodology:` prefix anchor + inner
    `M2` short-code check.
  * **M3** -- the e2e config used `min_char` (wrong field name) when
    production reads `min_text_length`. The wrong name was silently
    ignored. Fix: use the correct production field name in the e2e
    config so the fixture reflects production reality (and to flush
    out any user copying this config as a starting point).
  * **L1** -- AC5 same-mode test asserted `coding_calls > 0` only.
    Tightened to exact equality with `test_mode$sample_size = 3`
    so a regression that skips entries (or double-codes them)
    surfaces.

Phase 34 net adds 98 e2e tests + 2 production bug fixes (the original
empty-emerged-themes bug + the same pattern in the embedded Rmd
plot code) + the integrity-check fix for `analysis_report.Rmd`
unconditional listing. Test count: 2293 -> 2391 net. R CMD check
stays at 0 errors / 0 warnings / 2 routine NOTEs.

The audit pattern ("write the test, find a bug") worked exactly as
the user's standing memo predicted: the e2e tests caught the first
production bug, the audit subagent caught the same bug pattern in a
different location plus an entire missing test path that hid the
phase 29 silent failure mode.

## Sprint-4 phase 33: M1.3 reflexive memos as data (Mode 1 AC6 parity)

Closes phase 30 audit HIGH #2 / H5: ResearcherReflectionLog has carried
a `memos = list()` slot since phase 30 but had no CRUD API. Per AC6
(symmetric obligations across modes), Mode 1's burden parity vs Modes
2/3 is delivered through reflexive memos at pause points -- without a
memo CRUD + persistence layer, Mode 1's burden was aspirational rather
than operational. Phase 33 implements the foundational layer per the
SPRINT4_DESIGN.md M1.3 spec (line 277-298): typed memos with Markdown
round-trip + YAML frontmatter + persistence under `memos/` + report
section + integrity tracking. Future phases may layer AI-coding-of-
memos (the "researcher voice" theme set, spec line 293) on top.

- **`R/memos.R`** (new file). The full memo API:
  * `make_memo(body, type, ...)` constructor with the SPRINT4_DESIGN.md
    M1.3 schema (id, timestamp, author, type, linked_codes,
    linked_themes, linked_entries, linked_prior_memo, body) + 4 valid
    types (operational / coding / theoretical / positionality).
  * `add_memo(log, body, ...)` appends a memo to a
    ResearcherReflectionLog. Pass-by-value: callers must capture the
    return. Memos are *immutable* once added -- there is no
    `update_memo` or `delete_memo` (revisions add a NEW memo with
    `linked_prior_memo` pointing at the antecedent; memo evolution is
    data per Birks/Chapman/Francis 2025). When an `audit_log` is
    supplied, a `memo_added` decision is recorded.
  * `read_memo(log, id)` returns the Memo or NULL.
  * `list_memos(log, type/author/linked_theme)` returns a filterable
    tibble.
  * `memo_to_markdown(memo)` / `markdown_to_memo(md_text)` -- the
    Markdown + YAML frontmatter round-trip per spec line 280-291.
    Handles bodies with apostrophes, quotes, colons, multi-line
    content; YAML strings are single-quoted with embedded apostrophes
    doubled per the YAML spec.
  * `persist_memos(log, run_dir)` writes one
    `outputs/<run>/memos/<memo_id>.md` per memo. Idempotent: re-calls
    produce byte-equivalent output (replay-equivalence).
  * `load_memos(run_dir)` reads them back, sorted chronologically.
    Skips malformed `.md` files with a warning rather than crashing
    (resilient to manual edits / corruption).
  * `Memo` S3 class + `print.Memo`.

- **ResearcherReflectionLog schema 1.1.0 -> 1.2.0**: the `memos` slot
  now holds typed `Memo` S3 objects rather than unstructured list
  entries. Backward-compatible: pre-1.2.0 logs whose memos slot
  contains untyped entries are preserved in place; downstream code
  paths (the report's memo section, the coverage's n_memos field)
  filter via `inherits(m, "Memo")` so only typed memos are surfaced.

- **`run_mode1()` integration**:
  * **Resume**: hydrates memos from on-disk Markdown files
    (`load_memos(output_dir)`) when resuming. Per AC4, the Markdown
    files are the canonical source of truth for memo content -- the
    `reflection_log.json` carries an in-memory copy for replay
    convenience but the Markdown files survive a JSON-format change.
  * **Finalize**: persists all memos before `finalize_run()`. A memo
    write failure (e.g., disk full) is a soft warning, not an abort
    -- unlike the framework_archive case (where AC4 mandates the
    archive), memos are researcher-authored and an empty memo set is
    a valid Mode 1 outcome.

- **Mode 1 report: Researcher Reflexive Memos section** (new
  `.build_mode1_memo_section` helper in `R/mode1_report.R`). Renders
  after the per-theme provocations section so the reading order
  mirrors the research workflow (provocations -> reflexive memos).
  Sorted chronologically (timestamp ascending) so the timeline reads
  earliest -> latest. Each memo block carries: type badge, id,
  timestamp + author, links (themes + codes + entries + prior memo),
  and body in a `<pre>`-wrapped `white-space: pre-wrap` block so
  Markdown inside the body doesn't restructure the surrounding
  document. **Empty-memo state** renders an explicit AC6-noting
  notice rather than silent omission.

- **`ProvocationCoverage` schema 2.0.0 -> 2.1.0**: added `n_memos`
  (count of typed memos) and `memos_by_type` (named list, e.g.,
  `list(operational = 2, theoretical = 5)`). Informational only --
  the headline `no_silent_skip` is unchanged. The fields exist so
  the methodology paper can compute researcher-side burden as a KPI
  alongside AI-side coverage.

- **`verify_run_integrity_mode1`** now returns `n_memos_persisted`
  (count of `.md` files under `memos/`). Memos are conditionally
  expected -- a Mode 1 run with zero memos is valid (the run may
  have been used for provocation generation only).

- **HTML escaping**: every researcher-supplied interpolation in the
  memo report block (body, links, author, prior-memo id) is routed
  through `.html_esc`. A regression test pins XSS prevention with a
  crafted `<script>` body + author + theme.

- **Audit-driven hardening (1 background subagent on the implementation
  surface)**:
  * **Audit C1 (CRITICAL)** -- `.read_reflection_log_json` was
    re-classing provocations + provenance on resume but not memos.
    Memos in a resumed JSON-only run were plain lists, and every
    consumer gating on `inherits(m, "Memo")` (the report's memo
    section, persist_memos, the n_memos counter on the coverage
    object, the by-type print breakdown) silently treated the run
    as having zero memos -- a researcher-work-loss bug. Fix: same
    re-class pattern the provocations + provenance use.
  * **Audit H1** -- the body round-trip was not byte-equivalent for
    bodies with trailing whitespace (`"test\n"` round-tripped to
    `"test"`, etc.). Fix: canonicalize body at `make_memo` construction
    time (strip trailing whitespace once, store the canonical form);
    `markdown_to_memo` no longer post-processes the parsed body. Now
    the round-trip is trivially identity.
  * **Audit H2** -- `print.Memo` crashed on a Memo with NULL
    `linked_prior_memo` because `is.na(NULL)` returns `logical(0)`
    and `if` errors on length zero. Fix: NULL-safe
    `if (!is.null(...) && length(...) > 0L && !is.na(...))`.
  * **Audit H3** -- the unit tests in `test-memos.R` exercised
    `persist_memos` / `load_memos` / `add_memo` directly, but no
    test exercised them through the `run_mode1` orchestrator. A
    regression that removed the `persist_memos` call from the
    orchestrator would have surfaced only at manual inspection. Fix:
    added an end-to-end test that drives `run_mode1`, adds memos
    post-run, persists them, hydrates them via `load_memos`, and
    verifies the rendered Mode 1 report includes the bodies (no
    empty-state notice). Plus a focused regression test for the C1
    JSON re-class fix.
  * **Audit M1** -- `load_memos` sort was non-deterministic on
    identical timestamps because `order()` falls back to position-
    in-input. Fix: secondary sort by id so the chronological view
    is deterministic regardless of filesystem return order.
  * **Audit M2 + Resume divergence** -- documented `persist_memos`
    overwrite policy explicitly; added a `log_warn` when
    `reflection_log.json` and on-disk memo counts differ on resume
    (the on-disk version always wins per AC4, but a count mismatch
    usually signals manual intervention worth flagging).

Phase 33 net adds ~120 memo tests across `test-memos.R` + the new
e2e + JSON-re-class tests in `test-mode1-orchestrator.R`. Test count:
2168 -> 2293 net (the audit-fix tests pin every issue listed above so
the same bugs cannot regress). R CMD check stays at 0 errors / 0
warnings / 2 routine NOTEs.

## Sprint-4 phase 32: Mode 3 (Framework Applied) transparency hardening

Closes phase 30 audit findings H1 (framework_spec archive), H2 (Framework
Declaration in report), and audit MEDIUM #5 / C3 (Mode 3 + Anthropic
Citations API silent bypass). Before phase 32, a Mode 3 reviewer reading
a generated HTML report saw "M3 - Framework Applied" stamped at the top
but could not tell WHICH theoretical framework was applied (TPB?
COM-B? TDF? a custom YAML?), what its citations were, what the
epistemic stance was, or what the anomaly handling policy said -- AC4
("methodology stamped on every output") was honored at the mode level
but not the framework level. Phase 32 closes the gap end-to-end.

- **`archive_framework_spec()`** (new in `R/framework_spec.R`). Writes
  a verbatim byte-equivalent copy of the loaded framework spec to
  `outputs/<run>/framework_applied.{yaml|json}` (extension preserved
  from source) and computes a deterministic SHA-256 over the file's
  bytes. Returns a metadata list (`name`, `hash`, `epistemic_stance`,
  `anomaly_handling`, `n_constructs`, `construct_ids`,
  `relative_path`) suitable for splatting into `init_run_state(...)`
  and forwarding to the report renderer. Per AC4 the archive is
  mandatory for any Mode 3 run -- the integrity check now flags a
  missing archive as an incomplete run.

- **`run_metadata.json` framework stamping**. `R/18_pipeline.R` now
  invokes `archive_framework_spec()` immediately after `load_framework_spec()`
  (Mode 3 only) and splats the resulting metadata into `init_run_state`
  via `do.call`. Mode 3 runs now carry `framework_name`,
  `framework_hash`, `framework_relative_path`,
  `framework_epistemic_stance`, `framework_anomaly_handling`,
  `framework_n_constructs`, `framework_construct_ids`, and
  `framework_schema_version` in `run_metadata.json`. Mode 1 + Mode 2
  metadata is unchanged (the splat is conditional on
  `framework_archive` being non-NULL).

- **Framework Declaration section** (new `.build_framework_declaration()`
  helper in `R/17_report.R`). Renders only for Mode 3 runs, immediately
  after the Tier-0 dashboards. Includes:
  * Framework name + sha256 fingerprint (first 12 chars) + link to
    the archived spec file
  * Citations (full reference list from the spec)
  * Epistemic stance + plain-language explainer ("constructionist
    treats constructs as researcher-developed lenses; positivist
    treats constructs as universal categories; mixed applies
    constructs as primary but tolerates legitimate revision")
  * Anomaly handling policy + plain-language explainer ("extend =
    new constructs; revise = modify existing definitions; bracket =
    flag as out-of-scope")
  * Full constructs table with id, name, description, and first 3
    example indicators (rest counted as "+N more" so the section
    stays readable on long frameworks like TDF's 14)
  * Citation paragraph confirming byte-equivalence with the loaded
    spec + sha256 fingerprint location in `run_metadata.json`
  Falls through to an explicit "transparency failure" notice when
  the archive is unavailable -- absence is itself an AC4 signal.

- **Mode 3 + Anthropic Citations API bypass footnote** (new
  `.tier0_citations_api_bypass_footnote()` helper). When the run is
  Mode 3 + provider = `"anthropic"`, the Tier-0 source-breakdown card
  now appends an explicit footnote explaining that the Citations API
  prevention layer is structurally precluded (forced `tool_use` schema
  for framework-construct constraint is mutually exclusive with the
  Citations API output format on the same Anthropic response). The
  Mode 3 pipeline relies on the verification ladder's DETECTION-only
  path; the footnote makes the architectural reason explicit rather
  than letting a reviewer infer a bug. Future phases may explore a
  hybrid schema (constrained constructs + paired citation offsets) as
  a research spike.

- **`verify_run_integrity()` Mode 3 expectation**. The integrity check
  for `mode = "framework_applied"` now expects
  `framework_applied.{yaml|json}` to exist under `run_dir` and reports
  it as missing if absent. The accept-either logic supports both
  YAML-source and JSON-source frameworks.

- **HTML escaping**: every researcher-supplied interpolation in
  `.build_framework_declaration` (framework name, citations, construct
  id/name/description, example indicators) is routed through
  `.html_esc`. Tested with a synthetic spec containing `<script>` /
  `&` / quote characters in framework name + construct fields; the
  rendered HTML neutralizes all HTML-active content.

- **Audit-driven hardening (1 background subagent on the implementation
  surface)**:
  * **Audit H1** (silent skip in e2e test) — the previous Mode 3
    integration test wrapped `generate_report()` in a `tryCatch` that
    swallowed the error AND then `skip()`ped if no Rmd was written,
    making any future regression of the Mode 3 wiring invisible. The
    audit predicted this exact failure class ("silent Mode 3 end-to-end
    failure") matched the user's standing audit-pattern memo. Fix: build
    a sufficient input set (full `export_files` shape, sentiment scores
    per entry, multi-theme `theme_set`) so `generate_report` actually
    completes the Rmd, and remove the skip-fallback so any failure is a
    hard test failure. Added a Mode 2 negative test that pins the
    Framework Declaration is NOT rendered for non-Mode-3 runs.
  * **Audit M1 + M2** (archive failure was a soft warn) — per AC4 a
    Mode 3 run cannot finalize without its framework archive; the
    previous `tryCatch` around `archive_framework_spec` absorbed errors
    into a warning, letting the run finalize with broken provenance.
    Fix: the call now propagates errors directly so any archive failure
    aborts `run_analysis()` before any expensive coding work. Symmetric
    with the existing "Mode 3 requires a valid framework spec" hard-stop
    on spec load.
  * **Audit L2** (single-construct `framework_construct_ids` would
    scalarize) — `jsonlite::write_json` with `auto_unbox=TRUE` collapses
    length-1 character vectors into JSON scalars, so a 1-construct
    framework's `construct_ids` would round-trip as `"only_one"` instead
    of `["only_one"]`. Fix: splat `as.list(construct_ids)` into
    `init_run_state`'s extras so the JSON array shape is preserved
    regardless of length. Added a regression test that round-trips
    `run_metadata.json` and asserts the array shape.
  * **Bypass footnote on empty-dashboard path** — caught while fixing
    H1: the Citations API bypass footnote previously only fired when
    the source-breakdown sub-block ran, but the dashboard short-circuits
    to an empty card when zero verbatim claims exist (e.g., a Mode 3 +
    Anthropic run that produces only construct labels with no quote
    citations). Fix: footnote also renders on the empty-dashboard path
    so the architectural reason for the absence is surfaced.

Phase 32 net adds 18 tests (8 in `test-framework_spec.R` for the
archive helper across all three built-in frameworks + the L2
single-construct round-trip; 10 in `test-mode3-framework.R` for the
Framework Declaration render + HTML escaping + bypass footnote
dispatch + integrity-check expectation + a working Mode 3 e2e
report-render + a Mode 2 negative test). Test count: 2089 -> 2168
net. R CMD check stays at 0 errors / 0 warnings / 2 routine NOTEs.

## Sprint-4 phase 31: Mode 1 (Reflexive Scaffold) full orchestrator

Closes the phase 30 audit's CRITICAL findings C1 + C2: Mode 1 was
operational at the provocateur-loop level (`run_provocateur_questioning`)
but lacked the AC4 + AC7 scaffolding that Modes 2/3 have. A Mode 1 run
emitted only a `ResearcherReflectionLog` -- no `run_metadata.json`, no
T0.2 spread, no T0.3 coverage, no rendered report, no `finalize_run()`
call. AC4 ("methodology stamped on every output") and AC7 ("universal
Tier-0 in all modes") were aspirational for Mode 1; phase 31 makes them
operational.

- **`run_mode1()` orchestrator** (new `R/mode1_orchestrator.R`). Top-level
  Mode 1 entry point that mirrors `run_analysis()`'s scaffolding (output
  dir + run metadata + methodology rules + audit log + fabrication log
  + finalize_run + integrity check) but routes through the provocateur
  loop instead of progressive coding. Produces a complete Mode 1 run
  directory under `outputs/<run-id>_M1/` with every Tier-0/Tier-1
  artifact a reviewer would expect, plus Mode 1-specific canonical
  artifacts: `reflection_log.json`, `provocations.csv`,
  `provocation_attempts.csv`, `themes.json`, `coverage_mode1.json`.

- **`compute_mode1_coverage()` + `ProvocationCoverage` S3** (new in
  `R/mode1_orchestrator.R`). Mode 1's analog of T0.3. Where Mode 2/3 assert
  "no silent truncation in the LLM call path" (every preprocessed entry
  reached the LLM), Mode 1 asserts **"no silent skip across themes ×
  provocation categories"** + "the full corpus was provided to per-
  category prompts." Distinguishes legitimate empty results (a category
  that returned zero provocations because no qualifying entries existed)
  from silent skips (a category that was never attempted) -- the central
  semantic that lets the coverage card make a defensible claim.
  `ProvocationCoverage` shares a `Tier0Coverage` virtual parent class
  with `CorpusCoverage` so the report renderer dispatches uniformly via
  the new `render_tier0_coverage_card()` S3 generic.

- **`generate_mode1_report()`** (new `R/mode1_report.R`). Mode 1-specific
  HTML report renderer. The existing `generate_report()` is wired to
  coding_state + sentiment + correlations + AI synthesis -- none of
  which exist in Mode 1, and stubbing them out would risk silent Mode 2/3
  regressions. The Mode 1 report instead reuses atomic helpers
  (`stamp_methodology_html`, `.build_tier0_dashboard`,
  `.build_participant_spread_card`) and adds Mode 1-specific section
  builders for per-theme provocations grouped by category, the Mode 1
  coverage card via `render_tier0_coverage_card()` S3 dispatch, and a
  deterministic executive summary that surfaces top categories, themes
  attracting the most disconfirming evidence, and participant-
  concentration flags.

- **`compute_provocation_provenance_stats()`** (new). Mode 1's analog of
  `compute_quote_provenance_stats()`. Walks
  `reflection_log$provocations`, extracts each provocation's
  `QuoteProvenance` field (built and verified by the per-category
  function via `.citation_to_provocation`), and feeds them through
  `quote_provenance_summary()`. Provocations from observational
  categories (absent_voice, parts of assumption_surfacing) carry NULL
  provenance and are excluded from the verification stats -- the Tier-0
  dashboard's domain is verbatim claims.

- **ResearcherReflectionLog schema 1.1.0**: adds `provocation_attempts`
  and `skipped_themes` data.frames. The attempt tracker records one row
  per (theme × category) attempt regardless of how many provocations the
  AI emitted -- the row's existence proves "not silently skipped" while
  the `n_emitted` column measures emission. The skipped_themes tracker
  records themes the orchestrator bypassed with a stated reason (e.g.,
  zero supporting entries) so the coverage card distinguishes
  *explicit skip with stated reason* from *silent skip*. Backward-
  compatible: 1.0.0 logs loaded as `resume_log` have the new slots
  backfilled empty.

- **`render_tier0_coverage_card()` S3 generic** (new in
  `R/corpus_coverage.R`). Single dispatch entry point for the report's
  Tier-0 coverage card; methods on `CorpusCoverage` (Mode 2/3, in
  `R/17_report.R`) and `ProvocationCoverage` (Mode 1, in
  `R/mode1_orchestrator.R`) keep the call site in `.build_rmd_content`
  branch-free. The legacy `.build_corpus_coverage_card()` is preserved as
  a thin compat wrapper that routes through the generic so existing tests
  in `test-corpus_coverage.R` and `test-tier0-smoke.R` continue to pass.

- **`verify_run_integrity()` mode dispatch**. The integrity-check
  function now dispatches on `config$methodology$mode`. Mode 1 expects a
  different artifact set (no sentiment_scores.csv, no correlations.csv,
  no theme_entries directory; instead reflection_log.json,
  provocations.csv, provocation_attempts.csv, coverage_mode1.json).
  Mode 2/3 expectations unchanged.

- **Bug fix**: `find_latest_run()` regex predated the T1.7 mode-suffixed
  run dirs (phase 25-27) and silently returned NULL for ANY mode-
  suffixed dir, breaking the resume path across all modes. Regex updated
  to allow the optional `_M[123]` tail. Caught while writing the AC5
  resume-finalized refusal test for Mode 1; affects Mode 2 and Mode 3
  resume too.

- **Pipeline friendly-error update**: `R/18_pipeline.R`'s Mode 1 refusal
  message now points users at `run_mode1()` (the scaffolded entry point)
  in addition to `run_provocateur_questioning()` (the bare loop).

- **Audit-driven hardening (3 parallel general-purpose audit subagents)**.
  The audit pattern caught real issues that unit-testing alone missed;
  fixes applied in the same commit:
  * **C1** `attempts_per_category` and `explicit_skip_reasons` now
    serialize as named JSON objects (previously `auto_unbox=TRUE` on
    named integer vectors produced anonymous arrays in
    `coverage_mode1.json`, defeating replay/audit). `ProvocationCoverage`
    schema bumped 1.0.0 -> 2.0.0.
  * **H1** `compute_mode1_coverage` now partitions attempts into
    in-scope vs out-of-scope WRT `requested_categories` (previously
    `factor(..., levels=requested)` silently dropped unexpected-category
    rows from `attempts_per_category` while still counting them via
    `nrow(attempts)`, producing contradictory `recorded > expected`).
    New fields: `n_unexpected_category_attempts`, `unexpected_categories`,
    `no_unexpected_category_attempts`.
  * **H2 / H3** `no_silent_skip` headline now requires
    `n_themes_input > 0L` AND `n_themes_attempted > 0L` so degenerate
    states (zero-themes input, all-themes-explicit-skipped) don't grade
    as verified coverage. Coverage card banner branches accordingly with
    distinct messages for each degenerate case.
  * **M3** Replaced the unconditional `no_silent_corpus_truncation = TRUE`
    boolean (which overclaimed -- the per-category prompts in
    `R/provocateur.R` include only theme-supporting entries, not the
    full corpus text) with two honest fields:
    `corpus_provided_to_per_category_fns` (TRUE -- `data` IS passed) and
    `llm_prompt_includes_full_corpus` (FALSE -- prompts only embed
    supporting-entry text). Coverage card adds a "prompt context" note
    explaining the constraint and flagging corpus-search retrieval as a
    future phase. The verification ladder still catches any hallucinated
    entry_id the LLM might invent.
  * **A.H3** `.read_reflection_log_json` now uses `simplifyVector=FALSE`
    and explicitly re-classes nested `Provocation` + `QuoteProvenance`
    objects after the JSON read (previously the round-trip stripped S3
    classes and downstream resume-time consumers gating on
    `inherits(...)` would silently emit NA-cited rows).
  * **A.L4** `init_run_state` is now called AFTER `create_ai_provider`
    so `model_primary` and `model_fast` get stamped into
    `run_metadata.json` (parity with run_analysis -- previously Mode 1
    metadata was missing those cross-mode-comparison fields).
  * **A.H1 / H2** Wrong-mode + finalized-resume errors use single
    multi-line messages (parity with run_analysis's friendly-error
    style); `find_latest_run` returning NULL on `resume=TRUE` now logs
    "No previous run found" instead of falling through silently.
  * **B (XSS)** Theme names from researcher-supplied input are now
    HTML-escaped in the executive summary's concentration-flags and
    disconfirming-evidence lines (previously a crafted theme name like
    `<script>alert(1)</script>` would interpolate raw into the rendered
    Rmd). The per-theme provocation section already escaped via
    `.html_esc`; this closes the remaining unescaped path.
  * **B (semantic)** Executive summary now distinguishes "no fabrications
    detected" (verbatim claims existed AND none failed verification)
    from "no verbatim claims to verify" (e.g., a Mode 1 run that only
    used absent_voice or assumption_surfacing erased-terms produces
    NULL-provenance provocations -- nothing to fabricate from).

Phase 31 net adds ~225 tests across `test-mode1-coverage.R`,
`test-mode1-orchestrator.R`, `test-mode1-report.R`, plus extensions to
`test-provocateur.R` (attempt-tracking + schema 1.1.0 assertion) and
`test-mode3-framework.R` (run_mode1 reference in the friendly-error).
Test count: 2032 -> 2089 net (the audit-fix tests pin every issue
listed above so the same bugs cannot regress). R CMD check stays at
0 errors / 0 warnings / 2 routine NOTEs (21 imports + future
timestamps -- both environmental and unchanged from prior phases).

## Pre-publication rename: thematicai -> pakhom

Pre-publication rename so the package's name matches its GitHub repo and
methodology-paper identity. The new name **pakhom** (Coptic ⲡⲁϩⲱⲙ,
"eagle") honors Saint Pachomius the Great (c. 292–348 CE), the Coptic
Egyptian abbot whose written **Rule** of communal discipline established
the genre of methodology-as-written-document. The package extends that
lineage to AI-assisted thematic analysis: methodology is codified at
the **architectural** level, not at the configuration level. The Coptic
form *pakhom* (rather than the Hellenized *Pachomius*) is used
deliberately — naming a tradition in its own voice.

The previous name *thematicai* had real conflicts that the multi-round
investigation surfaced: namespace adjacency to the existing CRAN package
`thematic` (Posit, ggplot2 theming), trademark adjacency to Thematic
Analysis Inc. (the YC-backed customer-feedback SaaS company), and an
SEO-impossible name (the descriptive phrase "thematic AI" is used by
every commercial QDA vendor). pakhom solves all three: clean CRAN
namespace, no commercial conflicts, distinctive search profile.

Mechanical change only — all behavior unchanged. S3 class names like
`ThematicConfig` and `ProgressiveCodingState` are preserved because
they describe what the object *is conceptually* (a thematic analysis
config, a progressive coding state), not the package brand.

## Sprint-4 Phase B: Tier-0 Universal Requirements (in progress)

Phase B addresses the most-cited empirical critiques of LLM-for-TA tools
via three "Frankenstein-derived" universal requirements (mandatory in all
modes). The naming references Jowsey, Braun, Clarke, Lupton & Fine 2025
(PLOS One, doi:10.1371/journal.pone.0330217) which characterized
Microsoft Copilot's failures as Frankenstein-like assemblage from
disconnected fragments.

- **T0.1 part 1: Quote provenance + 4-step verification ladder** — new
  `R/quote_provenance.R` module with `make_quote()` constructor (deterministic
  SHA-1 quote_id, source SHA-256 for drift detection), `verify_quote()`
  four-step ladder (strict offline string match → normalized match
  (smart-quote/whitespace/case) → substring search → embedding cosine
  via provider) that downgrades verification_status accordingly,
  `verify_quotes()` batch wrapper, `init_fabrication_log()` /
  `log_fabrication()` for the methodology paper's KPI CSV at
  `outputs/<run>/fabrication_log.csv`, and `quote_provenance_summary()`
  for the upcoming report dashboard. Verification ladder distinguishes
  fabricated (text never in source) from drifted (source edited since
  attribution) via SHA-256 comparison. Render policy: fabricated quotes
  are never rendered; unverified get warning markers; drifted trigger
  corpus-integrity warnings. Module is foundation for the upcoming
  Anthropic Citations API integration (T0.1 part 3).

- **T0.1 part 3a: Tier-0 Data Integrity Dashboard in the report** — every
  generated HTML report now renders a "Data Integrity Dashboard (T0.1)"
  card immediately after the Executive Summary, showing how many
  AI-attributed verbatim claims were checked, how many verified exactly
  vs fuzzy (with method breakdown by ladder step: string_match,
  normalized_match, substring_search, embedding_cosine), how many
  fabrications were dropped, and a relative-path link to
  `outputs/<run>/fabrication_log.csv` when fabrications occurred. The
  dashboard cites Jowsey et al. 2025 doi:10.1371/journal.pone.0330217 in
  its body so reviewers immediately see what the package is doing about
  the field's most-cited critique. New `compute_quote_provenance_stats(coding_state)`
  exported helper aggregates verification stats from any
  ProgressiveCodingState; pre-T0.1 states (no `$provenance` on
  segments) get an empty-summary dashboard explaining why.
  Singular/plural noun-verb agreement is correct for both 0, 1, and N+
  fabrications.

- **T0.1 part 2: Verification ladder wired into per-entry coding** —
  `.code_entry_progressive()` now constructs a `QuoteProvenance` for every
  AI-attributed coded segment and runs the 4-step ladder against the
  source entry text. Fabricated segments are dropped from the codebook
  AND the entry's coded_segments AND written to
  `outputs/<run>/fabrication_log.csv` AND emit a `quote_fabricated`
  audit decision (T1.4 schema slot) for cross-run analysis. Verified
  segments (exact OR fuzzy) keep the QuoteProvenance attached as
  `seg$provenance` so downstream consumers can show verification
  status, attribute back to the AI call (`ai_call_id` joins to audit
  log `request_id`), and re-verify at report-load time. `run_progressive_coding()`
  and the pipeline orchestrator gain a `fabrication_log` parameter
  (NULL keeps verification active but skips the CSV; pipeline always
  passes a real log). The previous primitive substring-match validation
  (which would silently keep AI-fabricated text "as-is") is removed.

## Sprint-4 Phase A: Foundation (complete)

The Sprint-4 architectural rebuild transforms pakhom from "AI-assisted
thematic analysis tool" into a multi-mode AI-qualitative platform with
explicit per-methodology agency configuration, Frankenstein-derived
universal requirements, and methodology-as-permission-structure
architecture. Phase A lays the foundation; subsequent phases ship the
mode clusters, Tier-0 universals, and open-science infrastructure.

Phase A items shipped (all backward-compatible — existing v1.x
configs and runs continue to work):

- **OS.1: Saturation Signal 2 citation precision** — `slope_ratio` is now
  documented as De Paoli & Mathis (2024) Inductive Thematic Saturation
  ratio (doi:10.1007/s11135-024-01950-6). The 0.05 stopping threshold is
  noted as stricter than De Paoli's illustrative 0.28 single-timepoint
  observation because we use the ratio as a stopping criterion.

- **OS.2: Correlations reframed as exploratory associations** — themes are
  inductively derived from the same data the correlations are computed on,
  so framing the results as significance-tested findings was misleading.
  Per Rothman (1990, *Epidemiology* 1(1):43-46), correlations now ship
  with raw + Benjamini-Hochberg + Bonferroni p-values side-by-side and
  meaningful-effect-size flags. Reports use "Exploratory Associations"
  framing with hypothesis-generating language. Legacy column names
  (`p_value`, `p_adjusted`, `significant`) preserved for back-compat.

- **T1.3: Multi-mode methodology declaration** — three modes shipped:
  `reflexive_scaffold` (AI as provocateur, Mode 1), `codebook_collaborative`
  (AI proposes, researcher gates, Mode 2), `framework_applied` (AI applies
  researcher's framework, Mode 3). Mode 4 (AI-heavy content analysis)
  intentionally not shipped per the Cochrane RevMan refusal pattern;
  content-analytic use case absorbed into Mode 3 via positivist framework
  choice. New `methodology_decision_aid()` function provides a 3-question
  wizard or non-interactive recommendation engine. Configs missing the
  methodology block fail validation with a pointer to the decision aid.
  References: Lin (2025) Cognitio Emergens (arXiv:2505.03105), Prahl ARC
  (Qual Health Res 2026, doi:10.1177/10497323251401503), Jowsey, Braun,
  Clarke, Lupton & Fine (2025, doi:10.1177/10778004251401851).

- **default_config() warning on silent methodology default** — closes a
  subtle internal contradiction: phase 11 cited Spool 2011 (>95% of users
  never change defaults) for "no silent default-mode trap" but
  `default_config()` itself was a silent trap. Now warns when called
  without `methodology` and falls back to `codebook_collaborative`;
  explicit-mode `default_config("...")` is silent.

- **T1.1: `ai_complete()` returns structured list with provenance** —
  refactored from returning a bare string to returning
  `list(content, model, usage, finish_reason, raw_response, prompt_hash,
  request_id)`. The structured return unblocks the audit log expansion
  (T1.4), Structured Outputs migration (T1.2), and replay_run (OS.5)
  simultaneously. Token usage is normalized across providers (Anthropic's
  `input_tokens`/`output_tokens` remap to OpenAI-style names); Anthropic's
  `stop_reason` normalizes to canonical `stop`/`length`/`tool_use`.
  `prompt_hash` is a SHA-256 over the JSON serialization of the request
  (stable across R versions and platforms) and is used as the cache key
  for replay. `ai_complete()` is internal so the change is contained to
  in-package callers (7 sites updated); no external callers affected.

- **T1.2: Native Structured Outputs migration (OpenAI strict json_schema +
  Anthropic forced tool-use)** — six task schemas in
  `R/structured_schemas.R` (coding, saturation, sentiment, theming,
  insight, synthesis) replace the prompt-based JSON-mode coercion that
  previously relied on `parse_json_safely()` defensive parsing. With the
  schema enforced server-side (`response_format = list(type =
  "json_schema", strict = TRUE, ...)` for OpenAI; a forced
  `record_analysis` tool-call for Anthropic), JSON parse failures are
  eliminated and schema-drift bugs become impossible: a model update
  can no longer silently change the response shape. Schemas are designed
  to satisfy OpenAI's strict-mode constraints (the stricter of the two
  providers): every object has `additionalProperties = FALSE`, every
  property is in `required` (optional fields use nullable types like
  `list("integer", "null")` for theming's merge_into), `required` and
  `enum` arrays use `list()` to survive jsonlite's auto_unbox. The
  `.validate_schema()` helper catches mistakes at package-load and test
  time so a malformed schema fails fast in R rather than producing an
  opaque OpenAI 400. Reasoning models (o1/o3/o4) silently fall back to
  json_mode because they don't support strict json_schema as of writing.
  `prompt_hash` (the OS.5 replay cache key) now includes
  `response_schema` so requests with different schemas don't collide.
  All six in-package callers migrated (`.code_entry_progressive`,
  `.ai_saturation_check`, `analyze_sentiment`, `.run_merge_pass`,
  `generate_insights`, `generate_ai_synthesis`); `parse_json_safely()`
  remains as a defensive parsing layer for one minor version per the
  Sprint-4 design plan, then will be removed.

- **T1.4: Audit log schema expansion + content-addressable response cache**
  — `init_audit_log()` accepts `config` and auto-stamps
  `methodology_mode` on every JSONL record. New `log_ai_request()` helper
  records each `ai_complete()` call as an `ai_request` audit decision
  with model, usage, finish_reason, prompt_hash, and request_id. Raw API
  responses are stored content-addressably in `api_responses/{prompt_hash}.json`
  via the new `init_response_cache()`/`cache_response()`/`read_cached_response()`
  module — JSONL stays light, identical requests dedupe on disk, and the
  layout is exactly what `replay_run()` (OS.5) needs. `summarize_audit_log()`
  surfaces new stats: `total_ai_requests`, `total_tokens_used`,
  `ai_requests_by_model`, `methodology_modes_observed`. New decision_types
  declared up-front so Phases B/C/D items don't bounce off validation when
  they land: `provocation_emitted`, `memo_added`, `positionality_recorded`,
  `reflexivity_collapse_detected`, `mode_changed`, `quote_verified`,
  `quote_fabricated`. Schema version bumped 1.0 -> 1.1 (minor: additive,
  pre-1.1 runs remain comparable to post-1.1 runs because the structural
  artifacts read by `compare_runs()` are unchanged). Incidental fix:
  `n_written` counter in `AuditLog`/`ResponseCache` now uses
  environment-backed state so increments survive R's pass-by-value
  semantics — `close_audit_log()`'s "N decisions recorded" log message now
  reflects the actual count instead of always reading 0.

## Pipeline Architecture

pakhom uses a progressive sequential coding pipeline faithful to how manual
reflexive thematic analysis works in NVivo and similar QDA platforms:

1. **Codebook-first learning** from prior manual analyses (QDPX, Excel, CSV)
2. **Progressive sequential coding** -- entries read one at a time, coded inline
3. **Thematic saturation detection** -- triangulated stopping criterion
4. **Code-aware sentiment analysis** -- sentiment scored after coding with code context
5. **Iterative bottom-up theme generation** -- sequential merging of codes into clusters
6. **Deterministic code-path cascading** -- entries map to themes through their codes

## Major Features

- Multi-provider AI support (OpenAI GPT-4o, Anthropic Claude Sonnet 4)
- Progressive sequential coding: entries processed one at a time with a growing
  codebook, just like a human researcher in NVivo. No batch coding, no separate
  deduplication or consolidation steps
- Thematic saturation detection using triangulated signals: code creation rate
  monitoring (Guest et al., 2020), Inductive Thematic Saturation ratio
  (De Paoli & Mathis, 2024, doi:10.1007/s11135-024-01950-6), and AI
  self-assessment. Saturation curve saved for publication
- Iterative bottom-up theme generation: codes are merged through multiple
  sequential passes until no more productive groupings exist. Themes and
  subthemes emerge organically from the data
- Deterministic code-path cascading: entry-to-theme mapping flows through the
  code hierarchy (no AI re-reading of raw text), faithful to the inductive process
- Code-aware sentiment analysis: sentiment scored after initial coding, using
  assigned codes as context for more accurate emotional valence detection
- Codebook-first learning from prior studies: full theme/subtheme/code hierarchy
  extraction from QDPX files (NVivo exports), Excel, and CSV codebooks.
  Manuscripts serve as supplementary clarification only when codebook descriptions
  are insufficient
- Cross-study qualitative synthesis: structural patterns identified across all
  available prior analyses collectively (not just in sequence)
- Researcher review points: pause the pipeline after coding or theme generation
  to curate the AI's output before continuing
- Checkpoint/resume system with flat checkpoint architecture for reliable resume
  across retries and interruptions
- Inter-rater reliability: Cohen's kappa and Krippendorff's alpha with
  small-sample correction for human verification of AI-generated codes
- Reddit scraper with OAuth authentication
- Rich HTML report with interactive DataTables, saturation curves, sentiment-coded
  quotes, theme detail drill-downs, and cross-run comparison dashboard
- Cross-run comparison module with 7 analysis dimensions (sample overlap,
  sentiment drift, code stability, theme evolution, entry migration,
  correlation persistence, run dashboard)
- Dynamic token-aware batch sizing for AI operations
- Interactive Shiny configuration wizard for building config.yaml

## Bug Fixes and Improvements

- Fixed: token-limits table in the methodology appendix now filters to
  the v1.0 task whitelist (`coding`, `theming`, `sentiment`, `review`,
  `insight`, `synthesis`). User configs migrated from earlier versions
  may carry stale `consolidation` / `assignment` / `relevance` keys;
  these are no longer surfaced in the report
- Fixed: `close_audit_log()` is now idempotent. The pause-for-review
  pipeline branches both call `close_audit_log` explicitly and rely on
  an `on.exit` safety net; previously the second call logged a spurious
  "invalid connection" warning. Now silently no-ops when the connection
  is already closed
- Removed: optional `tiktoken` integration in `estimate_tokens()`. The
  package is not on CRAN and was triggering an undeclared-namespace
  warning under `R CMD check --as-cran`. The script-aware character
  heuristic is sufficient for batch-size budgeting (the only place
  token estimation is used) and was already the production path on any
  install without `tiktoken`
- Removed: leftover references to the pre-1.0 architecture in the
  Reddit scraper docstring, `06_manuscript_learning.R` placeholder
  comment, the `compute_dynamic_batches` example text, and the
  `globalVariables` registration of `relevance_score`
- Fixed: thematic-saturation detection no longer fires prematurely on
  longer runs. The previous implementation derived each code's
  birth-time-in-coded-entries from per-checkpoint accumulator lists
  that get reset every checkpoint interval; after the first reset the
  signal collapsed toward zero and saturation could be declared even
  while novel codes were still being created. Now stored directly in
  a parallel `code_n_coded_at_birth` map at the moment of code creation
- Added: `analysis_schema_version` field in `run_metadata.json`. The
  cross-run comparison module (`compare_runs`, `compare_models`,
  `list_available_runs`) is now schema-aware: incompatible snapshots
  are excluded with a clear warning rather than silently NA-padded
- Removed: hardcoded medication-research framing from sentiment and
  cross-study-synthesis prompts. The sentiment system prompt no longer
  asserts "qualitative health research" / "clinical experiences /
  treatment effects" regardless of the user's actual research domain;
  `.synthesize_cross_study_patterns` no longer matches theme names
  against six medication-specific regex categories or injects an
  unconditional medication-narrative-arc claim into the AI's learning
  context. Replaced with a domain-neutral, evidence-based listing of
  the actual top-level themes from each prior codebook
- Removed: `confidence` from substantive correlation analyses
  (`prepare_correlation_data`, `compare_theme_groups`). The AI sentiment
  prompt elicits confidence and emotion_intensity in the same single
  call, so they co-vary by design (r >= 0.83 across all observed runs);
  reporting their correlation as a finding misled readers. Confidence
  remains in the per-entry `sentiment_scores.csv` as a diagnostic
- Fixed: `DT` package (in Suggests) now has graceful fallback to `knitr::kable`
  when not installed
- Fixed: `.html_esc()` now escapes single quotes to prevent XSS in HTML
  attribute contexts
- Fixed: Variable shadowing of exported `n_themes()` function in report builder
- Replaced `library(tidyverse)` with targeted imports in generated Rmd for
  faster rendering
- Added `error = TRUE` to generated Rmd chunks so individual chunk failures
  don't prevent report rendering
- Added warnings when `load_data()` auto-excludes database tables
- Added small-sample-size warnings for theme membership correlations
- Added methodological notes on saturation criteria and pipeline-wide
  multiple testing in report appendix
- Standardized error messages with `validate_class()` helper
- Added `list_available_runs()` convenience function for cross-run comparison
- Added `getting-started` vignette (now also documents Reddit's post-2025
  Responsible Builder Program approval requirement for the optional
  scraper)
- Added `config_wizard_app()` for interactive configuration building
- Removed leftover stale references to the pre-1.0 architecture's
  removed pipeline steps (relevance filtering, batch coding, code
  consolidation, theme assignment) from the report's learning-
  transparency section, the methodology appendix, and the distributed
  default config template
- Cleaned all R CMD check warnings: documented previously undocumented
  function arguments, declared previously implicit base-stats imports
  (`complete.cases`, `shapiro.test`, `wilcox.test`, `fisher.test`,
  `chisq.test`), removed non-ASCII characters from R sources
