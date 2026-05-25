# Current ResearcherReflectionLog schema version

1.0.0 – initial schema (phase 30): provocations + memos +
positionality_history + reflexivity_collapse_flags +
researcher_authored_codes + researcher_authored_themes. 1.1.0 – phase 31
(run_mode1 orchestrator): add provocation_attempts and skipped_themes
data.frames so Mode 1 can honestly assert T0.3 coverage.
provocation_attempts records one row per (theme, category) attempt
regardless of how many provocations the AI emitted; the distinction
matters because a category that legitimately returns zero provocations
(e.g., counter_narrative finds no qualifying entries) is NOT a coverage
failure – whereas a category that was never attempted IS. skipped_themes
records themes the orchestrator bypassed (e.g., zero supporting entries)
with an explicit reason, so the coverage card distinguishes "silent
skip" from "explicit skip with stated reason." 1.2.0 – phase 33 (M1.3
reflexive memos): the memos slot now holds a list of typed `Memo` S3
objects rather than an unstructured list. The Memo schema (id,
timestamp, author, type, linked_codes, linked_themes, linked_entries,
linked_prior_memo, body) supports Markdown round-trip with YAML
frontmatter (per SPRINT4_DESIGN.md M1.3 spec line 277-298) and is the
AC6 burden-parity counterpart to Modes 2/3's codebook + theme review
pause-points. CRUD via `add_memo`, `read_memo`, `list_memos`;
persistence via `persist_memos` / `load_memos`. Backward-compatible:
1.1.0 logs with a list of pre-Memo entries are kept in place (the new
code paths gate on `inherits(m, "Memo")`).

## Usage

``` r
.RESEARCHER_REFLECTION_LOG_SCHEMA_VERSION
```

## Format

An object of class `character` of length 1.
