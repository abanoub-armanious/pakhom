# ProvocationCoverage schema version

1.0.0 – initial phase 31 release. 2.0.0 – phase 31 audit fixes (audit C:
C1 + H1 + H2 + H3 + M3):

- `explicit_skip_reasons` and `attempts_per_category` stored as named
  lists (not named integer vectors) so they serialize faithfully via
  [`jsonlite::write_json`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html)
  (the coverage_mode1.json artifact is the canonical reviewable record
  per AC4).

- Added `n_unexpected_category_attempts` and `unexpected_categories`
  fields to surface attempts whose category is outside
  `requested_categories` as a distinct anomaly (previously silently
  miscounted).

- `no_silent_skip` headline now requires `n_themes_input > 0` AND
  `n_themes_attempted > 0` so degenerate states (zero-themes input,
  all-themes-explicit- skipped) don't grade as verified coverage.

- Replaced the unconditional `no_silent_corpus_truncation` boolean
  (which overclaimed – the per-category prompts include only
  theme-supporting entries, not the full corpus text) with two honest
  fields: `corpus_provided_to_per_category_fns` (TRUE – the data tibble
  IS passed) and `llm_prompt_includes_full_corpus` (FALSE – current
  prompts only embed supporting-entry text). 2.1.0 – phase 33 (M1.3
  reflexive memos): added `n_memos` and `memos_by_type` informational
  fields. Memo writing is a researcher activity, not a pipeline gate –
  the headline `no_silent_skip` boolean is unchanged. The fields exist
  so the methodology paper can report researcher-side burden as a KPI
  alongside AI-side coverage.

## Usage

``` r
.PROVOCATION_COVERAGE_SCHEMA_VERSION
```

## Format

An object of class `character` of length 1.
