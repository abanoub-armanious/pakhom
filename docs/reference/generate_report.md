# Generate the full HTML analysis report

Builds an Rmd file from data and renders it to HTML.

## Usage

``` r
generate_report(
  data,
  theme_set,
  correlations_df,
  insights,
  export_files,
  consolidated = NULL,
  learning_context = NULL,
  provider = NULL,
  config = NULL,
  output_file = "analysis_report.html",
  irr_result = NULL,
  comparison_result = NULL,
  self_contained = TRUE,
  coding_results = NULL,
  coding_state = NULL,
  excerpt_verification = NULL,
  theme_group_tests = NULL,
  cooccurrence_tests = NULL,
  audit_log = NULL,
  response_cache = NULL,
  coverage = NULL,
  framework_spec = NULL,
  framework_archive = NULL
)
```

## Arguments

- data:

  tibble with all analysis columns

- theme_set:

  ThemeSet object

- correlations_df:

  Correlations tibble

- insights:

  Insights list

- export_files:

  List of export file paths

- consolidated:

  ConsolidatedCodes list (or NULL)

- learning_context:

  LearningContext object (or NULL)

- provider:

  AIProvider object (or NULL)

- config:

  ThematicConfig object (or NULL)

- output_file:

  Path for the HTML report

- irr_result:

  Inter-rater reliability result list (or NULL)

- comparison_result:

  ComparisonResult object from compare_runs() (or NULL)

- self_contained:

  If TRUE (default), produce a self-contained HTML file with all
  resources embedded. Set to FALSE for faster rendering and smaller file
  size (external CSS/JS will be referenced).

- coding_results:

  Legacy CodingResults list as returned by `as_coding_results`. Used to
  populate per-theme entry tables with the codes assigned to each entry.
  Pass NULL to omit the codes column.

- coding_state:

  ProgressiveCodingState with saturation data (or NULL)

- excerpt_verification:

  Optional list returned by `verify_excerpts` containing substring_stats
  and (optionally) coherence_stats. When provided, the report's
  data-quality appendix shows excerpt validation results.

- theme_group_tests:

  Optional tibble returned by `compare_theme_groups` (Mann-Whitney U
  tests). When provided, the correlation section gains a 'Theme Group
  Comparisons' subsection.

- cooccurrence_tests:

  Optional tibble returned by `test_theme_cooccurrence` (chi-square /
  Fisher tests). When provided, the correlation section gains a 'Theme
  Co-occurrence' subsection.

- audit_log:

  Optional `AuditLog` object (T1.4) forwarded to `generate_ai_synthesis`
  so the executive-summary AI call is recorded as an `ai_request` audit
  decision.

- response_cache:

  Optional `ResponseCache` object (T1.4) forwarded to
  `generate_ai_synthesis` so the raw API response is written to the
  cache and referenced from the audit log.

- coverage:

  Optional `CorpusCoverage` object (T0.3) from
  [`compute_corpus_coverage`](https://abanoub-armanious.github.io/pakhom/reference/compute_corpus_coverage.md).
  When provided, the report renders a Tier-0 corpus-coverage card
  asserting that every entry surviving preprocessing reached the LLM (no
  silent truncation). When NULL the card renders an explicit "coverage
  not computed" notice rather than silently omitting – absence is itself
  a transparency signal per AC4.

- framework_spec:

  Optional `FrameworkSpec` object (Mode 3 only). When provided AND
  `config$methodology$mode` is `"framework_applied"`, the report renders
  a Framework Declaration section with the framework's name, citations,
  epistemic stance, anomaly handling policy, and full constructs list.
  NULL on Mode 1 / Mode 2 runs.

- framework_archive:

  Optional named list returned by
  [`archive_framework_spec`](https://abanoub-armanious.github.io/pakhom/reference/archive_framework_spec.md)
  carrying the archived framework's path + sha256 hash. When provided
  alongside `framework_spec`, the Framework Declaration section includes
  the sha256 fingerprint and a link to the archived spec.

## Value

Path to generated HTML report
