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
  cooccurrence_tests = NULL
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

## Value

Path to generated HTML report
