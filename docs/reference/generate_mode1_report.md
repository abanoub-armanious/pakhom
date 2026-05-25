# Generate the Mode 1 (Reflexive Scaffold) HTML analysis report

Mode 1's analog of
[`generate_report`](https://abanoub-armanious.github.io/pakhom/reference/generate_report.md).
Builds an Rmd, copies the shared CSS, and renders to HTML via
[`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
Called from
[`run_mode1`](https://abanoub-armanious.github.io/pakhom/reference/run_mode1.md);
can also be called directly with a previously-saved reflection_log +
theme_set if a report needs to be re-rendered after the fact.

## Usage

``` r
generate_mode1_report(
  data,
  theme_set,
  reflection_log,
  coverage = NULL,
  theme_stats = NULL,
  config = NULL,
  provider = NULL,
  audit_log = NULL,
  response_cache = NULL,
  fabrication_log = NULL,
  output_file = "analysis_report.html",
  self_contained = TRUE
)
```

## Arguments

- data:

  Tibble: standardized corpus.

- theme_set:

  ThemeSet: researcher-authored themes.

- reflection_log:

  ResearcherReflectionLog: provocateur output.

- coverage:

  ProvocationCoverage (or NULL).

- theme_stats:

  Named list returned by
  [`compute_mode1_theme_stats`](https://abanoub-armanious.github.io/pakhom/reference/compute_mode1_theme_stats.md).

- config:

  ThematicConfig (or list).

- provider:

  Optional AIProvider (currently unused; kept for signature parity +
  future AI-synthesis layer).

- audit_log:

  Optional AuditLog (currently unused; kept for parity).

- response_cache:

  Optional ResponseCache (currently unused).

- fabrication_log:

  Optional FabricationLog (currently unused).

- output_file:

  Path to the HTML output file.

- self_contained:

  Logical; if TRUE (default), produces a single self-contained HTML.

## Value

Path to the generated HTML on success, NULL on failure.
