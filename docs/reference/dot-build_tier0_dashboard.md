# Build the Tier-0 data integrity dashboard markdown for the report

Renders a markdown card that summarizes the run's anti-fabrication
verification work: how many AI-attributed verbatim claims were checked,
how many verified exactly vs fuzzy (with method breakdown), how many
fabrications were dropped, and a relative path link to the
`fabrication_log.csv` when fabrications occurred.

## Usage

``` r
.build_tier0_dashboard(
  stats,
  fabrication_log_relpath = "fabrication_log.csv",
  config = NULL,
  fabrication_log_path = NULL,
  n_fabricated_caught = NULL
)
```

## Arguments

- stats:

  Named list returned by
  [`compute_quote_provenance_stats`](https://abanoub-armanious.github.io/pakhom/reference/compute_quote_provenance_stats.md).

- fabrication_log_relpath:

  Optional relative path to `fabrication_log.csv` (relative to the
  report HTML's directory). When NULL or no fabrications occurred, no
  link is rendered.

- config:

  ThematicConfig object (or NULL) used for the Citations API bypass
  footnote.

- fabrication_log_path:

  Phase 58 Tier 4 V-5: absolute path to `fabrication_log.csv`. When
  supplied, the dashboard counts pre-rejection fabrications from this
  file (the surviving population in `stats` is post-rejection, so it
  always reports 0 caught fabrications by itself). Pass `NULL` on legacy
  callers that don't have the path; the dashboard falls back to the
  surviving-population count.

- n_fabricated_caught:

  Phase 58 Tier 4 V-5: explicit count of pre-rejection fabrications
  (from `FabricationLog$state$n_logged`). Overrides
  `fabrication_log_path` when supplied. Pass `NULL` to skip.

## Value

A character string of markdown content (one card).

## Details

This dashboard is the user-visible artifact of T0.1's universal
verification contract – it makes the package's anti-fabrication work
empirically inspectable from the report itself, addressing the
transparency dimension of Jowsey et al. 2025's critique. When no
verification was run (pre-T0.1 runs, or runs that skipped coding), the
dashboard renders a "Verification not available" notice rather than
silently omitting – absence of the badge would be its own integrity
signal that we don't want to send.
