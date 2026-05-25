# Aggregate verification stats across all provocations in a reflection log

Mode 1's analog of
[`compute_quote_provenance_stats`](https://abanoub-armanious.github.io/pakhom/reference/compute_quote_provenance_stats.md).
Walks `reflection_log$provocations`, extracts each provocation's
`$provenance` field (a `QuoteProvenance` object built and verified by
the per-category function – see
R/provocateur.R::.citation_to_provocation), and feeds them through
[`quote_provenance_summary`](https://abanoub-armanious.github.io/pakhom/reference/quote_provenance_summary.md).

## Usage

``` r
compute_provocation_provenance_stats(reflection_log)
```

## Arguments

- reflection_log:

  A `ResearcherReflectionLog`, or NULL.

## Value

The list returned by
[`quote_provenance_summary`](https://abanoub-armanious.github.io/pakhom/reference/quote_provenance_summary.md).

## Details

Provocations from observational categories (absent_voice, parts of
assumption_surfacing) carry NULL provenance because the AI is reasoning
ABOUT the data rather than quoting it; those are excluded from the
verification stats (the Tier-0 dashboard's domain is verbatim claims).
