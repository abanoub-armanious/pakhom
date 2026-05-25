# Summarize quote provenance for the report's Tier-0 dashboard

Computes counts + rates from a list of verified quotes. Used by the
Tier-0 dashboard
([`.build_tier0_dashboard`](https://abanoub-armanious.github.io/pakhom/reference/dot-build_tier0_dashboard.md))
and usable programmatically for cross-run analyses (the methodology
paper's KPIs draw from the same shape).

## Usage

``` r
quote_provenance_summary(quotes)
```

## Arguments

- quotes:

  List of QuoteProvenance objects (after `verify_quote`).

## Value

Named list:

- `total`: total quote count

- `by_status`: integer vector keyed by verification_status

- `by_method`: integer vector keyed by verification_method

- `by_citation_source`: integer vector keyed by `citation_source`
  (`"anthropic_citations_api"`, `"model_freeform"`, etc.)

- `verification_rate`: proportion in either verified state

- `fabrication_rate`: proportion fabricated

- `drift_rate`: proportion drifted

- `verification_rate_by_source`: named numeric vector –
  per-citation_source verification rate (verified / total quotes with
  that source). Lets the dashboard expose the differential reliability
  of the prevention layer (citations API) vs the detection-only layer
  (model_freeform + ladder).

- `n_citations_api`: integer count of quotes with
  `citation_source == "anthropic_citations_api"`. Convenience accessor
  for the dashboard's headline KPI.

- `citations_api_rate`: proportion of quotes that came through the
  citations API path (vs. fell back to model_freeform). This is the
  package's empirical answer to "did the prevention layer actually
  engage on this run?".

## Details

Sprint-4 T0.1 part 3b adds the `by_citation_source` breakdown and
`verification_rate_by_source` so the dashboard can distinguish Anthropic
Citations API quotes (server-side-grounded by Anthropic plus
client-verified by our ladder) from model-freeform quotes (client-
verified only). Both are valid; the citations source is strictly
stronger.
