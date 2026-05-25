# Aggregate verification stats across all coded segments in a coding state

Walks `coding_state$codebook`, extracts the `$provenance` field attached
to each coded segment by `.code_entry_progressive` (T0.1 part 2 wiring),
and feeds them through `quote_provenance_summary`.

## Usage

``` r
compute_quote_provenance_stats(coding_state)
```

## Arguments

- coding_state:

  ProgressiveCodingState (or NULL).

## Value

The list returned by
[`quote_provenance_summary`](https://abanoub-armanious.github.io/pakhom/reference/quote_provenance_summary.md).

## Details

Returns the empty-summary shape (zero counts, NA rates) when:

- `coding_state` is NULL (e.g., the run skipped coding)

- `coding_state` predates the T0.1 wiring and has no `$provenance` on
  its segments (legacy runs)

- the codebook is empty

This helper is what the report's Tier-0 dashboard reads – so the
empty-summary fallback is load-bearing for back-compat: pre-T0.1 runs
still render a dashboard, just one that says "verification not run".
