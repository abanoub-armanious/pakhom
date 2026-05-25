# Persist a CorpusCoverage / ProvocationCoverage object to disk

Phase 58 Tier 8 H-10: pre-Tier-8 the CorpusCoverage S3 was computed in
memory and rendered as HTML but never written to disk as
machine-readable data. A reproducibility audit couldn't read coverage
state without re-running the pipeline. This writer serializes the full
coverage object as `coverage_card.json` alongside the report HTML,
methodology-stamped per AC4.

## Usage

``` r
write_corpus_coverage(coverage, output_dir, methodology_mode = NULL)
```

## Arguments

- coverage:

  CorpusCoverage / ProvocationCoverage / Tier0Coverage object from
  [`compute_corpus_coverage`](https://abanoub-armanious.github.io/pakhom/reference/compute_corpus_coverage.md).

- output_dir:

  Run output directory.

- methodology_mode:

  Optional methodology mode for AC4 stamping.

## Value

Invisible path to the written JSON.

## Details

The JSON shape preserves every field on the S3 (`n_input_to_- coding`,
`n_processed`, `n_unprocessed`, `n_skipped`, `n_coded`, `skip_reasons`,
`words_processed`, `coverage_rate`, `no_silent_truncation`,
`stop_- reason`, `saturation_reached`, `reached_at_entry`, etc.) so a
downstream consumer can reconstruct the funnel + saturation state
without the original coding_state.
