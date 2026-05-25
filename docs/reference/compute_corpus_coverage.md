# Compute corpus coverage from a completed coding run

Asserts the LLM saw every entry that survived preprocessing. Returns a
`CorpusCoverage` S3 object summarising the funnel from preprocessed data
to LLM-processed entries to coded entries, plus the
`no_silent_truncation` flag that pakhom uses as the headline Tier-0
assertion.

## Usage

``` r
compute_corpus_coverage(
  coding_state,
  data,
  n_raw_loaded = NA_integer_,
  n_after_preprocessing = NA_integer_,
  test_mode_sample_size = NA_integer_
)
```

## Arguments

- coding_state:

  A finalized `ProgressiveCodingState` (the one returned by
  [`run_progressive_coding`](https://abanoub-armanious.github.io/pakhom/reference/run_progressive_coding.md)).

- data:

  The standardized + preprocessed tibble that was fed to the coding step
  (must have `std_id` and `std_text`). Used to compute byte counts and
  to verify every entry has a matching `entry_results` record.

- n_raw_loaded:

  Optional integer: rows loaded from the database before preprocessing.
  `NA_integer_` when unknown (e.g., resumed run where the raw count
  wasn't preserved across the checkpoint).

- n_after_preprocessing:

  Optional integer: rows after preprocessing but before any test-mode
  sampling. Defaults to `NA_integer_`.

- test_mode_sample_size:

  Optional integer: when test mode was on, the sub-sample size used.
  `NA_integer_` when test mode was off.

## Value

A `CorpusCoverage` S3 object (a list with class).

## Details

Pre-preprocessing counts (e.g., raw rows from the database before
deduplication and length filtering) can be supplied via `n_raw_loaded`
and `n_after_preprocessing`; when omitted, the coverage object reports
them as `NA_integer_` and the card degrades gracefully. The headline
assertion (no silent truncation in the LLM call path) does not depend on
pre-preprocessing counts.
