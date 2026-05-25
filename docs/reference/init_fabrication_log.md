# Initialize the fabrication log

Creates (or truncates to header) `outputs/<run>/fabrication_log.csv` and
returns a `FabricationLog` S3 object that can be passed to
[`log_fabrication`](https://abanoub-armanious.github.io/pakhom/reference/log_fabrication.md).

## Usage

``` r
init_fabrication_log(output_dir, methodology_mode = NULL)
```

## Arguments

- output_dir:

  Run output directory (where the CSV is written).

- methodology_mode:

  Optional methodology mode (T1.7 / AC4). When non-NULL, the CSV header
  is preceded by a comment-style methodology stamp identifying the mode
  and run id, so a reviewer picking up the bare CSV sees the methodology
  declaration. NULL skips stamping (legacy / test callers).

## Value

A FabricationLog S3 object.

## Details

Each fabricated quote becomes one CSV row with columns: timestamp,
quote_id, source_doc_id, attributed_theme_id, attributed_code_id,
ai_model, ai_call_id, exact_text, verification_status. The CSV format
(rather than JSONL) is deliberate: methodology-paper analyses run R-side
aggregations that are easier on a wide CSV than nested JSONL.
