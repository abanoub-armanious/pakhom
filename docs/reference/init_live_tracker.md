# Initialize the live tracker for a run

Creates (or truncates) the three artifact files in `<output_dir>/live/`
and returns a `LiveTracker` S3 object that can be passed to
[`run_progressive_coding()`](https://abanoub-armanious.github.io/pakhom/reference/run_progressive_coding.md),
[`generate_themes_iterative()`](https://abanoub-armanious.github.io/pakhom/reference/generate_themes_iterative.md),
and other pipeline functions.

## Usage

``` r
init_live_tracker(
  output_dir,
  codebook_snapshot_every = .LIVE_CODEBOOK_SNAPSHOT_EVERY
)
```

## Arguments

- output_dir:

  Run directory (the parent; this function creates a `live/`
  subdirectory inside).

- codebook_snapshot_every:

  Integer; rewrite codebook_live.json every N entries (default 1 = after
  every entry).

## Value

`LiveTracker` S3 object.

## Details

Pass `NULL` to any of these functions to disable live tracking entirely
(for tests, mock pipelines, or runs that don't need it).
