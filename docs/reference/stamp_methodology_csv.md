# Stamp a CSV file with a methodology comment header

Writes a comment-style header line and an empty separator before the CSV
body. R's
[`readr::read_csv`](https://readr.tidyverse.org/reference/read_delim.html)
and [`utils::read.csv`](https://rdrr.io/r/utils/read.table.html) both
accept a `comment` arg that skips lines starting with `#`; downstream
consumers using those parsers transparently strip the stamp. Consumers
using a stricter parser see the comment and can re-export without it.

## Usage

``` r
stamp_methodology_csv(csv_path, mode, run_id = NULL)
```

## Arguments

- csv_path:

  Path to a CSV file (will be re-written with the stamp).

- mode:

  Character methodology mode.

- run_id:

  Optional run identifier.

## Value

Invisibly returns `csv_path`.

## Details

Two-line stamp (kept short for tabular tools that DON'T strip comments –
they'll still parse the data starting line 3):


    # methodology: M1 - Reflexive Scaffold | run: run_2026-...
    #
    col1,col2,col3
    ...
