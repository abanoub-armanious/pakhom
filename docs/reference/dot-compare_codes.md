# Compare consolidated codes across runs

Compare consolidated codes across runs

## Usage

``` r
.compare_codes(snapshots, current, threshold = 0.75)
```

## Arguments

- snapshots:

  List of RunSnapshot objects

- current:

  RunSnapshot for the current run

- threshold:

  Jaro-Winkler threshold

## Value

List with pairwise, stability, all_runs
