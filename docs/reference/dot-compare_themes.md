# Compare themes across runs using fuzzy matching

Compare themes across runs using fuzzy matching

## Usage

``` r
.compare_themes(snapshots, current, threshold = 0.75)
```

## Arguments

- snapshots:

  List of RunSnapshot objects

- current:

  RunSnapshot for the current run

- threshold:

  Jaro-Winkler similarity threshold

## Value

List with pairwise and timeline
