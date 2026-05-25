# Default codebook-snapshot rewrite cadence

How many entries between codebook_live.json rewrites. Set to 1 for
"after every entry"; default to 1 because the snapshot is small enough
that atomic rewrite is cheap (10ms even for a 500-code codebook). Phase
56's performance pass may revisit this.

## Usage

``` r
.LIVE_CODEBOOK_SNAPSHOT_EVERY
```

## Format

An object of class `integer` of length 1.
