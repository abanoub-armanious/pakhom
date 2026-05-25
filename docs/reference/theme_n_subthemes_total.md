# Total real (named) subthemes across every depth of a theme

Phase 58 Tier 1 AF-3: with C-12's recursive walker, subthemes can nest.
This getter counts every named subtheme regardless of depth so
downstream consumers can report the "true" decomposition size of a
theme.

## Usage

``` r
theme_n_subthemes_total(theme)
```

## Arguments

- theme:

  A theme list

## Value

Integer; named-subtheme count across all depths.
