# Build a reverse map from code_name -\> list(theme, subtheme) using ThemeSet

Walks the ThemeSet structure and its merge_history (if present) to
figure out which theme and subtheme each code belongs to.

## Usage

``` r
.build_code_hierarchy(theme_set, coding_state)
```

## Arguments

- theme_set:

  ThemeSet object

- coding_state:

  ProgressiveCodingState (for codebook key -\> name lookup)

## Value

Named list keyed by code_name, each value a list with `theme_name` and
`subtheme_name` (the latter may be NULL).
