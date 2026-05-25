# Number of TOP-LEVEL real subthemes in a theme (excludes virtual wrappers)

Phase 58 Tier 1 AF-3: this counter is unchanged from Phase 51 – it
counts only the immediate (depth-1) named subthemes of the theme.
Virtual (NA-named) subthemes are excluded; nested sub-subthemes are NOT
counted. For "all real subthemes at every depth" use
[`theme_n_subthemes_total()`](https://abanoub-armanious.github.io/pakhom/reference/theme_n_subthemes_total.md).
For "raw structural count including virtual wrappers" use
`length(theme$subthemes)`.

## Usage

``` r
theme_n_subthemes(theme)
```

## Arguments

- theme:

  A theme list

## Value

Integer; depth-1 named subtheme count.
