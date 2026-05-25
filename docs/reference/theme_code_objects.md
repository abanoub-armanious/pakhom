# Flatten Code S3 objects across all subthemes (and sub-subthemes) of a theme

Phase 58 Tier 1 C-12: now recurses through nested Subthemes so codes in
sub-subthemes are included. Pre-Phase-58 ThemeSets without nesting are
unaffected (the recursion bottoms out at depth 1).

## Usage

``` r
theme_code_objects(theme)
```

## Arguments

- theme:

  A theme list (one element of theme_set\$themes)

## Value

List of Code S3 objects
