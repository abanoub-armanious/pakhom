# Export theme review sheet and apply modifications on resume

Exports a CSV where the researcher can rename, merge, split, or delete
generated themes before proceeding to correlations and report
generation.

## Usage

``` r
review_themes(theme_set, output_dir, audit_log = NULL)
```

## Arguments

- theme_set:

  ThemeSet S3 object

- output_dir:

  Pipeline output directory

## Value

List with status and updated theme_set
