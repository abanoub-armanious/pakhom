# Export theme review sheet and apply modifications on resume

Exports a CSV where the researcher can rename, merge, split, or delete
generated themes before proceeding to correlations and report
generation.

## Usage

``` r
review_themes(theme_set, output_dir, audit_log = NULL, methodology_mode = NULL)
```

## Arguments

- theme_set:

  ThemeSet S3 object

- output_dir:

  Pipeline output directory

- audit_log:

  Optional AuditLog

- methodology_mode:

  Optional methodology mode (T1.7 / AC4). When non-NULL, the exported
  review and disposition CSVs are stamped with a comment header. NULL
  skips stamping (legacy / test callers).

## Value

List with status and updated theme_set
