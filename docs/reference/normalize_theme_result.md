# Normalize raw AI theme output to canonical ThemeSet

Call this immediately after fromJSON() on any AI response that produces
themes. Handles both data.frame and list formats transparently.

## Usage

``` r
normalize_theme_result(raw_result)
```

## Arguments

- raw_result:

  Parsed JSON from AI (may be df or list)

## Value

ThemeSet S3 object (always list-based internally)
