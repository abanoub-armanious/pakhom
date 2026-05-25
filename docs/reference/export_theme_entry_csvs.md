# Export CSV files for each theme's entries

Export CSV files for each theme's entries

## Usage

``` r
export_theme_entry_csvs(data, theme_set, output_dir, methodology_mode = NULL)
```

## Arguments

- data:

  tibble with theme_membership\_\* or emerged_themes columns

- theme_set:

  ThemeSet object

- output_dir:

  Output directory

- methodology_mode:

  Optional methodology mode (T1.7). When non-NULL, every CSV produced is
  stamped with a comment header identifying the mode and run id (per
  AC4). NULL skips stamping – used by tests / legacy callers.

## Value

Named list of file info per theme
