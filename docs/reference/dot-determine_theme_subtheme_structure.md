# Determine theme and subtheme structure from merge passes

After iterative merging, the merge history determines the hierarchy:

- Items merged in the LAST productive pass become themes

- If a theme was formed by merging clusters from a PREVIOUS pass, those
  previous-pass clusters become subthemes

- Codes that were never merged become standalone themes (single-code
  themes)

## Usage

``` r
.determine_theme_subtheme_structure(items, merge_history, coding_state)
```

## Arguments

- items:

  Final list of items after all merge passes

- merge_history:

  List tracking merge passes

- coding_state:

  ProgressiveCodingState

## Value

List with themes (each having optional subthemes)
