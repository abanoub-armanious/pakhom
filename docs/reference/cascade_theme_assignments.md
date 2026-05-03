# Cascade theme assignments from codes to entries deterministically

For each entry, looks up its assigned codes, maps each code to a theme
(and optionally a subtheme) via the merge history, and marks the entry's
theme memberships. An entry belongs to EVERY theme that contains any of
its codes – there is no primary/secondary distinction.

## Usage

``` r
cascade_theme_assignments(data, coding_state, theme_set)
```

## Arguments

- data:

  Tibble with std_id column

- coding_state:

  ProgressiveCodingState

- theme_set:

  ThemeSet with merge_history attached

## Value

Tibble with emerged_themes, theme_membership\_\* columns, and
subtheme_assignments added
