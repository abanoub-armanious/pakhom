# Rebuild code-to-theme mapping after researcher restructuring

After the researcher modifies the theme structure (reassigning codes,
creating/splitting themes), the merge_history\$code_to_theme_map becomes
stale. This function rebuilds it from the current ThemeSet by walking
all themes and resolving code names back to code keys via the codebook.

## Usage

``` r
rebuild_code_to_theme_map(theme_set, coding_state)
```

## Arguments

- theme_set:

  ThemeSet with modified themes

- coding_state:

  ProgressiveCodingState for code name → key resolution

## Value

ThemeSet with updated merge_history\$code_to_theme_map
