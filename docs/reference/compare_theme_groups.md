# Compare continuous variables across theme groups using Mann-Whitney U tests

For each binary theme membership column, tests whether sentiment,
emotion intensity, and confidence differ between theme members and
non-members.

## Usage

``` r
compare_theme_groups(data, theme_set, config = list())
```

## Arguments

- data:

  Tibble with theme_membership\_\* and sentiment columns

- theme_set:

  ThemeSet object

- config:

  Correlation config section

## Value

Tibble with test results per theme-variable pair
