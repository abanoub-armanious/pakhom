# Create a ThemeSet object (canonical internal representation)

Accepts both the new hierarchy shape (themes with first-class Subtheme
S3 objects) and the legacy flat shape (themes with codes_included
character vectors). Legacy input is wrapped into a single virtual
Subtheme per theme.

## Usage

``` r
create_theme_set(
  themes,
  thematic_map = "",
  analysis_notes = "",
  review_notes = NULL,
  split_history = NULL
)
```

## Arguments

- themes:

  List of theme lists. Each theme requires id and name; codes and
  subthemes follow either the new (subthemes = list of Subtheme S3) or
  legacy (codes_included = character vector, subthemes = character
  vector) shape.

- thematic_map:

  Character description of inter-theme relationships

- analysis_notes:

  Character reflexive notes

- review_notes:

  List of review results (or NULL)

- split_history:

  List tracking any theme splits performed

## Value

ThemeSet S3 object
