# Create a ThemeSet object (canonical internal representation)

Create a ThemeSet object (canonical internal representation)

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

  List of theme lists, each with at minimum: id, name, description,
  codes_included

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
