# Build the project.qde XML document

Build the project.qde XML document

## Usage

``` r
.build_qde_xml(
  coding_state,
  data,
  source_paths,
  theme_set = NULL,
  study_name = "pakhom export"
)
```

## Arguments

- coding_state:

  ProgressiveCodingState

- data:

  Tibble with std_id, std_text columns

- source_paths:

  Named character vector from `.write_source_files`

- theme_set:

  Optional ThemeSet for hierarchical codes

- study_name:

  Character study name

## Value

xml2 document
