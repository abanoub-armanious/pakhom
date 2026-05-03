# Export coding results to QDPX format

Creates a QDPX file (ZIP archive) that can be imported into ATLAS.ti,
NVivo, MAXQDA, and other qualitative data analysis software that
supports the QDPX exchange standard.

## Usage

``` r
export_qdpx(
  coding_state,
  data,
  output_path,
  theme_set = NULL,
  study_name = "pakhom export"
)
```

## Arguments

- coding_state:

  `ProgressiveCodingState` object containing `$codebook` and
  `$entry_results`.

- data:

  Tibble with at least `std_id` and `std_text` columns.

- output_path:

  File path for the `.qdpx` file to create.

- theme_set:

  Optional `ThemeSet` object. If provided, builds a hierarchical code
  tree (Theme \> Subtheme \> Code).

- study_name:

  Character string used as the project name inside the QDPX file.
  Defaults to `"pakhom export"`.

## Value

The `output_path` (invisibly), or stops with an error.

## Details

The archive contains:

- `project.qde` — XML file with the codebook structure and all coding
  references (text selections linked to codes).

- `sources/` — directory of plain-text files, one per entry.

When a `theme_set` is provided the codebook is exported hierarchically:
Theme (non-codable) \> Subtheme (non-codable) \> Code (codable leaf).
Without a theme set, codes are exported as a flat list.
