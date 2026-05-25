# Build an HTML methodology badge for the report header

Renders a small div with the mode label and short description. Designed
to live near the top of the generated report, just below the title.

## Usage

``` r
stamp_methodology_html(mode, run_id = NULL)
```

## Arguments

- mode:

  Character methodology mode.

- run_id:

  Optional character run identifier; rendered alongside the mode if
  supplied.

## Value

Character HTML.
