# Run the full thematic analysis pipeline

Orchestrates all steps from data loading through report generation.
Supports checkpoint/resume for expensive API operations.

## Usage

``` r
run_analysis(config_path, resume = FALSE, config_overrides = list())
```

## Arguments

- config_path:

  Path to YAML config file

- resume:

  Logical; if TRUE, resume from last checkpoint

- config_overrides:

  Named list of dot-path overrides

## Value

List with data, analytic_data, coding_state, theme_set, correlations,
etc.
