# Load analysis configuration from YAML file

Load analysis configuration from YAML file

## Usage

``` r
load_config(config_path, overrides = list())
```

## Arguments

- config_path:

  Path to YAML config file

- overrides:

  Named list of overrides (dot-separated keys, e.g., list("ai.provider"
  = "anthropic"))

## Value

A validated ThematicConfig S3 object
