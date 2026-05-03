# Create a minimal configuration file

Generates a valid YAML config with sensible defaults and writes it to
disk.

## Usage

``` r
create_config(
  study_name = "Untitled Study",
  research_focus,
  concepts = NULL,
  data_path = NULL,
  source_type = "generic",
  output_dir = "outputs",
  provider = "openai",
  config_path = "config.yaml",
  ...
)
```

## Arguments

- study_name:

  Study name

- research_focus:

  Research focus string (required)

- concepts:

  Character vector of core research concepts

- data_path:

  Path to database or CSV file

- source_type:

  Data source type: "reddit", "twitter", "generic", "clinical"

- output_dir:

  Directory for results (default "outputs")

- provider:

  AI provider: "openai" or "anthropic" (default "openai")

- config_path:

  Where to save the YAML (default "config.yaml")

- ...:

  Additional overrides as dot-path = value pairs

## Value

The path to the created config file (invisibly)
