# Launch the interactive configuration wizard

Opens a Shiny app that walks you through every configuration option with
descriptions, validation, and sensible defaults. When finished, it
writes a validated `config.yaml` to disk.

## Usage

``` r
config_wizard_app(output_path = "config.yaml")
```

## Arguments

- output_path:

  Where to save the generated config (default "config.yaml"). The user
  can also change this in the UI.

## Value

The path to the created config file (invisibly). Returns NULL if the
user closes the app without saving.

## Details

This is the web-based companion to the CLI-based
[`config_wizard()`](https://abanoub-armanious.github.io/pakhom/reference/config_wizard.md).
Both produce identical YAML output.
