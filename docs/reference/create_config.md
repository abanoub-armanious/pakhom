# Create a minimal configuration file

Generates a valid YAML config with sensible defaults and writes it to
disk. Per T1.3 (phase 25-27) the `methodology` block is mandatory in
every config – this helper writes it for you given the `methodology`
argument.

## Usage

``` r
create_config(
  methodology = "codebook_collaborative",
  study_name = "Untitled Study",
  research_focus = NULL,
  framework_spec_path = NULL,
  database_path = NULL,
  output_path = "config.yaml",
  concepts = NULL,
  source_type = "generic",
  output_dir = "outputs",
  provider = "openai",
  ...
)
```

## Arguments

- methodology:

  Methodology mode (mandatory): one of `"reflexive_scaffold"` (Mode 1),
  `"codebook_collaborative"` (Mode 2; the default), or
  `"framework_applied"` (Mode 3). Mode 3 also requires
  `framework_spec_path`.

- study_name:

  Study name (default `"Untitled Study"`).

- research_focus:

  Research focus string. Required when no `...` override supplies it.

- framework_spec_path:

  Path to a framework spec YAML/JSON OR a built-in alias (`"tpb"`,
  `"comb"`, `"tdf"`). Required when `methodology = "framework_applied"`;
  ignored otherwise.

- database_path:

  Path to the SQLite database (alias for the internal `data$database`
  field). Modes 2/3 require a database; Mode 1 may use a tibble passed
  directly to
  [`run_mode1()`](https://abanoub-armanious.github.io/pakhom/reference/run_mode1.md)
  so the database can be NULL for Mode 1 configs.

- output_path:

  Where to save the YAML config file (default `"config.yaml"`). The
  output_path alias matches the README + vignette quickstart calls.

- concepts:

  Character vector of core research concepts (informs progressive coding
  prompts).

- source_type:

  Data source type: `"reddit"`, `"twitter"`, `"generic"`, `"clinical"`
  (default `"generic"`).

- output_dir:

  Directory for analysis results (default `"outputs"`).

- provider:

  AI provider: `"openai"` or `"anthropic"` (default `"openai"`).

- ...:

  Additional overrides as dot-path = value pairs (e.g.,
  `analysis.test_mode.enabled = TRUE`).

## Value

The path to the created config file (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
# Mode 2 (Codebook Collaborative)
create_config(
  methodology = "codebook_collaborative",
  study_name = "My Study",
  research_focus = "How does X relate to Y?",
  database_path = "my_data.db",
  output_path = "config.yaml"
)

# Mode 3 (Framework Applied) with a built-in framework
create_config(
  methodology = "framework_applied",
  framework_spec_path = "tpb",
  study_name = "TPB analysis",
  research_focus = "Behavioral intention -> behavior",
  database_path = "my_data.db",
  output_path = "config.yaml"
)

# Mode 1 (Reflexive Scaffold) -- corpus is supplied at run_mode1() time
create_config(
  methodology = "reflexive_scaffold",
  study_name = "Reflexive analysis",
  research_focus = "Provocation against my coded themes",
  output_path = "config.yaml"
)
} # }
```
