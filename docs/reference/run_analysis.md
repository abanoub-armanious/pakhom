# Run the full thematic analysis pipeline (Mode 2 + Mode 3)

Orchestrates all steps from data loading through report generation.
Supports checkpoint/resume for expensive API operations. Drives Modes 2
(Codebook Collaborative) and 3 (Framework Applied); Mode 1 (Reflexive
Scaffold) uses
[`run_mode1`](https://abanoub-armanious.github.io/pakhom/reference/run_mode1.md)
instead.

## Usage

``` r
run_analysis(config_path, resume = FALSE, config_overrides = list())
```

## Arguments

- config_path:

  Path to YAML config file. The config must declare `methodology$mode`
  as one of `"codebook_collaborative"` or `"framework_applied"` (Mode 1
  has its own entry point). For Mode 3, the config must also set
  `methodology$framework_spec_path` to a built-in framework alias
  (`"tpb"`, `"comb"`, `"tdf"`) or a path to a custom framework
  YAML/JSON.

- resume:

  Logical; if TRUE, resume from last checkpoint. Per AC5 (soft-lock with
  audit trail), a finalized run cannot be resumed in place – doing so
  would overwrite the canonical outputs without a fork record. Use
  [`clone_run_with_new_mode`](https://abanoub-armanious.github.io/pakhom/reference/clone_run_with_new_mode.md)
  to fork into a new run dir.

- config_overrides:

  Named list of dot-path overrides applied after config load. Useful for
  batch runs.

## Value

Invisible list with `data`, `analytic_data`, `coding_state`,
`theme_set`, `correlations`, `insights`, `learning_context`,
`comparison_result`, `export_files`, `output_dir`, `config`,
`integrity`.

## See also

[`run_mode1`](https://abanoub-armanious.github.io/pakhom/reference/run_mode1.md)
(Mode 1 entry point);
[`create_config`](https://abanoub-armanious.github.io/pakhom/reference/create_config.md)
(config builder);
[`load_framework_spec`](https://abanoub-armanious.github.io/pakhom/reference/load_framework_spec.md)
(Mode 3 framework loader);
[`vignette("methodology-modes")`](https://abanoub-armanious.github.io/pakhom/articles/methodology-modes.md)
(per-mode worked examples).

## Examples

``` r
if (FALSE) { # \dontrun{
# Mode 2 (Codebook Collaborative) -- the auto-pipeline
create_config(
  methodology = "codebook_collaborative",
  study_name = "My Study",
  research_focus = "How does X relate to Y?",
  database_path = "my_data.db",
  output_path = "config.yaml"
)
result <- run_analysis("config.yaml")

# Mode 3 (Framework Applied) -- apply a theoretical framework
create_config(
  methodology = "framework_applied",
  framework_spec_path = "tpb",  # built-in TPB; or path to custom spec
  study_name = "TPB analysis",
  research_focus = "Behavioral intention -> behavior",
  database_path = "my_data.db",
  output_path = "config.yaml"
)
result <- run_analysis("config.yaml")
} # }
```
