# Run a Mode 1 (Reflexive Scaffold) provocateur analysis

Top-level Mode 1 entry point. Where
[`run_analysis`](https://abanoub-armanious.github.io/pakhom/reference/run_analysis.md)
runs the Mode 2/3 inductive-/framework-coding pipeline, `run_mode1`
orchestrates the provocateur loop with the same scaffolding (output
directory + run_metadata.json + methodology rules + audit log +
fabrication log + finalize_run + report) so a Mode 1 run produces a
canonical reviewable artifact set under `outputs/<run-id>-rs/`.

## Usage

``` r
run_mode1(
  data,
  theme_set,
  config_path = NULL,
  config = NULL,
  categories = .VALID_PROVOCATION_CATEGORIES,
  resume = FALSE,
  config_overrides = list()
)
```

## Arguments

- data:

  Tibble: standardized + preprocessed corpus. Must carry `std_id` +
  `std_text`; should also carry `std_author` for T0.2 participant
  spread, and either `theme_membership_*` columns or an `emerged_themes`
  column to indicate which entries support each theme.

- theme_set:

  ThemeSet with researcher-authored themes. The provocateur loop runs
  once per theme.

- config_path:

  Path to a YAML config file that declares
  `methodology.mode = "reflexive_scaffold"`. Mutually exclusive with
  `config`.

- config:

  A pre-loaded `ThematicConfig`. Mutually exclusive with `config_path`.

- categories:

  Character vector of provocation categories to run (defaults to all
  five). Restricting this here also restricts the T0.3 coverage
  assertion to the supplied subset.

- resume:

  Logical; if TRUE, look for a prior Mode 1 run dir and resume the
  provocateur loop from its reflection_log.json. Memos are also
  rehydrated from `outputs/<run>/memos/<id>.md` (the canonical
  persistence layer per AC4).

- config_overrides:

  Named list of dot-path config overrides.

## Value

Invisibly: a list with `output_dir`, `reflection_log`, `theme_set`,
`coverage`, `theme_stats`, `config`, `integrity`, `artifact_paths`.

## Details

Mode 1's architectural commitment (Sarkar 2024 / patterns doc): the AI
does NOT author themes or codes – the researcher does, in their own
external workflow (NVivo, ATLAS.ti, MAXQDA, etc.). pakhom's contribution
is the extractive provocateur loop: counter-narrative, absent voice,
alternative interpretation, disconfirming evidence, and assumption
surfacing. This function takes the researcher's finished theme set as
input and surfaces the AI's challenges to it as verifiable,
citation-anchored provocations.

## See also

[`run_analysis`](https://abanoub-armanious.github.io/pakhom/reference/run_analysis.md)
(Mode 2/3 entry point);
[`run_provocateur_questioning`](https://abanoub-armanious.github.io/pakhom/reference/run_provocateur_questioning.md)
(the bare provocateur loop without scaffolding);
[`add_memo`](https://abanoub-armanious.github.io/pakhom/reference/add_memo.md)
(Mode 1 reflexive memo CRUD);
[`compute_mode1_coverage`](https://abanoub-armanious.github.io/pakhom/reference/compute_mode1_coverage.md)
(T0.3 coverage compute);
[`vignette("methodology-modes")`](https://abanoub-armanious.github.io/pakhom/articles/methodology-modes.md)
(per-mode worked examples).

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Author your themes elsewhere (e.g., NVivo) and load them.
#    pakhom never writes themes in Mode 1.
my_themes <- create_theme_set(list(
  list(id = 1, name = "Adherence",
       description = "Researcher-authored: medication adherence",
       codes_included = c("med_routine", "daily_pills"))
))

# 2. Run the provocateur loop with full Tier-0/Tier-1 scaffolding
result <- run_mode1(
  data        = my_corpus,        # tibble with std_id + std_text
  theme_set   = my_themes,
  config_path = "config.yaml"     # methodology.mode = "reflexive_scaffold"
)

# 3. Add reflexive memos (Mode 1's AC6 burden parity vs Modes 2/3)
result$reflection_log <- add_memo(
  result$reflection_log,
  body = "The 'Adherence' theme rests heavily on contributors 1-3.",
  type = "theoretical",
  linked_themes = "Adherence"
)
persist_memos(result$reflection_log, result$output_dir)
} # }
```
