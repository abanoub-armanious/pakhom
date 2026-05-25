# Methodology Modes: Choosing Mode 1, 2, or 3

## Why three modes?

`pakhom`’s architectural commitment **AC1** says: *AI is scaffold by
architecture, not by configuration.* Most “AI for thematic analysis”
tools collapse the methodological question (“what is the AI’s role
here?”) into a configuration option (“which prompt template do I
want?”). pakhom inverts that: the AI’s role is determined by which of
three **methodology modes** you declare, and the package code enforces
the commitments that flow from that mode — what the AI may produce, what
the researcher must author, which transparency artifacts are mandatory,
which pause-points are required.

The mode declaration is mandatory in every config (no default); it is
locked at run start and stamped onto every output. Any change creates a
**fork run** with `parent_run_id` linkage (REDCap dev/production
pattern), so the methodology trail is always reconstructable.

This vignette walks through each mode with a worked example, then gives
a decision rubric for choosing among them.

## Mode 1: Reflexive Scaffold (AI as provocateur)

> *“AI Should Challenge, Not Obey”* (Sarkar 2024, CACM Oct 2024).

In Mode 1, the AI never names themes, codes, or interpretations. The
researcher authors the analytic frame (typically in NVivo, ATLAS.ti, or
MAXQDA). pakhom contributes the **provocateur loop**: five extractive
questioning categories that surface counter-evidence the researcher’s
framing might overlook.

### The five provocation categories

| Category | What the AI returns |
|----|----|
| `counter_narrative` | Up to N entries from the corpus that frame the construct as not-Y (challenges the theme’s framing) |
| `disconfirming_evidence` | Entries that directly contradict the theme |
| `alternative_interpretation` | Methodologically-defensible alternative theme names that the same supporting quotes could support (without saying which is better) |
| `absent_voice` | Demographic / temporal / linguistic / topical segments of the corpus that are underrepresented in the theme’s supporting entries |
| `assumption_surfacing` | Other terms participants use for the same construct + a term the researcher’s framing erases |

Each provocation that cites a verbatim quote runs through the same
four-step verification ladder used in Modes 2/3 (T0.1 universal). A
fabricated quote is dropped silently and logged to `fabrication_log.csv`
— fabricated provocations never reach the researcher.

### Worked example

``` r

library(pakhom)

# 1. Author your themes elsewhere (e.g., NVivo) and load them as a ThemeSet.
#    pakhom never writes themes in Mode 1.
my_themes <- create_theme_set(list(
  list(id = 1, name = "Adherence",
       description = "Researcher-authored: medication adherence behaviors",
       codes_included = c("med_routine", "daily_pills")),
  list(id = 2, name = "Resistance",
       description = "Researcher-authored: resistance to the regimen",
       codes_included = c("skip_doses", "side_effects"))
))

# 2. Your corpus -- a tibble with std_id + std_text. theme_membership_*
#    columns indicate which entries support each theme. (Author IDs in
#    std_author drive the T0.2 participant-spread metric.) In practice
#    you'd build this tibble from a database load + your existing NVivo
#    coding; here a small toy corpus illustrates the shape.
my_corpus <- tibble::tibble(
  std_id   = c("e1", "e2", "e3", "e4"),
  std_text = c(
    "I plan to take my medication every day from now on.",
    "My doctor told me to follow this regimen carefully.",
    "I always forget my pills; the schedule is impossible.",
    "Side effects make me skip doses on weekends."
  ),
  std_author = c("alice", "bob", "carol", "dave"),
  theme_membership_Adherence  = c(1L, 1L, 0L, 0L),
  theme_membership_Resistance = c(0L, 0L, 1L, 1L)
)

# 3. Drive the provocateur loop with full Tier-0/Tier-1 scaffolding.
result <- run_mode1(
  data        = my_corpus,
  theme_set   = my_themes,
  config_path = "config.yaml",   # methodology.mode = "reflexive_scaffold"
  categories  = c("counter_narrative", "disconfirming_evidence",
                   "absent_voice")  # subset; defaults to all five
)

# result$reflection_log carries:
#   - provocations[]: list of Provocation S3 objects (verified citations)
#   - provocation_attempts: data.frame of every theme x category attempt
#                           (so coverage can distinguish "AI returned 0
#                           legitimately" from "category never attempted")
#   - skipped_themes:  themes the orchestrator bypassed (e.g., zero
#                      supporting entries) with explicit reasons
#   - memos:           reflexive notes (initially empty -- you write them)

# 4. Per AC6 (symmetric obligations across modes), Mode 1's burden parity
#    against Modes 2/3 is delivered through reflexive memos. Add memos
#    in response to provocations that move you, then persist them.
result$reflection_log <- add_memo(
  result$reflection_log,
  body = "The AI's counter_narrative for 'Adherence' surfaces e3 + e5,
which I had read as adherent because of the present-tense framing. Re-
reading them: they describe ASPIRATIONAL adherence, not observed
behavior. Theme name should probably be 'Adherence Talk' or split.",
  type = "theoretical",
  linked_themes = "Adherence"
)
persist_memos(result$reflection_log, result$output_dir)
```

### What you get

A finalized run directory at `outputs/<run-id>_M1/` containing:

- `run_metadata.json` — methodology stamp + mode + run id + framework
  hash (NA for Mode 1) + timestamp
- `rules/methodology_rules.md` — the AC9 system prompt that governed
  every AI call
- `reflection_log.json` — full reflection log (provocations + attempts +
  skips + memos)
- `provocations.csv` — flat exportable provocation list (theme +
  category + cited quote + verification status)
- `provocation_attempts.csv` — the attempt-tracking matrix that drives
  T0.3
- `coverage_mode1.json` — `ProvocationCoverage` (no_silent_skip
  headline + per-category attempt counts)
- `themes.json` — the researcher-authored themes archived for replay
- `memos/<id>.md` — one Markdown file per memo, with YAML frontmatter
- `fabrication_log.csv` — any fabricated provocation citations dropped
  during the run
- `ai_decisions.jsonl` — full audit trail of every AI call
- `analysis_report.html` — Mode 1 HTML report with Tier-0 dashboards +
  per-theme provocations + memo timeline

### When to use Mode 1

- You are conducting reflexive thematic analysis (Braun & Clarke 2022)
  and want AI to challenge your interpretation, not produce it.
- You have a constructionist or critical-realist epistemology where
  AI-generated themes would be epistemically incoherent with the
  methodology.
- Your codebook is small (under ~150 codes) so you can author it
  manually and want depth over scale.
- You’re worried about Vikan et al. 2026’s *engagement collapse* finding
  and want the package to force you back into the data via provocations.

## Mode 2: Codebook Collaborative (the auto-pipeline)

In Mode 2 the AI proposes codes, then themes, and the researcher **gates
each at pause-points**. This is the workflow most users coming from a
manual coding tradition will recognize: the AI does the mechanical work,
the researcher curates.

### Pause points

By default
[`run_analysis()`](https://abanoub-armanious.github.io/pakhom/reference/run_analysis.md)
will pause after progressive coding (so you can review the codebook in
CSV form) and again after theme generation (so you can curate themes
before correlations are computed). At each pause, the pipeline exports a
CSV; you edit in-place and rename it (`codebook_review.csv` →
`codebook_reviewed.csv`); then re-run with `resume = TRUE`.

You can also configure no pauses and let the pipeline run end-to-end —
useful for batch / replay runs after the codebook is stable.

### Worked example

``` r

library(pakhom)

# 1. Create a config (the wizard or programmatically). Methodology
#    declaration is mandatory.
create_config(
  methodology = "codebook_collaborative",
  study_name = "Sleep + medication forum study",
  research_focus = "How do users describe sleep effects of psychiatric meds?",
  database_path = "reddit_data.db",
  output_path = "config.yaml"
)

# 2. Run the full pipeline. Returns invisibly with the analytic_data
#    + theme_set + correlations + insights + paths to the output dir.
result <- run_analysis("config.yaml")

# 3. If pause-points are enabled, run_analysis returns a status message
#    and stops at the pause. Edit the exported CSV and re-run with
#    resume = TRUE:
result <- run_analysis("config.yaml", resume = TRUE)

# 4. The Mode 2 report at result$output_dir/analysis_report.html
#    carries the Tier-0 dashboards + saturation curve + per-theme
#    sentiment breakdown + correlation matrix + AI-synthesis executive
#    summary.
```

### What you get

A finalized run directory at `outputs/<run-id>_M2/` with:

- All universal Tier-0/Tier-1 artifacts (run_metadata, methodology
  rules, fabrication_log, ai_decisions)
- `sentiment_scores.csv` — per-entry sentiment + emotions + intensity
- `codes.csv` — the codebook (codes preserved as atomic leaves per C2; renamed from `consolidated_codes.csv` in Phase 60.6)
- `themes.json` — theme set with merge_history + supporting quotes
- `theme_entries/` — one CSV per theme with member entries
- `correlations.csv` — Spearman correlations + Bonferroni-adjusted
  p-values
- `analysis_report.html` — Mode 2 HTML report
- `correlation_plot.png`, `theme_network.png` — supporting
  visualizations

### When to use Mode 2

- You are doing codebook TA, template TA, or any approach where
  AI-generated codes are an acceptable input to your interpretive work.
- You have a medium-to-large corpus (200+ entries) and the AI’s
  mechanical coding speed is genuinely useful.
- You are willing to gate the AI’s output at pause-points rather than
  accept it wholesale.
- You want IRR + saturation diagnostics and the audit trail as your
  defensibility argument.

## Mode 3: Framework Applied (apply a theoretical framework verbatim)

In Mode 3 you supply a theoretical framework (e.g., the Theory of
Planned Behavior, COM-B, or the Theoretical Domains Framework). The AI
applies it verbatim — coding entries with the framework’s constructs as
labels — and flags entries that **resist** the framework as anomalies
per the framework’s anomaly_handling policy.

### Built-in frameworks

``` r

library(pakhom)
list_builtin_frameworks()
#> [1] "tpb"  "comb" "tdf"

# Each is loadable by alias OR by file path
spec <- load_framework_spec("tpb")
print(spec)
#> FrameworkSpec: Theory of Planned Behavior
#>   Epistemic stance: positivist
#>   Anomaly policy:   bracket
#>   Constructs:       5
#>     - attitude (Attitude toward the behavior): ...
#>     - subjective_norm (Subjective norm): ...
#>     - perceived_behavioral_control (Perceived behavioral control): ...
#>     - intention (Behavioral intention): ...
#>     - behavior (Behavior): ...
#>   Citations:        2
```

### The three anomaly_handling policies

What happens to entries that don’t fit any framework construct depends
on the framework’s `anomaly_handling` field:

| Policy | Behavior |
|----|----|
| `bracket` | Entry coded as `anomaly` with a one-sentence reason; framework NOT modified. Most positivist. |
| `extend` | Anomaly becomes a new construct (Vila-Henninger 2024 “abductive coding”); requires explicit researcher acceptance. |
| `revise` | Anomaly triggers modification of an existing construct’s definition; logged as framework revision. |

### Worked example

``` r

library(pakhom)

# 1. Create a Mode 3 config -- framework is applied verbatim.
create_config(
  methodology = "framework_applied",
  framework_spec_path = "tpb",   # built-in alias OR path to your YAML/JSON
  study_name = "TPB analysis: medication adherence",
  research_focus = "Behavioral intention -> adherence behavior",
  database_path = "reddit_data.db",
  output_path = "config.yaml"
)

# 2. Run the pipeline. Internally:
#    - load_framework_spec() -> validates the spec
#    - archive_framework_spec() -> writes outputs/<run>/framework_applied.yaml
#                                  + sha256 hash for replay-equivalence
#    - run_metadata.json carries framework_name + framework_hash +
#      framework_epistemic_stance + framework_anomaly_handling +
#      framework_n_constructs
#    - The Mode 3 HTML report renders a Framework Declaration section
#      with the framework's name + sha256 + citations + epistemic stance +
#      anomaly handling policy + full constructs table
result <- run_analysis("config.yaml")

# 3. The report at result$output_dir/analysis_report.html includes a
#    new "Theoretical Framework (Mode 3 / AC4)" section. The
#    framework_applied.yaml file alongside is byte-equivalent to the
#    spec that was loaded -- a downstream replay using the same hash
#    is provably the same framework.
```

### A note on Mode 3 + Anthropic

When `mode = "framework_applied"` AND `provider = "anthropic"`, the
Anthropic Citations API path is structurally precluded: forced
`tool_use` schema (which Mode 3 requires to constrain coding to
framework constructs) and the Citations API output format are mutually
exclusive on the same response. The Mode 3 + Anthropic pipeline
therefore relies on the verification ladder’s DETECTION-only path
(model_freeform + offline string match) rather than the API’s PREVENTION
layer.

The Mode 3 report renders an **explicit footnote** disclosing this
constraint. Without the footnote, a reviewer reading “Model freeform
(detection only)” in the Tier-0 dashboard would reasonably wonder why
the Anthropic prevention layer isn’t engaged — the footnote makes the
architectural reason explicit rather than letting it look like a bug.

A future phase may explore a hybrid schema (constrained constructs
paired with citation offsets) as a research spike.

### When to use Mode 3

- You have a pre-existing theoretical framework you want to apply
  rigorously.
- You are conducting deductive coding, content analysis, or framework
  analysis (Ritchie & Spencer).
- The framework’s epistemic stance and anomaly handling policy are
  intentional methodological choices, not defaults.

## Choosing among the three modes

| You are doing… | Mode |
|----|----|
| Reflexive thematic analysis (Braun & Clarke 2022); want AI to challenge, not produce | **1** |
| Codebook TA / template TA; want AI mechanical coding with researcher curation | **2** |
| Theoretical framework analysis (TPB, COM-B, TDF, your own) with deductive coding | **3** |
| Constructionist / critical realist epistemology; AI-as-author would be incoherent | **1** |
| Positivist or pragmatic epistemology; AI-as-coder is acceptable input | **2** or **3** |
| Small corpus (under ~150 codes); want depth | **1** |
| Medium-to-large corpus (200+ entries); want scale + audit trail | **2** |
| Pre-registered framework analysis | **3** |
| Your research focus differs from the corpus’s *dominant signal* | **1** (see “Mode 2 drift” below) |

## Mode 2 drift on skewed-signal corpora

Phase 57 (re-validation on a 9,178-entry binge-eating + sleep +
medication corpus) found a recurring failure mode you should know about
before choosing Mode 2:

**When the corpus’s dominant signal is NOT the configured research
focus, Mode 2 themes drift toward the corpus’s natural topic structure
and away from the question you asked.**

Concretely: the Phase 57 run configured
`research_focus = "medication × sleep × binge eating interactions"`
against r/BingeEatingDisorder posts and comments. The corpus’s actual
dominant signal was generic binge-eating affect (purging, shame,
emotional eating). Mode 2’s HAC + AI tree walk faithfully recovered the
dominant signal – 417 themes, organized cleanly. But of those 417, only
**2** named all three of {medication, binge, sleep} together, and only
**1** had substantive mass (~56 entries). The load-bearing medication ×
sleep interaction finding was buried in the long tail.

This is not a bug in Mode 2. The HAC algorithm is bottom-up: it clusters
by code-code similarity, not by research-question relevance. If 80% of
the corpus is about emotional eating, 80% of the themes will be about
emotional eating, regardless of what your `research_focus` says.

### What to do about it

1.  **Audit your corpus’s signal balance first**. If `research_focus` is
    a niche intersection (e.g., “medication × sleep × binge eating”),
    sample 100-200 random entries and ask: how many actually touch ALL
    three concepts? If the answer is “fewer than 10%”, Mode 2 will
    produce themes that *contain* your focus but won’t be *organized
    around* it.

2.  **Switch to Mode 1 (Reflexive Scaffold) for the niche-focus case**.
    In Mode 1 you author the themes; the AI doesn’t get a vote on theme
    generation. The themes are guaranteed to be organized around your
    `research_focus`, and the AI’s role is restricted to provocateur
    loops + memo prompts that challenge what you wrote. See
    [`vignette("getting-started")`](https://abanoub-armanious.github.io/pakhom/articles/getting-started.md)
    for the Mode 1 walkthrough.

3.  **Refine `research_focus` to match the dominant signal**. If your
    data is mostly about generic binge eating, you can rewrite the
    question as “How does generic binge eating intersect with medication
    and sleep when it does?” The themes will then be about binge eating
    with medication/sleep as discriminating features – a publishable
    framing.

4.  **Hybrid pipeline (intermediate effort)**. Run Mode 2 first, then
    manually curate the long-tail themes that touch your niche focus
    into a Mode 1 frame for a focused write-up. The Mode 2 audit log
    gives you cross-theme traceability for that curation.

Mode 2 is excellent when the research focus matches the corpus’s
dominant signal (e.g., r/BingeEatingDisorder + “binge eating” focus). It
is not the right tool for niche-intersection focuses on broad corpora.

## What every mode produces (universal Tier-0)

Regardless of mode, every finalized pakhom run produces these
**universal Tier-0 transparency artifacts** (per AC7):

- **T0.1 quote provenance** — every AI-attributed verbatim claim
  verified via the four-step ladder; fabrications dropped + logged to
  `fabrication_log.csv`.
- **T0.2 participant spread** — every theme reports
  n_distinct_contributors + Gini + top contributor share. Themes that
  look prevalent but rest on one heavy poster get a warning on the
  report.
- **T0.3 corpus coverage** — Modes 2/3 assert “no silent truncation in
  the LLM call path”; Mode 1 asserts “no silent skip across themes ×
  provocation categories.” Both are surfaced via a coverage card on the
  HTML report.

These commitments are load-bearing: a finalized pakhom run that lacks
any of them is a transparency failure surfaced by
[`verify_run_integrity()`](https://abanoub-armanious.github.io/pakhom/reference/verify_run_integrity.md).

## Replay-equivalence and run_metadata.json

Every finalized run carries `run_metadata.json` with:

- `run_id` + `methodology_mode` + `mode_locked_at` + `is_finalized`
- `provider` + `model_primary` + `model_fast` (which AI ran the
  analysis)
- `config_hash` (so a config drift between runs is detectable)
- `framework_name` + `framework_hash` + `framework_*` (Mode 3 only)
- `mode1_categories_requested` + `mode1_n_themes_input` (Mode 1 only)
- `parent_run_id` + `mode_changed_from` (when the run was forked from
  another)
- `analysis_schema_version` (output column schema;
  [`compare_runs()`](https://abanoub-armanious.github.io/pakhom/reference/compare_runs.md)
  uses this to refuse cross-schema comparisons)

This metadata is the contract that makes pakhom runs
**replay-equivalent**: two runs against the same data + config +
framework hash + provider should produce comparable artifacts.
Cross-mode comparisons via
[`compare_runs()`](https://abanoub-armanious.github.io/pakhom/reference/compare_runs.md)
or
[`compare_models()`](https://abanoub-armanious.github.io/pakhom/reference/compare_models.md)
route off these fields.

## Further reading

- [`vignette("getting-started")`](https://abanoub-armanious.github.io/pakhom/articles/getting-started.md)
  — full step-by-step Mode 2 walkthrough
- [`?run_mode1`](https://abanoub-armanious.github.io/pakhom/reference/run_mode1.md)
  — Mode 1 orchestrator API reference
- [`?run_analysis`](https://abanoub-armanious.github.io/pakhom/reference/run_analysis.md)
  — Mode 2/3 orchestrator API reference
- [`?load_framework_spec`](https://abanoub-armanious.github.io/pakhom/reference/load_framework_spec.md)
  — framework spec loading + built-in frameworks
- [`?add_memo`](https://abanoub-armanious.github.io/pakhom/reference/add_memo.md)
  — Mode 1 reflexive memo CRUD
- [`?compute_mode1_coverage`](https://abanoub-armanious.github.io/pakhom/reference/compute_mode1_coverage.md)
  — Mode 1 T0.3 coverage compute
- Sarkar 2024 (CACM, Oct 2024) — “AI Should Challenge, Not Obey” — Mode
  1 motivation
- Jowsey et al. 2025 (PLOS One, <doi:10.1371/journal.pone.0330217>) —
  “Frankenstein” finding — Tier-0 motivation
- Braun & Clarke 2022 — reflexive TA foundation
- Vila-Henninger 2024 — abductive coding (Mode 3 `extend` policy)
- Lin & Corley 2025 (arXiv:2505.03105) — methodology rules pattern (AC9)
