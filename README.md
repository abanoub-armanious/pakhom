# pakhom

> AI-Assisted Reflexive Thematic Analysis with Methodology-as-Architecture

**pakhom** is an R package that conducts AI-assisted reflexive thematic
analysis with three methodologically-distinct operating modes. Methodology is
codified at the **architectural** level — the AI's role is shaped by the mode
you declare, not by user discipline at the configuration level. Every output
carries the methodology stamp, every AI-attributed verbatim claim is verified
against the source corpus, and every run can be replay-equivalent.

You give it your data, your research question, and the methodological posture
you've chosen. It gives you back a complete thematic analysis — themes, codes,
sentiment, correlations, supporting evidence, full audit trail — that a peer
reviewer can read on the same epistemic terms as a hand-coded analysis.

## Why pakhom?

Most "AI for thematic analysis" tools are wrappers around an LLM call. The
researcher specifies the data; the model returns themes; the researcher
publishes. The methodology is implicit in the prompt, the inputs are
opaque to reviewers, and the chosen epistemic stance lives in informal
back-and-forth rather than the artifact itself.

pakhom takes a different position. **Methodology is architecture.** A pakhom
run declares which of three modes it operates under, and every commitment
flowing from that mode (what the AI may produce, what the researcher must
author, which transparency artifacts are mandatory) is enforced by the
package code, not by configuration or convention. The result is an analysis
that ships with its methodology *as data*, not as prose.

The empirical motivation comes from three lines of evidence:

- **Sarkar 2024** (CACM, *AI Should Challenge, Not Obey*) — when AI agrees,
  three failure modes follow: dilution, distortion, deskilling. Mode 1
  inverts this: AI as Socratic gadfly only.
- **Jowsey et al. 2025** (PLOS One, doi:10.1371/journal.pone.0330217) — the
  "Frankenstein" finding that Microsoft Copilot drew themes from only the
  first 2-3 pages of data. pakhom's Tier-0 transparency layer (T0.1 quote
  provenance + T0.2 participant spread + T0.3 corpus coverage) is
  the architectural answer.
- **Vikan et al. 2026** (Sage, doi:10.1177/10497323251365211) — under
  prolonged AI use, researcher engagement collapses to verification mode.
  Mode 1 forces the researcher back into the data through provocations;
  Modes 2/3 carry equivalent burden through pause-points and reflexive
  memos.

The package's name, **pakhom**, is the Coptic Egyptian form of *Pachomius*
— the desert abbot whose written **Rule** (c. 320 CE) established the genre
of methodology-as-written-document. Pakhom (the saint) wrote the Rule that
made monasticism reproducible; pakhom (the package) writes the rules that
make AI-assisted thematic analysis methodologically reproducible.

## Three methodology modes

The mode declaration is mandatory in every config (no default). It is locked
at run start, stamped on every output, and any change creates a fork run with
parent_run_id linkage (REDCap dev/production pattern).

| Mode | AI's role | Researcher authors | When to use |
|---|---|---|---|
| **`reflexive_scaffold`** (Mode 1) | Socratic gadfly: surfaces counter-narratives, absent voices, alternative interpretations, disconfirming evidence, assumption-surfacing terms | Codes + themes (typically in NVivo / ATLAS.ti) + reflexive memos | Reflexive TA, constructionist epistemology, depth over scale |
| **`codebook_collaborative`** (Mode 2) | Proposes codes + themes; researcher gates each at pause-points | Codebook curation + theme review + reflexivity statement | Codebook TA, template TA, the auto-pipeline you'd recognize from manual coding |
| **`framework_applied`** (Mode 3) | Applies a researcher-supplied framework verbatim; flags entries that resist the framework as anomalies | Framework spec (or pick a built-in: TPB, COM-B, TDF) + anomaly-handling decisions | Theory-driven analyses, deductive coding, content analysis |

Mode 1 uses [`run_mode1()`](https://abanoub-armanious.github.io/pakhom/reference/run_mode1.html);
Modes 2/3 use [`run_analysis()`](https://abanoub-armanious.github.io/pakhom/reference/run_analysis.html).
Both produce a finalized run directory with full audit trail, run_metadata.json,
methodology rules archive, and HTML report.

See [`vignette("methodology-modes")`](https://abanoub-armanious.github.io/pakhom/articles/methodology-modes.html)
for a worked example of each mode.

## Tier-0 transparency

Every mode produces three transparency artifacts addressing the most-cited
empirical critiques of LLM-for-TA tools:

- **T0.1 quote provenance + 4-step verification ladder** — every AI-attributed
  verbatim claim runs through strict offline match → normalized match →
  substring search → embedding similarity. Fabricated quotes are dropped and
  logged to `fabrication_log.csv`. Mode 1 verifies provocation citations;
  Mode 2/3 verify coded-segment quotes. On Anthropic, the Citations API serves
  as a prevention layer; the verification ladder is detection-in-depth.
- **T0.2 participant spread per theme** — every theme reports
  n_distinct_contributors + Gini coefficient + top contributor share. Themes
  that look prevalent but rest on one heavy poster get an explicit warning
  on the report.
- **T0.3 corpus coverage assertion** — Modes 2/3 assert "no silent truncation
  in the LLM call path"; Mode 1 asserts "no silent skip across themes ×
  provocation categories." The Mode 3 + Anthropic Citations API silent bypass
  (forced tool_use schema + Citations API are mutually exclusive at the
  response level) is disclosed via an explicit footnote rather than left
  invisible.

## About the name

**pakhom** (Coptic ⲡⲁϩⲱⲙ, "eagle") honors Saint Pachomius the Great
(c. 292–348 CE), the Coptic Egyptian abbot whose written **Rule** of communal
discipline established the genre of *methodology-as-written-document*. Before
Pachomius, monastic life was anchorite — solitary and unstructured. Pachomius
codified the first reproducible, inspectable, transmissible framework for
shared practice, transforming an unruly tradition into something that could
be taught, replicated across communities, and held to account.

This package is a digital descendent of that move. AI behavior in qualitative
analysis is constrained at the **architectural** level by methodologically-
coherent rules — not at the configuration level by user discipline. The
methodology is the permission structure. Pakhom (the saint) wrote the Rule
that made monasticism reproducible; pakhom (the package) writes the rules
that make AI-assisted thematic analysis methodologically reproducible.

The Coptic form *pakhom* (rather than the Hellenized *Pachomius*) is used
deliberately: a small act of restoration, naming a tradition in its own
voice. The author is Coptic Egyptian.

## Key Features

- **Progressive sequential coding** -- entries are read one at a time, just as
  a human researcher would in NVivo. The AI codes applicable text segments
  inline, building and reusing a codebook organically as it goes
- **Thematic saturation detection** -- triangulated monitoring (code creation
  rate, reuse stability, AI self-assessment) stops coding when no new patterns
  are emerging, saving time and cost
- **Iterative bottom-up theme generation** -- codes are merged into clusters
  through sequential passes until no more productive groupings exist. Themes
  and subthemes emerge organically from the data, not from top-down labels
- **Deterministic code-path cascading** -- entries map to themes through their
  codes (no AI re-reading of raw text), faithful to the inductive process
- **Code-aware sentiment analysis** -- sentiment is scored after coding, using
  assigned codes as context for more accurate emotional valence detection
- **Codebook-first learning from prior studies** -- learns coding conventions,
  structural relationships, and discarded patterns from QDPX codebooks (NVivo),
  Excel, or CSV files. Manuscripts serve as supplementary clarification
- **Researcher review points** -- pause the pipeline after coding or theme
  generation to curate the AI's output before continuing
- **Checkpoint/resume** -- long-running analyses can be interrupted and
  resumed without losing progress. Flat checkpoint architecture ensures
  reliable resume across retries
- **Inter-rater reliability** -- built-in IRR computation (Cohen's kappa,
  Krippendorff's alpha) for human verification of AI-generated codes
- **Multiple AI providers** -- works with OpenAI (GPT-4o) or Anthropic (Claude),
  with optional parallel multi-model mode for cross-provider analysis
- **Inter-model reliability** -- run the pipeline with different AI models and
  compare results via `compare_models()` to compute inter-model agreement
  (Cohen's kappa, theme similarity, sentiment correlation)
- **Embedding-augmented theme emergence** -- uses text embeddings and code
  co-occurrence statistics to provide quantitative context during iterative
  code merging, complementing the AI's semantic judgment
- **Researcher reflexivity** -- injects researcher positionality, research
  paradigm, and reflexive notes into all AI prompts, aligning with Braun &
  Clarke's emphasis on reflexive practice
- **AI decision audit trail** -- every AI decision (code assignments, merges,
  skips, saturation signals) is logged to a JSONL file with full rationale
  for post-hoc transparency review
- **QDPX export** -- exports codebook and coded segments in QDPX format for
  import into NVivo, ATLAS.ti, or MAXQDA for manual verification
- **Longitudinal analysis** -- when entries have timestamps, tracks theme
  prevalence trends and emergence timelines within a single run
- **Publication-quality reports** -- generates self-contained HTML reports with
  theme narratives, representative quotes, sentiment breakdowns, saturation
  curves, correlation tables, confidence intervals, and decision transparency
- **Run comparison** -- compare results across pipeline runs to assess
  stability and track how themes evolve

## Quick Start

### Install + set API key

```r
# Install from GitHub
devtools::install_github("abanoub-armanious/pakhom")

# Set your API key in .Renviron (persistent; recommended)
usethis::edit_r_environ()
# Add: OPENAI_API_KEY=sk-your-key-here
# (or ANTHROPIC_API_KEY=sk-ant-... for Claude)
# Restart R after editing.
```

### Recommended: web-based config wizard

The fastest path to a valid config is the Shiny wizard. It walks you
through methodology choice, study metadata, data path, and provider
selection, then writes a validated `config.yaml`:

```r
library(pakhom)
config_wizard_app()
```

If you'd rather build the config programmatically, the per-mode
examples below produce equivalent output.

### Mode 2 (Codebook Collaborative) — the auto-pipeline

```r
# Create config (declares methodology mode + study + data + output)
pakhom::create_config(
  methodology = "codebook_collaborative",
  study_name = "My Study",
  research_focus = "How does X relate to Y?",
  database_path = "my_data.db",
  output_path = "config.yaml"
)

# Run the full pipeline: progressive coding -> sentiment -> themes ->
# correlations -> Mode 2 HTML report -> finalize_run
results <- pakhom::run_analysis("config.yaml")
```

### Mode 3 (Framework Applied) — apply a theoretical framework

```r
# Pick a built-in framework (or supply your own YAML/JSON spec)
pakhom::list_builtin_frameworks()
# [1] "tpb"  "comb" "tdf"

# Create a Mode 3 config -- framework is applied verbatim, anomalies
# (entries that resist the framework) get flagged per the framework's
# anomaly_handling policy
pakhom::create_config(
  methodology = "framework_applied",
  framework_spec_path = "tpb",  # or path to your custom spec
  study_name = "TPB analysis",
  research_focus = "Behavioral intention -> behavior",
  database_path = "my_data.db",
  output_path = "config.yaml"
)
results <- pakhom::run_analysis("config.yaml")
```

### Mode 1 (Reflexive Scaffold) — AI as provocateur

```r
# Mode 1 expects you to author themes (e.g., in NVivo) and feed them to
# pakhom for AI-extracted provocations. The package never writes themes
# in this mode.
my_themes <- pakhom::create_theme_set(list(
  list(id = 1, name = "Adherence",
       description = "Researcher-authored theme",
       codes_included = c("medication", "routine"))
))

# Drive the provocateur loop with full Tier-0/Tier-1 scaffolding
result <- pakhom::run_mode1(
  data        = my_corpus,        # tibble with std_id + std_text
  theme_set   = my_themes,
  config_path = "config.yaml"     # methodology.mode = "reflexive_scaffold"
)

# Add reflexive memos (Mode 1's AC6 burden parity vs Modes 2/3)
result$reflection_log <- pakhom::add_memo(
  result$reflection_log,
  body = "The 'Adherence' theme rests heavily on contributors 1-3; the AI's counter_narrative provocations suggest theme reframing is warranted.",
  type = "theoretical",
  linked_themes = "Adherence"
)
pakhom::persist_memos(result$reflection_log, result$output_dir)
```

## Pipeline Overview

| Step | What it does |
|------|-------------|
| 1. Learn from prior studies | Parses QDPX codebooks and manuscripts for coding conventions and structural patterns |
| 2. Load & preprocess data | Reads from SQLite database, cleans text, standardizes columns |
| 3. Progressive coding | AI reads each entry sequentially, coding applicable text with existing or novel codes |
| 4. Saturation detection | Monitors code creation rate and reuse stability; stops when codebook plateaus |
| 5. Sentiment analysis | AI scores sentiment on coded entries, using codes as context |
| 6. Theme generation | Sequential bottom-up merging of codes into clusters across multiple passes |
| 7. Theme cascading | Deterministic entry-to-theme mapping through the code hierarchy |
| 8. Correlations | Statistical analysis of theme-sentiment relationships and co-occurrence |
| 9. QDPX export | Exports codebook and coded segments for QDA software interoperability |
| 10. Temporal analysis | Theme prevalence trends and emergence timelines (when timestamps available) |
| 11. Cross-run comparison | Compares themes, codes, and sentiment across runs; detects inter-model reliability |

Optional steps: researcher review points (after coding and/or theme generation,
in CSV or QDPX format), human verification (IRR), parallel multi-model mode,
and AI decision audit logging.

## Who is this for?

Researchers who:

- Are conducting qualitative or mixed-methods research with large text datasets
- Want to use AI to assist (not replace) their analytical process
- May or may not have deep experience with R programming
- Want reproducible, transparent, and auditable thematic analysis

## Requirements

- R >= 4.1.0 and RStudio (recommended)
- An API key from [OpenAI](https://platform.openai.com/) or
  [Anthropic](https://console.anthropic.com/)
- Your data in a SQLite database (.db file)

**Security note:** Always store API keys in environment variables (`.Renviron`)
rather than in config files. The package warns if it detects a key pasted
directly into `config.yaml`.

## Multi-Model Reliability

To assess inter-model reliability, run the pipeline multiple times with
different AI providers/models:

```r
# Run 1: OpenAI
results1 <- run_analysis("config.yaml")

# Run 2: Change provider to Anthropic in config, then re-run
results2 <- run_analysis("config.yaml")

# Compare models
comparison <- compare_models("outputs/")
```

An optional parallel multi-model mode is also available (`ai.multi_model.enabled: true`
in config). Note: parallel mode disables reviewer pauses to preserve model
independence. For reviewer-guided analysis, use single-model sequential runs.

## Documentation

- **[Getting Started vignette](articles/getting-started.html)** -- step-by-step
  guide from installation to interpreting results
- **[Methodology Modes vignette](articles/methodology-modes.html)** -- choosing
  among the three modes with worked examples + a decision rubric
- **[Function reference](reference/index.html)** -- documentation for all
  exported functions
- **`config_wizard_app()`** -- interactive web-based config builder

## For methodologists / reviewers: architectural commitments

The package codifies ten load-bearing commitments. Each is regression-
tested at the integration level (the test suite as of phase 36 has
2391 expectations pinning them against silent regression). They are
the contract a peer reviewer can check the package's claims against:

- **AC1**: AI is scaffold by architecture, not by configuration.
- **AC2**: Three modes; no fourth.
- **AC3**: No default mode; explicit declaration mandatory.
- **AC4**: Methodology stamped on every output (`run_metadata.json`,
  every CSV/JSON header, HTML stamp, plot watermarks).
- **AC5**: Soft-lock with audit trail; methodology change creates a new
  run with `parent_run_id` linkage (REDCap dev/production pattern).
- **AC6**: Symmetric researcher-burden obligations across modes (Mode 1
  reflexive memos == Modes 2/3 review pause-points).
- **AC7**: Universal Tier-0 transparency requirements in all modes.
- **AC8**: Modes are configurations of one architecture, never separate
  code paths.
- **AC9**: Methodology rules generated from config and injected into
  the model context every turn (Lin and Corley 2025 pattern).
- **AC10**: Stage-gating via filesystem state.

The methodology-modes vignette covers each commitment in narrative
context. The full design document is at
`pakhom/notes/strategic_audit/SPRINT4_DESIGN.md` Part I.

## Author

Developed by **[Abanoub J. Armanious, MS](https://www.linkedin.com/in/abanoubarmanious/)**.

## License

MIT
