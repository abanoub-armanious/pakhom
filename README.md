# pakhom

> AI-Assisted Thematic Analysis (reflexive · codebook · framework modes) with Methodology-as-Architecture

**pakhom** is an R package that conducts AI-assisted thematic analysis across three
methodologically-distinct operating modes (reflexive, codebook, framework). Methodology is
codified at the **architectural** level — the AI's role is shaped by the mode
you declare, not by user discipline at the configuration level. Every output
carries the methodology stamp, every AI-attributed verbatim claim is verified
against the analytic corpus, and every run is fully auditable and comparable
across re-runs (same data, config, framework, and provider).

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
  Modes 2/3 provide the same re-engagement levers (review pause-points and
  reflexive memos), opt-in rather than automatic.

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

- **T0.1 quote provenance + verification ladder** — every AI-attributed
  verbatim claim runs through strict offline match → normalized match →
  substring search → embedding similarity (this fourth step is optional -- it
  runs only when an embedding provider is supplied; the default coding path uses
  the first three). Fabricated quotes are dropped and
  logged to `fabrication_log.csv`. Mode 1 verifies provocation citations;
  Mode 2/3 verify coded-segment quotes. On Anthropic, the Citations API serves
  as a prevention layer; the verification ladder is detection-in-depth.
  Quotes are verified against the cleaned analytic text produced by
  preprocessing (with default Reddit cleaning, `r/<name>` appears as the
  redaction token `[subreddit]` and `u/` mentions are removed), so verified
  quotes reflect the analytic corpus rather than byte-identical raw posts;
  the raw text is preserved in `original_text`.
- **T0.2 participant spread per theme** — every theme reports
  n_distinct_contributors + Gini coefficient + top contributor share. Themes
  that look prevalent but rest on one heavy poster get an explicit warning
  on the report.
- **T0.3 corpus coverage assertion** — Modes 2/3 assert that every entry
  surviving preprocessing reached the LLM (entry-level coverage; entries
  longer than the configurable per-entry character cap are sent truncated,
  with the truncation measured and disclosed on the coverage card); Mode 1
  asserts "no silent skip across themes ×
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
  inline, building and reusing a codebook organically as it goes. Per-entry
  prompts use additive semantic retrieval: top-N most-frequent codes plus
  top-K semantically similar codes per entry, so the
  AI never sees a truncated codebook and rarely re-invents existing codes
- **AI saturation arbiter** -- per the architectural commitment that the AI
  decides when to stop (C1), saturation is judged by a structured AI call
  ("reached / not_yet / uncertain" with a 30+ char articulation requirement)
  every cadence ticks; no hardcoded windows / thresholds / confirmation
  counts. Cadence auto-scales with corpus size; the arbiter
  refuses vacuous articulations
- **Multi-pass clustering with label-after-clustering for themes** -- the AI
  sees ALL codes at once and proposes a partition into top-level clusters
  (pass 1); pass 2+ takes prior-pass clusters as new "leaves" and the AI
  may group them further or declare convergence. NO hardcoded
  pass count, NO hardcoded cluster-size thresholds. Labeling is a DEDICATED
  post-convergence pass: only after the AI declares convergence does it
  see the full tree and assign researcher-facing names + descriptions to
  every theme and subtheme, with cross-theme name distinctness enforced.
  This honors C-tenet 1 (AI-declared convergence) and
  C-tenet 5 (labels are assigned only after clustering, so no
  bucket-label pressure shapes the structural decisions). This multi-pass
  clustering is the only theme-generation engine; a config still pinning
  `config$analysis$themes$algorithm = "v1"` is honored with a
  one-time deprecation notice.
- **Emergent subtheme structure** -- subthemes arise from the
  multi-pass grouping itself (a penultimate-pass cluster becomes a theme's
  subthemes); their depth is the AI's dynamic call, not a fixed recursion
  limit. Paper-style
  per-subtheme summary tables render each metric column with the AI analyst's
  chosen primitives (Median(MAD) + Mean(SD) kept as a per-column
  fallback), small-n spread/shape cells flagged against the analyst's
  per-column reliability floor, and quotes tagged `[metric: value]`
- **Deterministic code-path cascading** -- entries map to themes through their
  codes (no AI re-reading of raw text), faithful to the inductive process
- **Code-aware sentiment analysis** -- sentiment is scored after coding, using
  assigned codes as context for more accurate emotional valence detection
- **Codebook-first learning from prior studies** -- learns coding conventions,
  structural relationships, and discarded patterns from QDPX codebooks (NVivo),
  Excel, or CSV files. Manuscripts serve as supplementary clarification
- **Live tracking artifacts** -- per-entry codebook snapshot + per-decision
  cluster snapshot are streamed to `outputs/<run>/live/` so a researcher
  can `tail -F` mid-run and watch the codebook + theme hierarchy grow
  (C3 commitment)
- **Methodology-grade statistical layer** -- Spearman/Kendall for ordinal
  sentiment, 4-tier effect-size labels including
  "negligible" (|r| < 0.10), rank-biserial effect_r (sign-aware,
  numerically stable on extreme p-values), Cramer's V populated on the
  Fisher dispatch path, `meaningful_effect ∩ significant` headline counts
- **AI as analyst with a calculator** -- a Methodology Assistant
  articulates a relevance criterion that keeps coding on-focus, and for each
  numeric / timestamp column chooses, by free-form request (never a fixed
  menu), which computational primitives are an honest summary -- a right-skewed
  count gets a median + tail measures, not a mean+SD. Choices are archived and
  can be pinned to re-apply the same metric interpretations in a confirmatory
  re-run, backed by a ~45-primitive backend catalog the researcher never has to
  configure
- **Credibility / honesty layer** -- the report is built to survive review:
  metrics are judged substantive vs source/platform metadata and grouped
  accordingly; circular correlations between two AI codings of the
  same text are excluded from findings but kept in the exported matrix with an
  `exclusion_reason` (flag-don't-drop), and never headline the
  correlation plot; saturation reporting distinguishes entries coded / examined
  / sampled; and no n-floor ever suppresses a value -- small-n statistics are
  marked, never hidden
- **Researcher review points** -- pause the pipeline after coding or theme
  generation to curate the AI's output before continuing. The Subtheme
  hierarchy is preserved across rename + description-only edits; only
  code-mutating edits trigger a re-flatten
- **Checkpoint/resume** -- long-running analyses can be interrupted and
  resumed without losing progress. Flat checkpoint architecture ensures
  reliable resume across retries. Resume paths emit explicit
  WARN banners describing methodology-era drift
- **Inter-rater reliability** -- built-in IRR computation (Cohen's kappa,
  Krippendorff's alpha) for human verification of AI-generated codes
- **Multiple AI providers** -- works with OpenAI (GPT-4o) or Anthropic (Claude)
- **Inter-model reliability** -- run the pipeline with different AI models and
  compare results via `compare_models()` (theme Jaccard similarity, code
  stability/churn, sentiment drift, and correlation persistence across the runs)
- **Researcher reflexivity** -- injects researcher positionality, research
  paradigm, and reflexive notes into all AI prompts, aligning with Braun &
  Clarke's emphasis on reflexive practice and Olmos-Vega AMEE Guide 149
- **Full audit trail** -- every AI decision (code assignments, code groupings, skips,
  saturation judgments, fabrication catches) is logged to JSONL with
  methodology stamp + schema_version + rationale. Pre-rejection fabrication
  attempts are logged to `fabrication_log.csv` with a structured
  `failure_reason` field (which ladder step failed) for methodology-paper
  attribution
- **QDPX export** -- exports codebook and coded segments in QDPX format for
  import into NVivo, ATLAS.ti, or MAXQDA. Project Description honestly
  reports pre-rejection fabrication-caught counts
- **Longitudinal analysis** -- when entries have timestamps, tracks theme
  prevalence trends and emergence timelines within a single run; figures
  filter to top-N by entry count to stay legible at scale
- **Publication-quality reports** -- generates self-contained HTML reports with
  paper-style per-subtheme summary tables, representative quotes (sentiment-
  positioned + author-spread-aware), saturation arbiter rationale,
  effect-size lollipop charts for large correlation matrices, theme network
  filtered to top-N by weighted degree with an explanatory legend, top-N
  inline cards plus compact-row tail for large theme inventories
- **Methodological transparency report bundler** --
  `bundle_transparency_report(run_dir)` produces a single self-contained HTML + JSON companion
  bundling audit log + Lincoln & Guba (1985) credibility / dependability /
  confirmability / transferability mapping + reflexivity scaffold + T0.1
  dashboard + T0.3 coverage card + theme set summary. The "AI does the
  bookkeeping so the human does the reflexivity, here is the receipt for
  everything" artifact
- **Run comparison** -- compare results across pipeline runs to assess
  stability and track how themes evolve. Coverage card persistence
  (`coverage_card.json`) lets reproducibility audits
  reconstruct the funnel without re-running the pipeline

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
# in this mode. For the expected shape of `my_corpus` (a tibble with
# std_id + std_text, optional std_author), see
# vignette("methodology-modes").
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

# Add reflexive memos (the AC6 engagement affordance shared across modes)
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
| 4. Saturation detection | AI arbiter judges saturation at adaptive cadence (`reached` / `not_yet` / `uncertain` with a 30-char articulation floor). Replaces an earlier heuristic that monitored code-creation rate and reuse stability |
| 5. Sentiment analysis | AI scores sentiment on coded entries, using codes as context |
| 6. Theme generation | Multi-pass AI-judged clustering with label-after-clustering. At every pass, the AI sees all current leaves (codes initially, then prior-pass clusters) and either proposes a partition into new clusters OR declares convergence. After convergence, a single dedicated labeling pass assigns researcher-facing names to every theme + subtheme with the whole tree visible. NO hardcoded thresholds (C1); codes preserved as atomic leaves (C2); no name leakage during clustering (C5) |
| 7. Theme cascading | Deterministic entry-to-theme mapping through the code hierarchy |
| 8. Correlations | Statistical analysis of theme-sentiment relationships and co-occurrence |
| 9. QDPX export | Exports codebook and coded segments for QDA software interoperability |
| 10. Temporal analysis | Theme prevalence trends and emergence timelines (when timestamps available) |
| 11. Cross-run comparison | Compares themes, codes, and sentiment across runs; detects inter-model reliability |

Optional steps: researcher review points (after coding and/or theme generation,
in CSV or QDPX format), human verification (IRR), and AI decision audit logging.

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

**Your privacy:** pakhom runs entirely on your own machine. It collects no
telemetry and sends nothing about you or your data to its authors or any third
party. Your data is transmitted only to the AI provider you configure (OpenAI or
Anthropic, over HTTPS), and solely to perform the analysis you request; your API
key is read from your environment and is never written to logs, audit records,
or outputs. See [`SECURITY.md`](SECURITY.md) for the full data-handling
description.

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
tested at the integration level (the test suite has more than 4,900 expectations
pinning them against silent regression). They are
the contract a peer reviewer can check the package's claims against:

- **AC1**: AI is scaffold by architecture, not by configuration.
- **AC2**: Three modes; no fourth.
- **AC3**: No default mode; explicit declaration mandatory.
- **AC4**: Methodology stamped on every output (`run_metadata.json`,
  every CSV/JSON header, HTML stamp, plot watermarks).
- **AC5**: Soft-lock with audit trail; methodology change creates a new
  run with `parent_run_id` linkage (REDCap dev/production pattern).
- **AC6**: Symmetric researcher-engagement affordances across modes: reflexive
  memos and review pause-points exist in every mode (the Modes 2/3
  pause-points are opt-in, off by default).
- **AC7**: Universal Tier-0 transparency requirements in all modes.
- **AC8**: Modes are configurations of one architecture, never separate
  code paths.
- **AC9**: Methodology rules generated from config and injected into
  the model context every turn (Lin and Corley 2025 pattern).
- **AC10**: Stage-gating via filesystem state.

The methodology-modes vignette covers each commitment in narrative
context.

## For methodologists: rewrite-direction commitments (C1–C8)

Distinct from the mode-design commitments above, the package's
**algorithm-level** behaviour is governed by eight commitments — C1
through C8 — distilled from the rewrite-direction conversations that
shaped the package's design. These are *how the package thinks*, not *what modes
it offers*. They are equally load-bearing and equally regression-tested,
and any code change that touches coding, clustering, statistics, or
output rendering must honour them:

- **C1 — AI decides when to stop.** No hardcoded `n_themes`,
  `max_themes`, `max_passes`, `min_codes_per_theme`, or saturation
  thresholds. The AI judges saturation and clustering convergence;
  pakhom records the AI's articulation but never overrides it with a
  count gate. Enforced in `R/saturation_arbiter.R` (coding saturation)
  and `R/theme_algorithm_v2.R` (multi-pass clustering convergence).
- **C2 — Codes preserved through clustering.** Codes are atomic
  leaves; themes and subthemes are GROUPS of codes, not summaries that
  replace them. Clustering never mutates code names, descriptions, or
  segment assignments. This preserves entry → code → theme traceability
  and protects code-level nuance. Enforced by the `Code` S3 class in
  `R/12_theme_data.R`.
- **C3 — Live tracking artifacts during processing.** Researchers can
  `tail -F outputs/<run>/live/` mid-run and watch the codebook + theme
  hierarchy grow in real time. Three streamed files
  (`code_assignments.jsonl`, `codebook_live.json`,
  `code_to_cluster.json`) capture entry → code → cluster mappings as
  they happen. Enforced in `R/live_tracking.R`.
- **C4 — Dataset-agnostic.** Works with any corpus shape and any
  metric columns. No hardcoded column-name allowlists; auto-detect
  column types; statistics adapt to whatever numeric columns the data
  provides. A clinical-interview corpus with `age` + `medication_dose`
  produces the same quality of output as a Reddit corpus with
  `score` + `upvote_ratio`. Enforced by `.detect_metric_columns()` in
  `R/16_report_helpers.R` and by the dynamic
  `compare_theme_groups()` in `R/14_correlations.R`.
- **C5 — No catch-all / "Other" buckets.** In the inductive modes
  (Mode 2), the AI is never given an "Other" or "Miscellaneous" code
  to dump uncertain segments into. Every coded segment must articulate
  what it represents. During theme clustering, the AI's
  prompts in `R/theme_algorithm_v2.R` explicitly forbid bucket-label
  openers ("Various aspects of X", "Mixed experiences with Y"); the
  `.clustering_schema()` has no name/description fields at all so
  labeling pressure cannot leak into structural decisions. In Mode 3,
  the `anomaly` bucket is intentional and methodologically required —
  it surfaces framework-resistant data rather than hiding it.
- **C6 — Arbitrary research-question length/complexity.** No
  hardcoded character limits on the research focus; no assumption
  that the question is a single sentence. Multi-paragraph research
  briefs work the same as one-line questions.
- **C7 — Mode-aware behaviour.** Architecture-level branches on
  methodology mode where the modes genuinely require different
  behaviour (e.g., Mode 1 has no codebook + invokes the provocateur
  loop; Mode 3 pre-populates the codebook with framework constructs).
  Surface-level decisions (sentiment cutoff, prevalence bins, etc.)
  should NOT branch on mode — only deep architectural decisions do.
- **C8 — Publication-quality output shape.** The output target
  approximates an Eaton 2020-style per-theme subtheme
  table: subtheme name, n, Median(MAD) + Mean(SD) on the most
  interesting metrics, supporting quotes with metric tags. The
  package picks summary statistics appropriately for each metric —
  never hardcoded to particular column names. Enforced in
  `R/16_report_helpers.R::.build_subtheme_summary_table`.

**Any future contributor should re-read both AC1–AC10
and C1–C8 before changing the coding loop, the theme algorithm, the
statistical layer, or the report renderer.** These eighteen
commitments together are the contract the package promises peer
reviewers.

## Author

Developed by **Abanoub J. Armanious, MS**.

- ORCID: <https://orcid.org/0000-0002-7005-8297>
- GitHub: [@abanoub-armanious](https://github.com/abanoub-armanious)
- Google Scholar: [profile](https://scholar.google.com/citations?user=dGC45ngAAAAJ&hl=en)
- ResearchGate: [Abanoub-Armanious](https://www.researchgate.net/profile/Abanoub-Armanious)
- LinkedIn: [abanoubarmanious](https://www.linkedin.com/in/abanoubarmanious)
- Bug reports & questions: [GitHub issues](https://github.com/abanoub-armanious/pakhom/issues) or armaniousabanoub@gmail.com

### A note from the author

pakhom is the first R package I've built, developed over roughly two years. I've
worked hard to make it rigorous and genuinely useful (including anti-fabrication
checks, a transparency layer, and thousands of tests), but it's the work of one
person learning as they go, and there will be rough edges. If you hit a bug, find
something unclear, or have an idea, please contact me. I'll be grateful for your
patience and your feedback, and I'll do my best to make pakhom better for the
community.

## License

MIT
