# pakhom

> AI-Integrated Reflexive Thematic Analysis Following Braun & Clarke
> (2006)

**pakhom** is an R package that automates reflexive thematic
analysis using OpenAI or Anthropic APIs. It takes a dataset of text
entries (forum posts, survey responses, interview transcripts) and
produces a publication-quality HTML report containing themes, codes,
sentiment analysis, correlation analysis, and supporting evidence.

You give it your data and your research question. It gives you back a
complete thematic analysis – including the kinds of tables, figures, and
statistical tests you would normally produce by hand over weeks of
manual coding.

## Key Features

- **Progressive sequential coding** – entries are read one at a time,
  just as a human researcher would in NVivo. The AI codes applicable
  text segments inline, building and reusing a codebook organically as
  it goes
- **Thematic saturation detection** – triangulated monitoring (code
  creation rate, reuse stability, AI self-assessment) stops coding when
  no new patterns are emerging, saving time and cost
- **Iterative bottom-up theme generation** – codes are merged into
  clusters through sequential passes until no more productive groupings
  exist. Themes and subthemes emerge organically from the data, not from
  top-down labels
- **Deterministic code-path cascading** – entries map to themes through
  their codes (no AI re-reading of raw text), faithful to the inductive
  process
- **Code-aware sentiment analysis** – sentiment is scored after coding,
  using assigned codes as context for more accurate emotional valence
  detection
- **Codebook-first learning from prior studies** – learns coding
  conventions, structural relationships, and discarded patterns from
  QDPX codebooks (NVivo), Excel, or CSV files. Manuscripts serve as
  supplementary clarification
- **Researcher review points** – pause the pipeline after coding or
  theme generation to curate the AI’s output before continuing
- **Checkpoint/resume** – long-running analyses can be interrupted and
  resumed without losing progress. Flat checkpoint architecture ensures
  reliable resume across retries
- **Inter-rater reliability** – built-in IRR computation (Cohen’s kappa,
  Krippendorff’s alpha) for human verification of AI-generated codes
- **Multiple AI providers** – works with OpenAI (GPT-4o) or Anthropic
  (Claude), with optional parallel multi-model mode for cross-provider
  analysis
- **Inter-model reliability** – run the pipeline with different AI
  models and compare results via
  [`compare_models()`](reference/compare_models.md) to compute
  inter-model agreement (Cohen’s kappa, theme similarity, sentiment
  correlation)
- **Embedding-augmented theme emergence** – uses text embeddings and
  code co-occurrence statistics to provide quantitative context during
  iterative code merging, complementing the AI’s semantic judgment
- **Researcher reflexivity** – injects researcher positionality,
  research paradigm, and reflexive notes into all AI prompts, aligning
  with Braun & Clarke’s emphasis on reflexive practice
- **AI decision audit trail** – every AI decision (code assignments,
  merges, skips, saturation signals) is logged to a JSONL file with full
  rationale for post-hoc transparency review
- **QDPX export** – exports codebook and coded segments in QDPX format
  for import into NVivo, ATLAS.ti, or MAXQDA for manual verification
- **Longitudinal analysis** – when entries have timestamps, tracks theme
  prevalence trends and emergence timelines within a single run
- **Publication-quality reports** – generates self-contained HTML
  reports with theme narratives, representative quotes, sentiment
  breakdowns, saturation curves, correlation tables, confidence
  intervals, and decision transparency
- **Run comparison** – compare results across pipeline runs to assess
  stability and track how themes evolve

## Quick Start

``` r
# Install from GitHub
devtools::install_github("abanoub-armanious/pakhom")

# Set your API key in .Renviron
usethis::edit_r_environ()
# Add: OPENAI_API_KEY=sk-your-key-here

# Option A: Interactive config wizard (web UI)
pakhom::config_wizard_app()

# Option B: CLI config wizard
pakhom::config_wizard()

# Option C: Create config programmatically
pakhom::create_config(
  study_name = "My Study",
  research_focus = "How does X relate to Y?",
  database_path = "my_data.db",
  output_path = "config.yaml"
)

# Run the analysis
results <- pakhom::run_analysis("config.yaml")
```

## Pipeline Overview

| Step | What it does |
|----|----|
| 1\. Learn from prior studies | Parses QDPX codebooks and manuscripts for coding conventions and structural patterns |
| 2\. Load & preprocess data | Reads from SQLite database, cleans text, standardizes columns |
| 3\. Progressive coding | AI reads each entry sequentially, coding applicable text with existing or novel codes |
| 4\. Saturation detection | Monitors code creation rate and reuse stability; stops when codebook plateaus |
| 5\. Sentiment analysis | AI scores sentiment on coded entries, using codes as context |
| 6\. Theme generation | Sequential bottom-up merging of codes into clusters across multiple passes |
| 7\. Theme cascading | Deterministic entry-to-theme mapping through the code hierarchy |
| 8\. Correlations | Statistical analysis of theme-sentiment relationships and co-occurrence |
| 9\. QDPX export | Exports codebook and coded segments for QDA software interoperability |
| 10\. Temporal analysis | Theme prevalence trends and emergence timelines (when timestamps available) |
| 11\. Cross-run comparison | Compares themes, codes, and sentiment across runs; detects inter-model reliability |

Optional steps: researcher review points (after coding and/or theme
generation, in CSV or QDPX format), human verification (IRR), parallel
multi-model mode, and AI decision audit logging.

## Who is this for?

Researchers who:

- Are conducting qualitative or mixed-methods research with large text
  datasets
- Want to use AI to assist (not replace) their analytical process
- May or may not have deep experience with R programming
- Want reproducible, transparent, and auditable thematic analysis

## Requirements

- R \>= 4.1.0 and RStudio (recommended)
- An API key from [OpenAI](https://platform.openai.com/) or
  [Anthropic](https://console.anthropic.com/)
- Your data in a SQLite database (.db file)

**Security note:** Always store API keys in environment variables
(`.Renviron`) rather than in config files. The package warns if it
detects a key pasted directly into `config.yaml`.

## Multi-Model Reliability

To assess inter-model reliability, run the pipeline multiple times with
different AI providers/models:

``` r
# Run 1: OpenAI
results1 <- run_analysis("config.yaml")

# Run 2: Change provider to Anthropic in config, then re-run
results2 <- run_analysis("config.yaml")

# Compare models
comparison <- compare_models("outputs/")
```

An optional parallel multi-model mode is also available
(`ai.multi_model.enabled: true` in config). Note: parallel mode disables
reviewer pauses to preserve model independence. For reviewer-guided
analysis, use single-model sequential runs.

## Documentation

- **[Getting Started vignette](articles/getting-started.md)** –
  step-by-step guide from installation to interpreting results
- **[Function reference](reference/index.md)** – documentation for all
  exported functions
- **[`config_wizard_app()`](reference/config_wizard_app.md)** –
  interactive web-based config builder

## Author

Developed by **[Abanoub J. Armanious,
MS](https://www.linkedin.com/in/abanoubarmanious/)**.

## License

MIT
