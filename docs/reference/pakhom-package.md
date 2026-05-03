# pakhom: AI-Assisted Reflexive Thematic Analysis

Conducts AI-assisted reflexive thematic analysis following Braun &
Clarke's approach. Uses progressive sequential coding, iterative
bottom-up theme generation, and deterministic code-path cascading.
Supports OpenAI and Anthropic providers, codebook-first learning from
prior studies, checkpoint/resume, and publication-quality HTML
reporting.

## Main function

[`run_analysis`](run_analysis.md) — orchestrates the full pipeline from
a YAML config.

## Key features

- Progressive sequential coding (entries read one at a time, like NVivo)

- Thematic saturation detection (triangulated: code creation rate, reuse
  stability, AI self-assessment)

- Iterative bottom-up theme generation (codes merged into clusters
  through multiple passes until convergence)

- Deterministic code-path cascading (entries map to themes through
  codes)

- Code-aware sentiment analysis (sentiment scored after coding, using
  codes as context)

- Codebook-first learning from prior studies (QDPX, Excel, CSV codebooks
  with full theme/subtheme/code hierarchies)

- Researcher review points (pause after coding or theme generation)

- Checkpoint/resume for long-running analyses

- Correlation analysis of theme-sentiment relationships

- Publication-quality HTML report with saturation curves, theme
  narratives, and interactive tables

## Author

**Maintainer**: Abanoub J. Armanious <armaniousabanoub@gmail.com>
