# pakhom 1.0.1

- Saturation checks are now scheduled by the number of coded entries rather
  than the raw corpus size. On corpora where most entries are skipped, the
  check is evaluated at appropriate intervals instead of being keyed to total
  corpus size. Finalized runs and existing checkpoints are unaffected.

# pakhom 1.0.0

> This is the initial release of pakhom.

pakhom conducts AI-assisted thematic analysis with *methodology as architecture*:
what a run does is fixed by the methodology mode declared in its configuration,
not left to user discipline at call time. The same corpus, analyzed under
different declared methodologies, is treated differently by construction.

## Three methodology modes

- **Reflexive scaffold (Mode 1).** An AI "provocateur" interrogates the
  researcher's own coding rather than authoring themes. It surfaces
  counter-narratives, blind spots, and alternative readings, with a reflexive
  memo trail. Counter-evidence provocations draw on a bounded, disclosed
  sample of corpus entries from outside the theme, so disconfirming readings
  are grounded in the data. For researchers who hold that interpretation must
  stay human.
- **Codebook collaborative (Mode 2).** Inductive, bottom-up coding of the corpus
  into an emergent codebook, then multi-pass AI-judged clustering into themes.
- **Framework applied (Mode 3).** Deductive coding against a supplied framework
  (the Theory of Planned Behavior, COM-B, and the Theoretical Domains Framework
  are built in), with explicit, transparent handling of segments the framework
  does not capture.

## Design principles

- **The AI judges convergence; the package sets no count thresholds.** Theme
  count, clustering depth, and saturation are decided per data, research
  question, and codebook, never against a hardcoded target. A flat result and
  a deep hierarchy are equally valid outcomes.
- **The package groups codes; it never combines them into new codes.** Clustering
  returns only a partition of the existing codes; a theme's membership is the
  union of the grouped codes; labeling is a separate pass that runs after
  clustering.
- **Explain, don't gate.** Statistics, small-sample cautions, and exclusions are
  surfaced with their evidence rather than silently dropped or hidden behind a
  pass/fail threshold.

## AI as analyst

The Methodology Assistant lets the AI choose, per metric column, which
descriptive statistics are meaningful for the data at hand and articulate why,
drawing from a fixed catalog of deterministic primitives. The AI reasons; the
package computes. It also returns a per-column small-sample reliability floor,
used to flag spread and shape statistics computed on too few entries.

## Universal transparency and anti-fabrication

Every mode, with no opt-out, enforces a foundational transparency layer:

- **Quote provenance.** Every AI-attributed quote passes a verification
  ladder against the source text (string match, normalized match, substring
  search, plus an optional embedding-similarity step); fabricated or drifted
  quotes are dropped and logged, never rendered, and verification successes
  are logged too, so verification rates carry a real denominator. (This
  stance is motivated by Jowsey et al. 2025, the "Frankenstein" finding on
  generative-AI quote fabrication in qualitative work.)
- **Whole-corpus coverage.** Reports assert what fraction of the corpus was
  processed and coded, and distinguish intentional early stops (saturation) from
  silent truncation. Within-entry truncation against the per-entry prompt cap
  is measured and disclosed on the coverage card (entries truncated,
  characters sent to the model), never silently absorbed.
- **Participant spread.** Per-theme participant counts guard against a theme that
  rests on a single prolific voice.
- **Audit trail.** Every AI decision is logged to JSONL with a methodology stamp,
  and every output carries the methodology declaration.

## Reporting

Publication-quality, self-contained HTML reports, plus CSV and JSON artifacts:
per-theme cards with representative quotes selected by analytic fit; a
paper-style per-subtheme statistics layer; correlation analysis that flags
circular, analyst-internal pairs rather than headlining them; a theme
co-occurrence network; a Longitudinal Patterns section (theme prevalence and
emergence over time, also returned to the caller as `temporal_results`); a
research-question
coverage section showing where each named facet of the focus landed across the
themes; and an auto-generated methodology appendix that describes the methods
actually run. A companion methodological-transparency report bundles the
provenance, coverage, and audit evidence for peer review.

## Workflow and interoperability

- **Providers.** OpenAI and Anthropic, with provider-appropriate handling of
  structured output and source-grounded citations. By design (the
  anti-fabrication layer), Anthropic coding uses the Citations API prevention path
  while OpenAI uses the forced-tool_use schema path, and semantic code retrieval
  is OpenAI-only. This per-provider asymmetry adds a small coding-path component
  to any OpenAI-vs-Anthropic comparison and is now disclosed wherever
  cross-provider comparison is documented (`?compare_models`, README), with the
  recommendation to corroborate label-level metrics with content-level theme
  correspondence and to prefer same-provider repeats for pure inter-model
  reliability.
- **Reproducibility.** Checkpointed, resumable runs; soft-lock and
  `parent_run_id` for auditable, comparable re-runs.
- **Failure containment.** An aggregate AI-failure breaker stops the run with
  a resume hint when AI calls fail for too many consecutive entries or too
  large a fraction of attempted entries (configurable via
  `max_consecutive_entry_failures` and `max_failed_entry_fraction`); only
  real call failures count, never AI-judged skips, and failed entries are
  retried on resume. A crashed sentiment step resumes from its partial
  checkpoint instead of re-scoring the corpus.
- **Researcher review.** Optional mid-pipeline pause points to review and edit
  the codebook and themes (via CSV or QDPX) before the report is built.
- **QDA-software interoperability.** QDPX export structurally conformant to
  the REFI-QDA project-exchange format (`urn:QDA-XML:project:1.0`), with
  GUID-identified elements and GUID-named plain-text sources, for import into
  NVivo, ATLAS.ti, and MAXQDA.
- **Reflexive memos as data.** Typed memos with a Markdown round-trip.
- **Configuration.** A guided Shiny wizard and CLI helpers write a valid
  configuration, including the mandatory methodology block.

## Methodology and acknowledgements

pakhom's design is empirically motivated by Sarkar 2024 (*AI Should Challenge,
Not Obey*), Jowsey et al. 2025 (the Frankenstein finding), and Braun and Clarke
2022 (reflexive thematic analysis). The name *pakhom* (Coptic for "eagle")
honors Saint Pachomius the Great (c. 292-348 CE), whose written Rule of communal
discipline established the genre of methodology-as-written-document, of which
this package is a digital descendant.
