# pakhom: AI-Assisted Reflexive Thematic Analysis

Conducts AI-assisted reflexive thematic analysis with methodology
codified at the architectural level. Three methodologically-distinct
operating modes (Reflexive Scaffold, Codebook Collaborative, Framework
Applied) shape the AI's role explicitly so the chosen epistemic stance
is visible to reviewers, replicable across runs, and stamped onto every
output.

## The name

**pakhom** (Coptic *eagle*) is the native Coptic Egyptian form of the
name of Saint Pachomius the Great (c. 292-348 CE), the desert abbot
whose written Rule established the genre of
methodology-as-written-document in Christian tradition. The Pachomian
Rule was the first codified framework for organized communal practice;
it transformed the unruly anchorite tradition into reproducible,
inspectable, transmissible discipline. This package is a digital
descendant of that tradition: AI behavior in qualitative analysis is
constrained at the architectural level by methodologically-coherent
rules, not at the configuration level by user discipline. Pakhom
codified the Rule; pakhom codifies the methodology-as-permission-
structure.

## Three methodology modes

Each mode encodes a different posture for AI agency. The mode
declaration is mandatory in every config (no default); it is locked at
run start, stamped on every output, and any change creates a fork run
with parent_run_id linkage.

- `reflexive_scaffold` (Mode 1):

  AI as Socratic gadfly (Sarkar 2024). The researcher authors codes and
  themes (typically in NVivo / ATLAS.ti); pakhom contributes the
  provocateur loop that surfaces counter-narratives, absent voices,
  alternative interpretations, disconfirming evidence, and
  assumption-surfacing terms. The AI never names themes or codes. Use
  [`run_mode1`](https://abanoub-armanious.github.io/pakhom/reference/run_mode1.md).
  Per AC6 (symmetric obligations), Mode 1's burden parity with Modes 2/3
  is delivered through *reflexive memos* – typed Markdown notes
  round-tripped via YAML frontmatter
  ([`add_memo`](https://abanoub-armanious.github.io/pakhom/reference/add_memo.md),
  [`persist_memos`](https://abanoub-armanious.github.io/pakhom/reference/persist_memos.md)).

- `codebook_collaborative` (Mode 2):

  AI proposes codes; researcher gates each at the codebook + theme
  review pause-points. The auto-pipeline of
  [`run_analysis`](https://abanoub-armanious.github.io/pakhom/reference/run_analysis.md);
  this is what users coming from a codebook TA / template TA tradition
  will recognize. IRR + saturation are quality diagnostics; researcher
  review points interleave with the AI's progressive coding pass.

- `framework_applied` (Mode 3):

  Researcher provides a theoretical framework (e.g., Theory of Planned
  Behavior, COM-B, Theoretical Domains Framework – pre-built specs ship
  in `inst/extdata/frameworks/`); AI applies it verbatim and flags
  entries that resist the framework as anomalies per the framework's
  anomaly_handling policy. Use
  [`run_analysis`](https://abanoub-armanious.github.io/pakhom/reference/run_analysis.md)
  with `config$methodology$framework_spec_path` set. The framework spec
  is archived byte-equivalently into the run dir and its sha256 is
  stamped into `run_metadata.json`.

## Architectural commitments (AC1-AC10)

These commitments are load-bearing and do not weaken across modes.

- **AC1**: AI is scaffold by architecture, not by configuration.

- **AC2**: Three modes; no fourth.

- **AC3**: No default mode; explicit declaration mandatory.

- **AC4**: Methodology stamped on every output (ClinicalTrials.gov
  pattern – run_metadata.json, every CSV/JSON header, HTML stamp).

- **AC5**: Soft-lock with audit trail; methodology change creates a new
  run with parent_run_id linkage (REDCap dev/production pattern).

- **AC6**: Symmetric researcher-burden obligations across modes
  (anti-gaming).

- **AC7**: Universal Tier-0 transparency requirements (T0.1 quote
  provenance, T0.2 participant spread, T0.3 coverage) in all modes.

- **AC8**: Modes are configurations of one architecture, never separate
  code paths.

- **AC9**: Methodology rules generated from config and injected into the
  model context every turn.

- **AC10**: Stage-gating via filesystem state.

## Tier-0 universal transparency requirements

Three commitments mandatory in every mode, addressing the most-cited
empirical critiques of LLM-for-TA tools.

- T0.1 – Quote provenance + 4-step verification ladder:

  Every AI-attributed verbatim claim runs through strict offline match,
  normalized match, substring search, and embedding similarity.
  Fabricated quotes are dropped silently and logged to
  `fabrication_log.csv`. Mode 1 + Anthropic + framework_applied
  constraints handled per provider.

- T0.2 – Participant spread per theme:

  Every theme reports n_distinct_contributors + Gini coefficient + top
  contributor share, so themes that look prevalent but rest on one heavy
  poster get surfaced (Jowsey et al. 2025 "Frankenstein" finding).

- T0.3 – Whole-corpus coverage assertion:

  Modes 2/3 assert "no silent truncation in the LLM call path" via
  [`compute_corpus_coverage`](https://abanoub-armanious.github.io/pakhom/reference/compute_corpus_coverage.md);
  Mode 1 asserts "no silent skip across themes x provocation categories"
  via
  [`compute_mode1_coverage`](https://abanoub-armanious.github.io/pakhom/reference/compute_mode1_coverage.md).
  Both inherit a virtual `Tier0Coverage` parent class so the report
  dispatches uniformly via
  [`render_tier0_coverage_card`](https://abanoub-armanious.github.io/pakhom/reference/render_tier0_coverage_card.md).

## Main entry points

- [`run_analysis`](https://abanoub-armanious.github.io/pakhom/reference/run_analysis.md):

  Modes 2 + 3 orchestrator (data load -\> coding -\> sentiment -\>
  themes -\> correlations -\> report -\> finalize_run).

- [`run_mode1`](https://abanoub-armanious.github.io/pakhom/reference/run_mode1.md):

  Mode 1 orchestrator (provocateur loop + memos + Mode 1 report).
  Mirrors run_analysis's scaffolding but routes through
  [`run_provocateur_questioning`](https://abanoub-armanious.github.io/pakhom/reference/run_provocateur_questioning.md).

- [`create_config`](https://abanoub-armanious.github.io/pakhom/reference/create_config.md)
  /
  [`config_wizard_app`](https://abanoub-armanious.github.io/pakhom/reference/config_wizard_app.md):

  Create a config programmatically or via a Shiny wizard.

- [`load_framework_spec`](https://abanoub-armanious.github.io/pakhom/reference/load_framework_spec.md):

  Load a theoretical framework for Mode 3 (built-in: `"tpb"`, `"comb"`,
  `"tdf"`).

- [`add_memo`](https://abanoub-armanious.github.io/pakhom/reference/add_memo.md)
  /
  [`persist_memos`](https://abanoub-armanious.github.io/pakhom/reference/persist_memos.md):

  Mode 1 reflexive memo CRUD + Markdown round-trip.

- [`compare_runs`](https://abanoub-armanious.github.io/pakhom/reference/compare_runs.md)
  /
  [`compare_models`](https://abanoub-armanious.github.io/pakhom/reference/compare_models.md):

  Cross-run and inter-model reliability comparisons.

## Provider support

OpenAI (GPT-4o family) and Anthropic (Claude family) with a unified
[`ai_complete`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md)
abstraction. Mode 3 + Anthropic structurally precludes the Citations API
(forced `tool_use` schema and Citations API output are mutually
exclusive on the same response); the Mode 3 report renders an explicit
footnote disclosing this rather than letting reviewers infer a bug.

## Further reading

- [`vignette("getting-started")`](https://abanoub-armanious.github.io/pakhom/articles/getting-started.md)
  – step-by-step Mode 2 walkthrough

- [`vignette("methodology-modes")`](https://abanoub-armanious.github.io/pakhom/articles/methodology-modes.md)
  – choosing between the three modes; worked examples for each

- Sarkar 2024 (CACM) "AI Should Challenge, Not Obey" – Mode 1 motivation

- Braun and Clarke 2022 – reflexive TA foundation

- Jowsey et al. 2025 (PLOS One, doi:10.1371/journal.pone.0330217) – the
  "Frankenstein" finding that motivated Tier-0

## See also

Useful links:

- <https://github.com/abanoub-armanious/pakhom>

- <https://abanoub-armanious.github.io/pakhom/>

- Report bugs at <https://github.com/abanoub-armanious/pakhom/issues>

## Author

**Maintainer**: Abanoub J. Armanious <armaniousabanoub@gmail.com>
