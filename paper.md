<!--
NOTE TO MAINTAINER, before any JOSS submission:
  1. ORCID is set below. Confirm the `affiliation` (currently "Independent
     Researcher") is how you want to be listed, or set an institution.
  2. JOSS requires the software repository to be PUBLIC; make it public first.
This paper.md / paper.bib are excluded from the built package tarball via .Rbuildignore.
-->
---
title: 'pakhom: AI-Assisted Thematic Analysis with Methodology-as-Architecture'
tags:
  - R
  - qualitative research
  - thematic analysis
  - reflexive thematic analysis
  - large language models
  - artificial intelligence
  - reproducibility
authors:
  - name: Abanoub J. Armanious
    orcid: 0000-0002-7005-8297
    affiliation: 1
affiliations:
  - name: Independent Researcher
    index: 1
date: 4 June 2026
bibliography: paper.bib
---

# Summary

`pakhom` is an R package for conducting **AI-assisted thematic analysis** (TA) of
qualitative text data with methodological rigor and auditable transparency. TA is
among the most widely used qualitative methods across the social, health, and
behavioral sciences [@braun2006using; @braun2022thematic], and researchers
increasingly enlist large language models (LLMs) to assist or automate it.
Unconstrained automation is risky: a recent evaluation of generative-AI thematic
analysis reported multiple errors and between-method discrepancies serious enough
that the tool could not be recommended for the task [@jowsey2025frankenstein], and
assistant-tuned models are optimized to complete the user's task and tend to go along
with the user's framing rather than challenge it — a disposition that is corrosive to
critical interpretation [@sarkar2024challenge].
`pakhom` responds by encoding the *methodology itself* into the software
architecture: the researcher declares one of three methodologically distinct modes,
and the AI's permitted role — what it may propose, what the researcher must author,
and which transparency artifacts are mandatory — is enforced at the code and schema
level rather than left to prompt-writing discipline.

A mandatory transparency layer binds every run in every mode. Each AI-attributed
quotation is verified verbatim against the analytic corpus (the cleaned text the
model coded; the raw platform text is preserved alongside it) through a multi-step
provenance ladder; a quotation that cannot be traced to its source is dropped and
logged rather than rendered. Representative quotes are spread across contributors,
whole-corpus coverage is asserted rather than silently truncated, and every model
decision is written to a JSON-lines audit trail stamped with the active methodology
mode. `pakhom` supports OpenAI and Anthropic models, exports to the REFI-QDA QDPX
standard for interoperability with established qualitative-analysis software, and
renders a publication-quality HTML report with per-theme statistics,
provenance-checked exemplar quotes, and an auto-generated methodology appendix.

# Statement of need

Qualitative researchers face growing pressure to apply LLMs to coding and theme
development at a scale that manual analysis cannot match. The most direct route —
pasting transcripts into a general-purpose chat interface — provides no
methodological guardrails, no guarantee that quoted evidence is genuine, no record
of how conclusions were reached, and no path to a reproducible, peer-reviewable
analysis. The consequences are concrete: in a systematic appraisal across five
studies, over half of the supporting quotations a leading assistant produced were
modified or fabricated — absent from or altered relative to the source data — its
themes were drawn largely from the first two to three pages of datasets running to
150 pages, and it did not report the spread of participants represented, leading the
authors to conclude that the tool could not be recommended for thematic analysis
[@jowsey2025frankenstein; @jowsey2025correction]. Tellingly, the same appraisal found
that the human analysts also produced fabricated quotes, at a substantially lower
rate. Fabrication and selective reading are failure modes of qualitative analysis in
general, which the scale and fluency of AI only amplify.
These failures are compounded by the disposition of assistant-tuned models to advance
the user's task rather than interrogate its premises, which undermines the critical
stance that qualitative interpretation requires [@sarkar2024challenge]. `pakhom`'s mandatory
transparency layer answers each failure architecturally: verbatim quote provenance,
whole-corpus coverage, and per-theme participant spread.

`pakhom` is designed to make AI-assisted TA *defensible under peer review*. Three
modes span the methodological spectrum and constrain the model accordingly:

- **Reflexive Scaffold (Mode 1)** keeps interpretive authorship entirely with the
  researcher; the model is restricted to extractive and *adversarial* operations —
  surfacing counter-evidence, blind spots, and questions the analyst may have missed
  — operationalizing the call for AI that challenges rather than obeys
  [@sarkar2024challenge]. It never proposes themes.
- **Codebook Collaborative (Mode 2)** lets the model propose codes and cluster-level
  groupings inductively, with the researcher accepting, editing, or rejecting each;
  the codebook remains the researcher's deliverable.
- **Framework Applied (Mode 3)** deductively applies a researcher-supplied framework
  (the Theory of Planned Behavior, COM-B, and the Theoretical Domains Framework are
  included as built-ins), flagging framework-resistant data as explicit anomalies
  rather than forcing a fit.

Because these commitments are enforced architecturally rather than by prompt, their
integrity does not depend on the user remembering to ask for them. The clustering
step, for instance, can only return a partition of the existing codes and carries no
field through which the model could invent or rename one, so codes are *grouped*,
never silently rewritten — preserving entry-to-code-to-theme traceability. The result
is an analysis whose provenance, coverage, and decision trail a reviewer can inspect,
which directly addresses the trust gap limiting scholarly acceptance of AI-assisted
qualitative work.

# Design and key features

`pakhom` implements the analytic pipeline as a sequence of auditable stages:

- **Methodology rules as standing context.** Per-mode rules are generated from the
  configuration, injected into *every* model call, and archived to disk for peer
  review, so the methodological posture is present on every turn rather than at a
  single prompt.
- **The AI as analyst, the package as its calculator.** Before coding, an AI
  "methodology assistant" articulates a relevance criterion and, for each numeric or
  temporal variable, chooses summary statistics that are *honest* for that variable's
  distribution — a right-skewed count is summarized by a median and tail measures,
  not a mean and standard deviation. The package performs the computation: every
  statistic in the structured report is the package's, with the model choosing the
  method rather than the value. (The narrative executive summary is interpretive
  AI prose, not a source of the report's computed statistics.)
- **Embedding-free, multi-pass clustering.** Themes are formed by a multi-pass
  procedure in which the model groups codes and declares its own convergence;
  clustering depth (flat or hierarchical) is an emergent property of the data rather
  than a fixed parameter.
- **Transparency and reproducibility.** Quote provenance, participant spread, and
  whole-corpus coverage are mandatory; runs are stamped with a methodology mode,
  configuration hash, and provider, supporting auditable comparison across re-runs;
  and reflexive memos are first-class, round-trippable data. Because the underlying
  models are generative, individual coding, sentiment, and synthesis outputs are not
  bit-for-bit reproducible across runs (and the AI-judged saturation and clustering
  convergence are model decisions, not threshold gates); the package therefore makes
  runs *auditable and comparable* — through provenance, stamping, and cross-run
  comparison — rather than claiming deterministic replay.

The package is covered by a test suite of more than 4,900 expectations that runs
entirely offline (all model calls are mocked) and is documented by a getting-started
walkthrough and a methodology-modes vignette.

# State of the field

Established QDA environments — NVivo [@nvivo], ATLAS.ti [@atlasti], and MAXQDA
[@maxqda] — support qualitative coding at scale but do not orchestrate LLMs under
methodological constraint; `pakhom` interoperates with them rather than replacing
them, exchanging codebooks and coded segments via the REFI-QDA (QDPX) standard
[@evers2020refi]. Within R, RQDA [@rqda] offered manual qualitative coding but was
archived from CRAN in 2020. Conversely, general-purpose R interfaces to LLM APIs —
ellmer [@ellmer], tidyllm [@tidyllm], and gptstudio [@gptstudio] — expose model
calls without a thematic-analysis methodology or a provenance guarantee.
`pakhom`'s contribution is the integration of an LLM-driven TA pipeline
with architecturally enforced methodological modes and a mandatory transparency
layer, grounded in the documented failure modes of unconstrained LLM analysis
[@jowsey2025frankenstein; @sarkar2024challenge] and in established reflexive-TA
practice [@braun2022thematic].

# Acknowledgements

The package name, *pakhom*, is the Coptic Egyptian form of Pachomius (c. 292–348 CE),
whose written *Rule* of communal discipline established the genre of
methodology-as-written-document.

# References
