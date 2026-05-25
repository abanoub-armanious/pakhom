# Render the Mode 3 Framework Declaration section

Phase 32 (audit H1 + H2): Mode 3 reports previously stamped the
methodology mode at the top ("M3 - Framework Applied") but never
surfaced WHICH theoretical framework was applied. A reviewer reading a
report could not reconstruct whether the analysis used the Theory of
Planned Behavior, COM-B, the Theoretical Domains Framework, or the
researcher's own custom YAML – which broke the methodology paper
provenance chain and made cross-run comparison opaque. This helper
renders the framework's identity (name + sha256 hash), its epistemic
stance, anomaly handling policy, and the full constructs list with
example indicators so the report is self-describing.

## Usage

``` r
.build_framework_declaration(spec, archive = NULL)
```

## Arguments

- spec:

  A `FrameworkSpec` object (from
  [`load_framework_spec`](https://abanoub-armanious.github.io/pakhom/reference/load_framework_spec.md)).
  NULL falls through to the unavailable variant.

- archive:

  Named list returned by
  [`archive_framework_spec`](https://abanoub-armanious.github.io/pakhom/reference/archive_framework_spec.md)
  carrying `path`, `relative_path`, and `hash`. NULL is acceptable but
  the rendered section will lack the file-link + sha256 fingerprint.

## Value

Character HTML/markdown string for the section.

## Details

Per AC4 ("methodology stamped on every output"), this section is
mandatory for any Mode 3 run. Absence (e.g., archive failed earlier in
the pipeline) renders an explicit "framework archive not available"
notice rather than silently omitting – the absence is itself a
transparency signal.
