# Schema for the Phase 56 AI saturation arbiter response

Used by
[`.ai_judge_saturation()`](https://abanoub-armanious.github.io/pakhom/reference/dot-ai_judge_saturation.md)
during progressive coding to ask the model whether thematic saturation
has been reached. Replaces the pre-Phase-56 binary
`.saturation_schema()` (novel_patterns_remaining

- reasoning). The new shape mirrors the Phase 52 theme-decision schema:

## Usage

``` r
.saturation_decision_schema()
```

## Details

\(a\) Articulation requirement – the model must FIRST describe what it
observes (code growth pattern, codebook composition, reuse density)
before committing to a verdict. Vacuous articulations (\<30 chars) force
a downgrade from "reached" -\> "not_yet" so the AI can't declare
saturation without substantive reasoning. Same anti-vacuous pattern
Phase 52 uses for theme decisions.

\(b\) Three-valued verdict instead of boolean – the pre-Phase-56 path
forced a binary novel_patterns_remaining: yes/no. The new shape adds
"uncertain" so the AI can decline to judge when the evidence is
insufficient (e.g., very early in coding). Per C1 ("AI decides when to
stop"), an "uncertain" verdict means "continue coding; re-check later"
rather than forcing a hardcoded min-entries gate.

\(c\) Rationale field – short justification (1-2 sentences) that must
reference the most distinctive evidence from the prompt.


      {
        "articulation": string,
        "verdict": "reached" | "not_yet" | "uncertain",
        "rationale": string
      }
