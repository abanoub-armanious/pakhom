# Phase 56: AI saturation arbiter

Single 3-valued judgment call to the AI. Returns a list with verdict
(one of `"reached"`, `"not_yet"`, `"uncertain"`) plus articulation,
rationale, and a success flag (FALSE on parse/API failure).

## Usage

``` r
.ai_judge_saturation(
  state,
  provider,
  research_focus,
  n_coded,
  n_corpus,
  n_done,
  audit_log = NULL,
  response_cache = NULL
)
```

## Arguments

- state:

  ProgressiveCodingState with codebook + curve

- provider:

  AIProvider

- research_focus:

  Research focus string (for in-context judgment)

- n_coded:

  Number of entries CODED so far (skipped excluded)

- n_corpus:

  Total entries in the corpus

- n_done:

  Total entries PROCESSED so far (coded + skipped)

- audit_log:

  Optional AuditLog

- response_cache:

  Optional ResponseCache

## Value

list(verdict, articulation, rationale, success)

## Details

Articulations under 30 chars downgrade "reached" to "not_yet" – Phase
52's anti-vacuous pattern. Failures are NOT counted against the verdict
(the caller's circuit breaker tracks failures separately).
