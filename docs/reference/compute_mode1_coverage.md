# Compute Mode 1 (Reflexive Scaffold) coverage from a finished provocateur run

Mode 1's analog of
[`compute_corpus_coverage`](https://abanoub-armanious.github.io/pakhom/reference/compute_corpus_coverage.md).
Where Mode 2/3 assert "the LLM saw every entry that survived
preprocessing" (no silent truncation), Mode 1 asserts:

## Usage

``` r
compute_mode1_coverage(
  reflection_log,
  theme_set,
  data,
  requested_categories = .VALID_PROVOCATION_CATEGORIES
)
```

## Arguments

- reflection_log:

  A `ResearcherReflectionLog` returned by
  [`run_provocateur_questioning`](https://abanoub-armanious.github.io/pakhom/reference/run_provocateur_questioning.md)
  (must be schema 1.1.0+ to carry the `provocation_attempts` +
  `skipped_themes` slots).

- theme_set:

  The `ThemeSet` the provocateur loop ran over.

- data:

  The corpus tibble passed to the loop (used for total entry count –
  `nrow(data)` – which the card surfaces as "corpus searchable for
  counter-evidence").

- requested_categories:

  Character vector of provocation categories the orchestrator requested
  (defaults to the full set of five). Used to compute the expected
  attempt-matrix size; supplying a subset here means the coverage card
  grades against that subset rather than all five.

## Value

A `ProvocationCoverage` S3 object (also inherits `Tier0Coverage`).

## Details

- every researcher-authored theme was challenged across every requested
  provocation category (no silent theme/category skip);

- the AI was given the FULL corpus when searching for counter- evidence
  (no silent corpus truncation – by construction in pakhom's prompt
  builders, which pass the entire corpus tibble to each per-category
  provocation function).

Distinguishing legitimate empty results from silent skips matters:
counter_narrative or disconfirming_evidence may legitimately return zero
provocations when no qualifying entries exist, and that is a valid
analytic outcome – not a coverage failure. The provocation loop's
per-attempt tracking (in `ResearcherReflectionLog$provocation_attempts`,
schema 1.1.0+) records one row per (theme, category) attempt regardless
of how many provocations the AI emitted, so this function can answer
"was the attempt made?" independently from "did the attempt produce
output?".

## See also

[`compute_corpus_coverage`](https://abanoub-armanious.github.io/pakhom/reference/compute_corpus_coverage.md)
for the Mode 2/3 counterpart;
[`render_tier0_coverage_card`](https://abanoub-armanious.github.io/pakhom/reference/render_tier0_coverage_card.md)
(the S3 generic that dispatches on the shared `Tier0Coverage` parent
class).

## Examples

``` r
if (FALSE) { # \dontrun{
# After a run_mode1 invocation:
result <- run_mode1(data = my_corpus, theme_set = my_themes,
                      config_path = "config.yaml")

# The coverage object is already on result$coverage; or recompute:
cov <- compute_mode1_coverage(
  reflection_log      = result$reflection_log,
  theme_set           = result$theme_set,
  data                = my_corpus,
  requested_categories = c("counter_narrative", "disconfirming_evidence")
)
cov$no_silent_skip       # headline boolean
cov$attempts_per_category # named list keyed by category
print(cov)
} # }
```
