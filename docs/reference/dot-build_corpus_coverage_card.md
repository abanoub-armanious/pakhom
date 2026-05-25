# Render the Tier-0 corpus-coverage assertion card

Empirical answer to Jowsey et al. 2025's Frankenstein finding that
Microsoft Copilot "drew themes from only the first 2-3 pages of data."
pakhom processes entries strictly one at a time; this card surfaces the
funnel from preprocessed data to LLM-processed entries to coded entries
and asserts the headline `no_silent_truncation` claim explicitly.

## Usage

``` r
.build_corpus_coverage_card(coverage)
```

## Arguments

- coverage:

  A `CorpusCoverage` object from
  [`compute_corpus_coverage`](https://abanoub-armanious.github.io/pakhom/reference/compute_corpus_coverage.md),
  or NULL.

## Value

Character HTML/markdown string for the card.

## Details

Pairs with the T0.1 verification dashboard: T0.1 says "no fabrications",
T0.3 says "no silent truncation". Both are Tier-0 transparency cards
rendered above the substantive analysis so reviewers see the integrity
claims first.

Unavailable variant: when `coverage` is NULL (legacy report call, or
coverage computation failed) the card renders an explicit "coverage data
unavailable" notice rather than omitting silently. Per AC4 (methodology
stamped on every output), absence of the card is itself a failure signal
so we say so.
