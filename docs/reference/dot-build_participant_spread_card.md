# Render the per-theme Participant Distribution card

Empirical answer to Jowsey et al. 2025's Frankenstein finding that "none
of the Copilot outputs reported the participant spread". Three metrics
are surfaced as a meta card:

- `n_distinct_contributors` – count of unique authors

- `contributor_gini` – Gini coefficient (0 = even, 1 = one contributor
  takes everything)

- `top_contributor_share` – fraction from the most prolific contributor
  (the "is this one person's theme?" check)

## Usage

``` r
.build_participant_spread_card(ps)
```

## Arguments

- ps:

  participant_spread sub-list from
  [`aggregate_theme_statistics()`](https://abanoub-armanious.github.io/pakhom/reference/aggregate_theme_statistics.md)
  (or NULL/missing on legacy stats).

## Value

Character HTML/markdown string for the card.

## Details

Concentration warnings:

- When `n_distinct_contributors == 1`, renders a "single contributor"
  notice – the theme has zero participant spread.

- When `top_contributor_share > 0.5` (one contributor owns more than
  half), renders a caution banner.

Unavailable variant: when `participant_spread$available` is FALSE (no
`std_author` column in the data, or no non-NA author values for this
theme), renders a "Participant data not available" notice. Silent
omission is rejected because the absence itself carries methodological
signal (a Tier-0 universal that explicitly cannot be computed must say
so).
