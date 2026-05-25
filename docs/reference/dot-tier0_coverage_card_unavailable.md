# Unavailable-coverage card (NULL / unrecognized inputs)

Per AC4, absence of a Tier-0 card is itself a transparency signal –
rather than hide it, the report renders an explicit "not computed"
notice. Invoked from `render_tier0_coverage_card` when no method
matches.

## Usage

``` r
.tier0_coverage_card_unavailable()
```
