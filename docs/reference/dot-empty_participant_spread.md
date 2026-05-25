# Empty-shape result for the participant-spread sub-list

Used in two cases: themes with zero entries (no contributors to count),
and entries datasets that don't carry an `std_author` column. Keeping a
stable shape across those cases simplifies downstream rendering.

## Usage

``` r
.empty_participant_spread()
```
