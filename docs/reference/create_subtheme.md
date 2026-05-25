# Create a Subtheme S3 object

First-class container that holds a set of Code objects within a Theme.
Use NA_character\_ for name when codes are not yet sub-grouped (a
"virtual" subtheme that will be populated by Phase 52's clustering).

## Usage

``` r
create_subtheme(
  name = NA_character_,
  description = "",
  codes = list(),
  subthemes = list()
)
```

## Arguments

- name:

  Subtheme name; NA_character\_ for virtual/ungrouped

- description:

  Subtheme description

- codes:

  List of Code S3 objects (or character vector of code names — coerced
  to stub Codes for use in tests / non-coding-state contexts)

- subthemes:

  List of nested Subtheme S3 objects (or raw lists coerced via recursive
  create_subtheme call). Phase 58 Tier 1 C-12 added nested subthemes to
  support depth-N HAC walker decomposition. Empty list = leaf Subtheme
  (no nested children).

## Value

Subtheme S3 object
