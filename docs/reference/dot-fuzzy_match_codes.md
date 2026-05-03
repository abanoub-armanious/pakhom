# Match codes from source to target using normalized string distance

Match codes from source to target using normalized string distance

## Usage

``` r
.fuzzy_match_codes(source, target, threshold = 0.35)
```

## Arguments

- source:

  Character vector of codes to match

- target:

  Character vector of codes to match against

- threshold:

  Normalized distance threshold (0 = exact, 1 = anything matches)

## Value

List with `matched` (target codes that were matched) and `unmatched`
