# Map a methodology mode to its short-code (M1 / M2 / M3)

Used in run-directory names and filenames where the full mode string
would be visually noisy. The mapping is fixed: reflexive_scaffold = M1,
codebook_collaborative = M2, framework_applied = M3. Returns `"M?"` for
unknown / NULL modes (visible-failure rather than silent-empty-suffix).

## Usage

``` r
methodology_short_code(mode)
```

## Arguments

- mode:

  Character, one of the methodology modes.

## Value

Character short-code.
