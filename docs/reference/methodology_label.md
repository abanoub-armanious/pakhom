# Human-readable label for a methodology mode

Used in display contexts (HTML badges, console banners) where the raw
mode string would be terse. Returns the mode with a Mode-N prefix:
`"M1 - Reflexive Scaffold"`, etc. Unknown modes render as "Unknown
methodology" so the absence is visible.

## Usage

``` r
methodology_label(mode)
```

## Arguments

- mode:

  Character.

## Value

Character label.
