# Defensive code-name normalization

Phase 58 Tier 0 C-4: strips numbered-list prefixes (`321. `), `NEW:`
markers, and surrounding ASCII or Unicode smart quotes that the AI may
echo back from the codebook-summary prompt format. Applied once at code
admission so the codebook key is canonical regardless of which prefix
the AI emitted. Idempotent: two passes handle ordering variants like
`"1. NEW: Food"` and `"NEW: 1. Food"`.

## Usage

``` r
.normalize_code_name(name)
```

## Arguments

- name:

  Character scalar; the raw code name returned by the AI.

## Value

Cleaned code name with all known prefix/quote noise removed.
