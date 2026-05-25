# Normalize text for the verification ladder's fuzzy steps

Applies (in order): NFC unicode normalization (where stringi is
available), smart quote -\> ASCII quote conversion, unicode-aware
whitespace collapse, case-folding. This catches the most common
attribution drift patterns: model returns typographic quotes where
source has straight ASCII, model collapses or inserts whitespace
(including unicode NBSP / em-space / etc. that the default `\s` regex
misses), model lowercases.

## Usage

``` r
.normalize_quote_text(x)
```

## Details

Phase 58 Tier 7 M-24 + L-2: pre-Tier-7 this helper only did smart- quote
ASCII-fication + standard `\s` whitespace collapse. The Phase 57 audit
found 8 of 50 sampled verbatim spot-checks failed (16% miss rate) –
mostly because (a) source had typographic apostrophes that weren't
NFC-normalized to combine with the AI's ASCII rendering, and (b) source
had U+00A0 NBSP / U+2009 thin-space that R's PCRE `\s` doesn't match by
default. NFC normalization

- unicode-aware whitespace class `[\p{Z}\s]` together resolve both
  classes of false-positive in the fabrication log.
