# Approximate word count of a character vector

Splits on runs of whitespace and counts the resulting tokens. Used by
[`compute_corpus_coverage`](https://abanoub-armanious.github.io/pakhom/reference/compute_corpus_coverage.md)
for the "words processed" figure on the coverage card. Approximate
(handles English-like text; degrades gracefully on punctuation-heavy
text).

## Usage

``` r
.count_words_safe(text)
```
