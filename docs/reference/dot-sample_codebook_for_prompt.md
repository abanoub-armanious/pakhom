# Sample N codes from a codebook, weighted toward the most-frequent

Returns up to `n` code records (name + frequency) from `state$codebook`,
sorted by frequency descending. Used to give the AI a sense of what the
codebook contains without dumping the whole thing into the prompt for
large codebooks.

## Usage

``` r
.sample_codebook_for_prompt(codebook, n = 30L)
```
