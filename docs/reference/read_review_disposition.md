# Read review disposition from theme review directory

After theme review, the researcher can set disposition to
"revise_codebook" to loop back and revise the codebook before re-running
theme generation.

## Usage

``` r
read_review_disposition(output_dir)
```

## Arguments

- output_dir:

  Pipeline output directory

## Value

Character: "continue" (default) or "revise_codebook"
