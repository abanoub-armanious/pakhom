# Convert legacy subtheme representations into a list of Subtheme S3 objects

Handles the formats jsonlite emits:

- data.frame with name + description columns (simplifyVector = TRUE)

- list-of-lists with \$name + \$description

- plain character vector of subtheme names (no code mapping known)

- existing list of Subtheme S3 (passes through)

## Usage

``` r
.legacy_subthemes_to_objects(legacy_sts, all_code_names)
```

## Details

If no per-subtheme code mapping is present, returns an empty list and
the caller wraps all codes in a single virtual Subtheme.
