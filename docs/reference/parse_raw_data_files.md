# Parse raw data DOCX files and extract metadata from filenames

Handles the specific naming convention: YYYY-MM-DD_YYYY-MM-DD_Username
XXX_Rating X.X_Likes XXX.docx

## Usage

``` r
parse_raw_data_files(raw_data_dir, max_files = NULL, seed = NULL)
```

## Arguments

- raw_data_dir:

  Path to raw data folder

- max_files:

  Maximum files to process (NULL = all)

- seed:

  Optional random seed for reproducible file sampling

## Value

tibble with: filename, text, username, rating, likes, date_scraped,
date_posted
