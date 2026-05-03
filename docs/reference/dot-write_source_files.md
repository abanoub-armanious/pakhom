# Write plain-text source files into the sources/ directory

Write plain-text source files into the sources/ directory

## Usage

``` r
.write_source_files(data, sources_dir)
```

## Arguments

- data:

  Tibble with std_id, std_text columns

- sources_dir:

  Path to sources/ directory inside the staging area

## Value

Named character vector: entry_id -\> file path (relative to archive
root)
