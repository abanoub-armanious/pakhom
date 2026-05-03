# Parse NVivo QDPX project file with deep hierarchy and entry-level coding

QDPX files are ZIP archives containing a project.qde XML file with the
complete code structure, all coding references, and source texts. This
recursively extracts the full theme-\>subtheme-\>code hierarchy,
entry-level coding references (which text segments were coded), and
source document texts.

## Usage

``` r
.parse_qdpx_deep(path)
```

## Arguments

- path:

  Path to .qdpx file

## Value

List with: \$codebook (tibble), \$codebook_full (tibble),
\$coding_references (tibble), \$sources (tibble), \$hierarchy (nested
list)
