# Parse a Markdown-with-YAML-frontmatter string back into a Memo

Inverse of
[`memo_to_markdown`](https://abanoub-armanious.github.io/pakhom/reference/memo_to_markdown.md).
Handles the YAML frontmatter via
[`yaml::yaml.load`](https://yaml.r-lib.org/reference/yaml.load.html) and
treats everything after the closing `---` as the memo body. Returns the
Memo with its S3 class restored.

## Usage

``` r
markdown_to_memo(md_text)
```

## Arguments

- md_text:

  Character: full Markdown content.

## Value

A `Memo` object.
