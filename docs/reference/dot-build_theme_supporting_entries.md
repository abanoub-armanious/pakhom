# Build a list of supporting-entry summaries to pass to the AI

Used by category functions that need to show the model "here are the
entries the researcher believes support theme X". Returns a compact
one-line-per-entry string suitable for prompt injection.

## Usage

``` r
.build_theme_supporting_entries(theme_entries, max_chars = 400)
```
