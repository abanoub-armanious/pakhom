# Resolve methodology rules text from a config (helper for create_ai_provider)

Returns the rules string from `generate_methodology_rules(config)` when
`config` is a ThematicConfig (or a list with a methodology block).
Returns "" otherwise – the empty string is a no-op when prepended to a
system prompt, so legacy / test contexts continue to work without
changes.

## Usage

``` r
.resolve_methodology_rules(config)
```
