# Serialize a Memo to a Markdown string with YAML frontmatter

Per SPRINT4_DESIGN.md M1.3 (line 280-291). The frontmatter carries the
schema fields; the body follows after the closing `---`. YAML is written
with explicit quoting on fields that may contain special characters so
the round-trip is lossless even with apostrophes, colons, etc.

## Usage

``` r
memo_to_markdown(memo, methodology_mode = "reflexive_scaffold", run_id = NULL)
```

## Arguments

- memo:

  A `Memo` object.

- methodology_mode:

  Optional character: methodology mode to stamp into the frontmatter as
  `methodology_mode`. Per AC4 (methodology stamped on every output),
  Mode 1 memos persisted to disk should carry the mode declaration so a
  memo lifted out of its run directory still self-identifies. Defaults
  to `"reflexive_scaffold"` (Mode 1) since memos are a Mode 1 construct;
  pass NULL to omit the field.

- run_id:

  Optional character: run id to stamp into the frontmatter alongside
  methodology_mode. NULL omits the field.

## Value

Character: the full Markdown content (frontmatter + body).
