# Persist all memos in a ResearcherReflectionLog to disk

Writes one `outputs/<run>/memos/<memo_id>.md` per memo with full YAML
frontmatter. Idempotent: re-calling re-writes existing files (memo
content is immutable so re-writes produce byte-equivalent output –
useful for replay-equivalence checks).

## Usage

``` r
persist_memos(
  log,
  run_dir,
  methodology_mode = "reflexive_scaffold",
  run_id = NULL
)
```

## Arguments

- log:

  A `ResearcherReflectionLog`.

- run_dir:

  Path to the run output directory.

- methodology_mode:

  Optional character: methodology mode to stamp into each memo's YAML
  frontmatter (per AC4). Defaults to `"reflexive_scaffold"` since memos
  are a Mode 1 construct. Pass NULL to omit.

- run_id:

  Optional character: run id to stamp alongside the mode. Defaults to
  `basename(run_dir)` so a typical `run_mode1` call writes the right id
  automatically.

## Value

Invisibly: a character vector of the written file paths.

## Details

Per AC4 ("methodology stamped on every output"), the memos directory is
a canonical Mode 1 artifact – the integrity check expects `memos/` to
exist when any memos have been authored, and the Mode 1 report renders a
"Researcher Reflexive Memos" section driven by the on-disk files.

**Overwrite policy**: if a memo's `.md` file has been edited externally
(e.g., manually fixed up in a text editor), calling `persist_memos`
again will overwrite that file *without warning*. The in-memory `Memo`
is the authoritative version. To preserve external edits, load the memos
back via
[`load_memos`](https://abanoub-armanious.github.io/pakhom/reference/load_memos.md)
before calling `persist_memos` – the load round-trip pulls any external
edits into the in-memory log first.
