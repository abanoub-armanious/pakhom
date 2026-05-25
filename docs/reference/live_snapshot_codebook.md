# Snapshot the current codebook to `codebook_live.json`

Atomic rewrite: writes to a temp file in the same directory, then
`file.rename`s over the live file. A researcher `cat`-ing the file
always sees a coherent snapshot (no torn read).

## Usage

``` r
live_snapshot_codebook(
  tracker,
  codebook,
  entry_index = NA_integer_,
  force = FALSE
)
```

## Arguments

- tracker:

  A `LiveTracker` or NULL

- codebook:

  `coding_state$codebook` (named list of code records)

- entry_index:

  Optional integer; used in the snapshot timestamp

- force:

  Logical; bypass the every-N gate (used at end-of-coding)

## Value

The (possibly updated) tracker, invisibly.

## Details

Honors `tracker$codebook_snapshot_every`: writes only when the tracker's
`n_codebook_snapshots` would increment to a multiple of the cadence
(i.e., every-N-entries).

Safe to call with `tracker = NULL` (no-op).
