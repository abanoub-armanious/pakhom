# Write run metadata for a run directory

Writes `metadata` to `run_metadata.json` in JSON form. Atomic-ish:
writes to a temp file first, then renames. If the atomic-rename fails
(e.g., cross-filesystem move), falls back to a direct write – the
failure modes are not silent because both branches log on error.

## Usage

``` r
.write_run_metadata(run_dir, metadata)
```

## Arguments

- run_dir:

  Path to a run output directory (must exist).

- metadata:

  Named list to serialize.

## Value

TRUE on success, FALSE on failure (warning logged).
