# Archive a Mode 3 framework spec into the run output directory

Phase 32 (audit H1 + H2): a Mode 3 run loads
`config$methodology$framework_spec_path` into a typed `FrameworkSpec`
but never copies the source spec into the run outputs. The HTML report's
methodology stamp says "M3 - Framework Applied" but a reviewer cannot
reconstruct WHICH framework was used (TPB? COM-B? TDF? a custom YAML?),
what its citations are, or what its anomaly handling policy says.
Without the archive, replay / methodology-paper provenance is broken.

## Usage

``` r
archive_framework_spec(spec, run_dir, run_id = NULL)
```

## Arguments

- spec:

  A `FrameworkSpec` object (must carry a non-NA `source_path`). For
  built-in frameworks the source_path is the
  [`system.file()`](https://rdrr.io/r/base/system.file.html) resolution
  at load time.

- run_dir:

  Path to the run output directory. Created if missing.

- run_id:

  Optional character: run id used for the AC4 methodology stamp
  prepended to the archive (YAML/JSON comment). Phase 37 audit added the
  stamp; `run_id = NULL` omits the `| run: <id>` portion of the stamp.

## Value

Named list with `path` (path of the archived file under run_dir), `hash`
(sha256 hex string of the ORIGINAL source spec – not the post-stamp
archive bytes – so replay-equivalence is anchored to the source spec the
user supplied), `name` (framework\$name), `epistemic_stance`,
`anomaly_handling`, `n_constructs`, `schema_version`, suitable to splat
into `init_run_state(...)`.

## Details

This helper writes a verbatim copy of the source spec to
`outputs/<run>/framework_applied.yaml` (or .json – preserved from source
extension), computes a deterministic SHA-256 of the file's bytes, and
returns a metadata list suitable for stamping into `run_metadata.json`
via `init_run_state(...)`.

Per AC4 ("methodology stamped on every output"), the archive is
mandatory for any Mode 3 run – absence of the archive is a coverage
failure flagged by `verify_run_integrity`.

## See also

[`load_framework_spec`](https://abanoub-armanious.github.io/pakhom/reference/load_framework_spec.md);
[`init_run_state`](https://abanoub-armanious.github.io/pakhom/reference/init_run_state.md)
(consumes the metadata).

## Examples

``` r
spec <- load_framework_spec("tpb")
tmp <- tempfile()
dir.create(tmp)
arch <- archive_framework_spec(spec, tmp)
arch$relative_path  # "framework_applied.yaml"
nchar(arch$hash) == 64L  # TRUE -- sha256 hex string
file.exists(arch$path)   # TRUE
```
