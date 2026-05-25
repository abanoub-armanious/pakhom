# Bundle a run's transparency artifacts into a single report

Generates a self-contained HTML methodological-transparency report for a
pakhom run, plus a machine-readable JSON companion. Per AC4 (methodology
stamped on every output), the report is mode-stamped at the top; per the
OS.6 spec (anti-Jowsey compliance), it maps every pipeline step to a
Lincoln & Guba (1985) credibility / dependability / confirmability /
transferability checkpoint citing the exact decisions logged.

## Usage

``` r
bundle_transparency_report(run_dir, output_path = NULL)
```

## Arguments

- run_dir:

  Path to the run output directory (the directory containing
  run_metadata.json + ai_decisions.jsonl etc.).

- output_path:

  Optional path for the HTML output. Defaults to
  `file.path(run_dir, "transparency_report.html")`. The JSON companion
  is written alongside (same basename, .json extension).

## Value

Invisible list with `html_path`, `json_path`, and the parsed
`report_data` (the machine-readable contents).

## Details

The bundler reads ONLY from disk artifacts produced by a completed (or
in-progress) pakhom run – it never re-executes the pipeline, never calls
an AI provider, and is safe to invoke any number of times. Missing
artifacts degrade gracefully (the corresponding section renders an
"unavailable" notice rather than crashing).

## References

Lincoln, Y. S. & Guba, E. G. (1985). Naturalistic inquiry. Sage
Publications. Olmos-Vega, F. M. et al. (2023). A practical guide to
reflexivity in qualitative research: AMEE Guide No. 149. Jowsey et al.
(2025). PLOS One doi:10.1371/journal.pone.0330217.
