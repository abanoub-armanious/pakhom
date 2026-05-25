# Build a Mode N run-directory suffix for a fresh run

Standard pakhom run dirs are timestamped (`run_2026-05-03_103415`). T1.7
appends the mode short-code so the directory name itself carries the
methodology stamp: `run_2026-05-03_103415_M1`.

## Usage

``` r
run_id_with_mode(base_run_id, mode)
```

## Arguments

- base_run_id:

  Character, e.g. `"run_2026-05-03_103415"`.

- mode:

  Character methodology mode.

## Value

Character run_id with mode suffix.
