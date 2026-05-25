# Write methodology rules to a markdown file under `run_dir`

Creates `run_dir/rules/methodology_rules.md` with the generated rules.
The file is human-readable and serves as the canonical record of the
rules text the model was sent on every call during this run. Per AC9 +
AC4, rules are stamped on every call AND archived alongside the run
output so the methodology paper can attribute observed AI behavior to
the specific rules in force.

## Usage

``` r
write_methodology_rules(config, run_dir)
```

## Arguments

- config:

  A ThematicConfig (or list).

- run_dir:

  Path to the run output directory.

## Value

Path to the written file (invisibly), or NULL on failure.
