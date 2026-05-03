# Initialize checkpoint system for a pipeline run

Initialize checkpoint system for a pipeline run

## Usage

``` r
init_checkpoints(output_dir, config_hash = NULL, ...)
```

## Arguments

- output_dir:

  Base output directory for this run

- config_hash:

  Hash of config for detecting changes between runs

- ...:

  Reserved for future arguments; currently ignored. Accepted so callers
  passing extra named arguments don't error out.

## Value

CheckpointManager S3 object
