# Check whether a snapshot's schema version is compatible with the current package

Compatibility is defined as identical major-version. Minor-version
increments are backward-compatible additions and remain comparable.

## Usage

``` r
.schema_is_compatible(snapshot_version, current_version = .SCHEMA_VERSION)
```

## Arguments

- snapshot_version:

  Character string like "1.0" or NULL

- current_version:

  Character string (defaults to .SCHEMA_VERSION)

## Value

Logical TRUE if compatible, FALSE otherwise
