# Load + validate a theoretical framework specification

Reads YAML or JSON, validates against the M3.1 schema, and returns a
typed `FrameworkSpec` S3 object. The schema requires:

- `framework$name` – non-empty character

- `framework$constructs` – non-empty list, each with `id` (unique),
  `name`, `description`, optional `example_indicators`

- `framework$epistemic_stance` – one of `"constructionist"`,
  `"positivist"`, `"mixed"`

- `framework$anomaly_handling` – one of `"extend"`, `"revise"`,
  `"bracket"`

Optional fields: `citations`, `code_definitions`.

## Usage

``` r
load_framework_spec(path)
```

## Arguments

- path:

  Path to a YAML or JSON file (extension determines parser). Special
  values: when `path` is one of the built-in framework names (e.g.,
  `"tpb"`, `"comb"`, `"tdf"`), loads from `inst/extdata/frameworks/`.

## Value

A `FrameworkSpec` S3 object.

## Details

Construct `id` values must be unique within a framework (the coding
pipeline keys constructs by id). Validation errors point at the specific
construct that failed so users with a malformed spec get an actionable
message.

## See also

[`list_builtin_frameworks`](https://abanoub-armanious.github.io/pakhom/reference/list_builtin_frameworks.md);
[`archive_framework_spec`](https://abanoub-armanious.github.io/pakhom/reference/archive_framework_spec.md)
(writes a verbatim copy + sha256 to a Mode 3 run dir);
[`framework_prompt_block`](https://abanoub-armanious.github.io/pakhom/reference/framework_prompt_block.md)
(formats the spec for AI system prompts).

## Examples

``` r
# Load a built-in framework by alias
tpb <- load_framework_spec("tpb")
print(tpb)

list_builtin_frameworks()
# [1] "tpb"  "comb" "tdf"

if (FALSE) { # \dontrun{
# Load a custom spec from disk (YAML or JSON)
my_framework <- load_framework_spec("path/to/my_framework.yaml")
} # }
```
