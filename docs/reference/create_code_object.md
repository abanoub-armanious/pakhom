# Create a Code S3 object

Atomic leaf in the theme hierarchy. Carries name, description, type,
frequency, entry_ids, and coded_segments inline so a saved ThemeSet is
self-contained: a researcher with just themes.json can verify every
quote without needing the original coding_state.

## Usage

``` r
create_code_object(
  key,
  name = NULL,
  description = "",
  type = "descriptive",
  frequency = 0L,
  entry_ids = character(0),
  coded_segments = list()
)
```

## Arguments

- key:

  Code key (codebook lookup key, e.g., "med_helps")

- name:

  Human-readable code name (e.g., "Medication helps binge control")

- description:

  Code description

- type:

  Code type (e.g., "descriptive", "framework_construct", "anomaly")

- frequency:

  How many entries are coded with this code

- entry_ids:

  Character vector of entry std_ids

- coded_segments:

  List of coded-segment records (each with entry_id, text, offsets,
  QuoteProvenance)

## Value

Code S3 object
