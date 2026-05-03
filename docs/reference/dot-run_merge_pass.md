# Run a single sequential merge pass through all items

For each item (starting from the second), present the AI with the
current clusters and ask: "Should this item merge into an existing
cluster, or stand alone?" After processing all items, return the
resulting clusters and the number of merges that occurred.

## Usage

``` r
.run_merge_pass(
  items,
  pass_number,
  provider,
  research_focus,
  concept_str,
  calibration_text,
  reflexivity_block = "",
  co_occurrence = NULL,
  code_similarity = NULL,
  audit_log = NULL
)
```

## Arguments

- items:

  List of items (codes or previously-merged clusters)

- pass_number:

  Integer pass number (for logging)

- provider:

  AIProvider object

- research_focus:

  Research focus string

- concept_str:

  Concept string for the research focus

- calibration_text:

  Calibration text from previous studies

## Value

List with items, n_merges, ai_says_stop
