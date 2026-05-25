# Snapshot the current theme/cluster hierarchy to `code_to_cluster.json`

Called from
[`.walk_for_themes()`](https://abanoub-armanious.github.io/pakhom/reference/dot-walk_for_themes.md)
/
[`.walk_for_subthemes()`](https://abanoub-armanious.github.io/pakhom/reference/dot-walk_for_subthemes.md)
after each AI decision. Captures the in-progress theme structure so a
researcher can watch the HAC tree walk produce themes in real time.

## Usage

``` r
live_snapshot_clusters(
  tracker,
  walk_status,
  walk_state = NULL,
  themes_so_far = list()
)
```

## Arguments

- tracker:

  A `LiveTracker` or NULL

- walk_status:

  One of `"in_progress"`, `"theme_walk_complete"`,
  `"subtheme_walk_complete"`

- walk_state:

  The walk_state environment (or list) carrying `n_calls`,
  `n_failed_calls`, `decisions`.

- themes_so_far:

  List of in-progress theme records (each with `name`, `description`,
  `code_indices`, `code_keys`). Optional; the snapshot just records
  empty themes when the walk is mid-flight.

## Value

The (possibly updated) tracker, invisibly.

## Details

Atomic rewrite. Safe to call with `tracker = NULL` (no-op).
