# Create theme co-occurrence network visualization

Builds a network graph where nodes are themes and edges represent
co-occurrence strength (entries assigned to both themes). Requires
multi-label assignment columns (`theme_membership_*`).

## Usage

``` r
create_theme_network(
  data,
  theme_set,
  output_path = "theme_network.png",
  min_cooccurrence = 3,
  methodology_mode = NULL,
  run_id = NULL,
  max_inline_themes = 30L
)
```

## Arguments

- data:

  Tibble with theme_membership\_\* columns

- theme_set:

  ThemeSet object

- output_path:

  File path for PNG output

- min_cooccurrence:

  Minimum co-occurrence count to draw an edge (default 3)

- methodology_mode:

  Optional character (T1.7 / AC4): when supplied, adds a footer caption
  identifying the mode + run.

- run_id:

  Optional character: run identifier.

- max_inline_themes:

  Integer; when the graph has more nodes than this after isolated-vertex
  removal, the network is filtered to the top-N by weighted degree (sum
  of edge weights). Default 30L.

## Value

Invisible adjacency matrix, or NULL if igraph unavailable

## Details

Phase 58 Tier 5 AH-9/V-1: at scale the unfiltered network was an
unreadable hairball (Phase 57 audit observed 417 themes plotted at once
with no legend). The `max_inline_themes` parameter caps the visible
network at the top-N most-connected themes (ranked by weighted degree)
and adds an inline legend explaining node size + edge width encoding.
