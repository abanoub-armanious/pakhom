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
  min_cooccurrence = 3
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

## Value

Invisible adjacency matrix, or NULL if igraph unavailable
