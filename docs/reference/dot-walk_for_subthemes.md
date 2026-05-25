# Walk a theme's subtree to identify subthemes (recursive, depth-N)

For each immediate child of the theme node, the AI judges whether it
constitutes a coherent subtheme of the theme. If yes, the child's codes
form a Subtheme. If no, the child's codes are flattened directly into
the theme (no Subtheme; will be wrapped in a virtual NA-named Subtheme
by create_subtheme()/create_theme_set()).

## Usage

``` r
.walk_for_subthemes(
  theme_name,
  theme_node_idx,
  hac,
  codes,
  distance_matrix,
  co_occurrence,
  walk_ctx,
  current_depth = 1L,
  max_subtheme_depth = 3L,
  max_codes_per_subtheme = 25L
)
```

## Arguments

- theme_name:

  Parent theme name (passed to .evaluate_cluster as parent_label so the
  AI prompt knows the enclosing context).

- theme_node_idx:

  HAC merge-matrix row index for the theme.

- hac:

  Hierarchical clustering object (stats::hclust output).

- codes:

  List of Code records keyed by leaf index.

- distance_matrix:

  Pairwise distance matrix between codes.

- co_occurrence:

  Optional co-occurrence matrix.

- walk_ctx:

  List bundling walk_state + provider + prompts.

- current_depth:

  Depth of this recursive call (1 = direct subthemes of the theme; 2 =
  sub-subthemes; ...).

- max_subtheme_depth:

  Maximum recursion depth. Once `current_depth` reaches this value,
  large subthemes stop being re-walked. Default 3.

- max_codes_per_subtheme:

  Size threshold that triggers recursion. A coherent subtheme with more
  leaves than this gets re-walked one level deeper. Default 25.

## Details

Phase 58 Tier 1 C-12 + AF-8: when a coherent subtheme has more than
`max_codes_per_subtheme` codes AND we have depth budget left
(`current_depth < max_subtheme_depth`), the function recurses into that
subtheme's subtree to identify sub-subthemes. This is the multi-level
decomposition the Phase 57 audit found missing – the 237-code mega-theme
split as just 2 sub-buckets (32 + 205) is the canonical failure mode.

Phase 58 Tier 1 AF-4: when the HAC binary cut at this internal node is
imbalanced (one branch has ≤1 code) AND the cluster has \>3 codes total,
the function refuses to introduce subtheme structure at all – the codes
flow back into the parent under a single virtual subtheme. 1-code
subthemes paired with many-code siblings were the "55 imbalanced
binary-split themes" the audit flagged.

Returns a list of subtheme-group records. Each record has fields: name :
character or NA description : character code_indices : integer vector
children : list of (recursive) subtheme-group records, or list()
