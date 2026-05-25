# Format the recent saturation-curve trajectory as compact prompt text

Picks the last `n_recent` rows of the saturation curve (if available)
and renders each as one line:
`"entries_coded=300, n_codes=42, new_in_window=3, reuse_density=0.140"`.
The reuse_density is 1 - slope_ratio (i.e., fraction of assignments
going to EXISTING codes); it's the inverse of the ITS ratio so the AI
sees high reuse_density -\> high saturation candidate.

## Usage

``` r
.format_saturation_curve_for_prompt(curve, n_recent = 6L)
```
