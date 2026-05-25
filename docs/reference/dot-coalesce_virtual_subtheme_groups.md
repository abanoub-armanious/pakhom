# Combine adjacent NA-named subtheme groups into one virtual group

Virtual subthemes carry no children (they are flat by definition), so
the merge concatenates code_indices and preserves the empty children
list. Named groups pass through unchanged including their nested
children produced by the recursive walker.

## Usage

``` r
.coalesce_virtual_subtheme_groups(groups)
```
