# Select appropriate correlation method for a variable pair

Select appropriate correlation method for a variable pair

## Usage

``` r
.select_pair_method(x, y, type_x, type_y)
```

## Arguments

- x:

  Numeric vector

- y:

  Numeric vector

- type_x:

  Variable type ("binary", "ordinal", "continuous")

- type_y:

  Variable type ("binary", "ordinal", "continuous")

## Value

Character: "pearson" or "spearman"
