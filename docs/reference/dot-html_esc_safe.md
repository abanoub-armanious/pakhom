# Tiny HTML escaper used by the stamping API

We have a bigger `.html_esc` elsewhere but this module is self-contained
– defining a safe local escaper avoids cross-file load-order coupling.

## Usage

``` r
.html_esc_safe(x)
```
