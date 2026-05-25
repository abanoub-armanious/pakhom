# Slug an emergent code name into a safe codebook key

Lowercase + underscore-separated + alphanumeric-only. Truncated to 40
chars to keep the key readable in audit logs. Duplicate names produce
duplicate keys (intentional – the caller groups by key).

## Usage

``` r
.slug_emergent_code_name(name)
```
