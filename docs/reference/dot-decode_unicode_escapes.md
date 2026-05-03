# Decode R-style Unicode escape sequences like \<U+2019\>

When text is read from databases or files with encoding issues, Unicode
characters may appear as literal `<U+XXXX>` strings instead of the
actual characters.

## Usage

``` r
.decode_unicode_escapes(text)
```

## Arguments

- text:

  Character vector

## Value

Character vector with Unicode escapes decoded
