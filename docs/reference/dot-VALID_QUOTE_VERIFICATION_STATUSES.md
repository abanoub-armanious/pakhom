# Valid verification statuses, ordered from most to least confident. Render policy: verified_exact / verified_fuzzy -\> render normally unverified -\> render with warning marker drifted -\> corpus-integrity warning at load time fabricated -\> never rendered; logged

Valid verification statuses, ordered from most to least confident.
Render policy: verified_exact / verified_fuzzy -\> render normally
unverified -\> render with warning marker drifted -\> corpus-integrity
warning at load time fabricated -\> never rendered; logged

## Usage

``` r
.VALID_QUOTE_VERIFICATION_STATUSES
```

## Format

An object of class `character` of length 5.
