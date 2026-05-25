# Auto-scaled cadence for the AI saturation arbiter

Returns the number of coded entries between successive AI saturation
checks. The formula `max(20L, ceiling(n_corpus / 50))` produces ~50
checks regardless of corpus size, scaled so small corpora aren't
over-polled and large corpora aren't under-polled.

## Usage

``` r
.saturation_cadence(n_corpus)
```

## Arguments

- n_corpus:

  Integer; total entries in the corpus (after row filtering, before any
  are skipped).

## Value

Integer cadence (\>= 20L).

## Details

Examples:

- n_corpus = 100 -\> cadence = 20 (~5 checks)

- n_corpus = 1000 -\> cadence = 20 (~50 checks; cadence floor)

- n_corpus = 9178 -\> cadence = 184 (~50 checks)

- n_corpus = 50000-\> cadence = 1000 (~50 checks)

Floor of 20 prevents over-polling tiny corpora (where the floor produces
5 checks rather than 50).
