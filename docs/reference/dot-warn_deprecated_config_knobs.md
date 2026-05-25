# Warn about deprecated config knobs (Phase 58 Tier 3 AH-5)

Walks the user's config for keys that the package no longer reads. The
full list (15 knobs total) spans three cleanup waves: Phase 52/53
sequential-merge removal (4 knobs), Phase 53 theme- count cleanup (5
knobs), Phase 56 saturation-arbiter migration (6 knobs). Each surviving
stale knob in a user's personal config.yaml is logged as a warn with the
cleanup phase that removed it.

## Usage

``` r
.warn_deprecated_config_knobs(config)
```
