# Walk the codebook for high-frequency codes due for description refresh

Phase 58 Tier 2 C-5: every `refresh_interval` new codes admitted to the
codebook, scan for codes with `frequency >= min_freq` that haven't been
refreshed in this cycle, sample `sample_segments` of their
coded_segments, and ask the AI to refresh their description. Updates the
codebook in place and stamps `last_description_refresh_at` on each
refreshed code.

## Usage

``` r
.maybe_refresh_high_freq_descriptions(
  state,
  provider,
  audit_log = NULL,
  response_cache = NULL,
  refresh_interval = 100L,
  min_freq = 50L,
  sample_segments = 5L,
  methodology_override = NULL
)
```
