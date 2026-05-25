# Decide whether to use the Anthropic Citations API path for this provider

Returns TRUE for Anthropic providers; FALSE otherwise. Future Sprint-4
phases may add a config opt-out
(`config$data_integrity$use_citations_api`), but the
default-on-for-Anthropic stance is load-bearing – the Citations API is
the package's primary anti-fabrication PREVENTION layer (T0.1 part 3b)
and disabling it weakens the architectural commitment to AC1 (AI is
scaffold by architecture, not by configuration).

## Usage

``` r
.use_citations_for_provider(provider, config)
```
