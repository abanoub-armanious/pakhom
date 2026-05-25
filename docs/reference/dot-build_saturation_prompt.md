# Build the prompt for the AI saturation arbiter

Self-contained string. Includes:

- the research focus (so the AI judges saturation in context)

- the recent saturation-curve trajectory (new-codes-per-window + reuse
  density) – this is the EVIDENCE the pre-Phase-56 heuristic signals
  computed; now passed to the AI as data

- codebook composition summary (top-N codes by frequency)

- n_coded / n_corpus progress

## Usage

``` r
.build_saturation_prompt(state, research_focus, n_coded, n_corpus, n_done)
```
