# Build the prompt block describing the framework's constructs

Formats the framework as a system-prompt section that the Mode 3 coding
pipeline injects so the AI knows which constructs are permitted code
names + how to apply them. The block is kept compact (one line per
construct + one line per indicator) so it doesn't dominate the context
window.

## Usage

``` r
framework_prompt_block(spec)
```

## Arguments

- spec:

  A FrameworkSpec object.

## Value

Character: the prompt block.
