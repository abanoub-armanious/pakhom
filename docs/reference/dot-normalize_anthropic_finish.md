# Map Anthropic stop_reason to canonical finish_reason

Canonical values: "stop" (normal completion), "length" (truncated by
max_tokens), "tool_use" (model invoked a tool). Unknown values pass
through unchanged for forward compatibility with new stop_reasons.

## Usage

``` r
.normalize_anthropic_finish(stop_reason)
```

## Arguments

- stop_reason:

  Anthropic `stop_reason` value

## Value

Canonical finish_reason character
