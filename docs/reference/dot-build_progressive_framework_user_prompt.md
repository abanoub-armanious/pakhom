# Build the Mode 3 (framework-applied) user prompt

The framework's constructs are listed in the system prompt (via
framework_prompt_block). The user prompt presents the entry text and
instructs the AI to apply the framework constructs verbatim, flagging
segments that resist the framework as `construct_id = "anomaly"`.

## Usage

``` r
.build_progressive_framework_user_prompt(truncated_text, framework_spec)
```
