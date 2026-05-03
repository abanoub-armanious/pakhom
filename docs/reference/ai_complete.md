# High-level AI completion with retry and error handling

High-level AI completion with retry and error handling

## Usage

``` r
ai_complete(
  provider,
  prompt,
  system_prompt = NULL,
  task = "coding",
  model = NULL,
  temperature = NULL,
  max_tokens = NULL,
  json_mode = FALSE,
  max_retries = 3
)
```

## Arguments

- provider:

  AIProvider object

- prompt:

  User prompt text

- system_prompt:

  Optional system prompt

- task:

  Task name for looking up max_tokens/temperature defaults

- model:

  Model override (NULL uses models\$primary)

- temperature:

  Temperature override

- max_tokens:

  Max tokens override

- json_mode:

  Logical: request JSON response format

- max_retries:

  Number of retry attempts on failure

## Value

Character string of response content
