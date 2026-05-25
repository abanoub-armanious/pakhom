# Extract multi-label emotions from AI response

Handles the structured-outputs "emotions" array format (T1.2 schema
lock: .sentiment_schema requires `emotions` and forbids extra
properties, so a legacy `primary_emotion` field is architecturally
unreachable here). Returns a semicolon-separated all_emotions string.

## Usage

``` r
.extract_emotions(item, j, is_dataframe = FALSE)
```

## Arguments

- item:

  Data frame row or list item from parsed AI response

- j:

  Row index (only used when is_dataframe = TRUE)

- is_dataframe:

  Whether item is a data.frame (TRUE) or list (FALSE)

## Value

character scalar: semicolon-separated emotions string, or NA_character\_
