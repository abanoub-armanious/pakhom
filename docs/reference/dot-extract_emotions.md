# Extract multi-label emotions from AI response

Handles both the new "emotions" array format and legacy
"primary_emotion" single string. Returns a semicolon-separated
all_emotions string.

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
