# Schema for the insight-generation response (generate_insights)


      {
        "key_findings": [{ "insight": string, "explanation": string }],
        "theoretical_implications": string,
        "practical_implications": string,
        "limitations": [string],
        "future_directions": [string]
      }

## Usage

``` r
.insight_schema()
```
