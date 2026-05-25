# Schema for the per-batch sentiment response (analyze_sentiment)


      {
        "results": [
          {
            "id": integer,                      // matches the batch entry index
            "sentiment_score": number,          // -1 to 1
            "confidence": number,               // 0 to 1
            "emotions": [string],               // multi-label, ordered strongest first
            "emotion_intensity": number         // 0 to 1
          }, ...
        ]
      }

## Usage

``` r
.sentiment_schema(
  emotion_categories = c("joy", "sadness", "anger", "fear", "surprise", "disgust",
    "trust", "anticipation", "neutral")
)
```

## Arguments

- emotion_categories:

  Character vector of allowed emotion labels. Defaults to the eight
  Plutchik primaries plus "neutral". Pass
  `config$analysis$sentiment$emotion_categories` when you want the
  schema's enum to match the prompt's enum.
