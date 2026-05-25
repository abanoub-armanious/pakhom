# Run batch sentiment analysis on all entries

Run batch sentiment analysis on all entries

## Usage

``` r
analyze_sentiment(
  data,
  provider,
  config = list(),
  checkpoint = NULL,
  research_focus = "",
  coding_state = NULL,
  audit_log = NULL,
  response_cache = NULL
)
```

## Arguments

- data:

  Standardized tibble with std_text column

- provider:

  AIProvider object

- config:

  Sentiment config section

- checkpoint:

  CheckpointManager (or NULL)

- research_focus:

  Research focus string

- coding_state:

  ProgressiveCodingState (or NULL). When provided, only processes
  entries in the analytic sample (those with codes) and includes
  assigned codes as context for more accurate sentiment scoring.

- audit_log:

  An AuditLog object (from `init_audit_log`) for recording each
  sentiment-assignment decision, or NULL to disable audit logging for
  this step.

- response_cache:

  An optional ResponseCache object (from
  [`init_response_cache`](https://abanoub-armanious.github.io/pakhom/reference/init_response_cache.md)).
  When provided, raw API responses for each per-batch sentiment
  ai_complete_fast() call are written to the cache and referenced from
  the audit log (T1.4). Pass `NULL` to skip raw-response capture.

## Value

tibble with sentiment_score, confidence, all_emotions
(semicolon-separated), emotion_intensity columns added
