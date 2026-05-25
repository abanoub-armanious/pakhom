# Schema for batch inductive coding of Mode 3 anomaly segments (Phase 54)

Used by .inductive_code_anomaly_segments() to turn the segments that
didn't fit a framework into named inductive codes. The AI sees a batch
of anomaly segment texts and generates a code_name + code_description
per segment. Codes can naturally repeat across segments (the AI is
prompted to reuse code names for segments expressing the same concept);
Phase 52's HAC + AI tree walk then consolidates near-duplicates into
emergent themes.

## Usage

``` r
.emergent_coding_schema()
```

## Details

This schema is intentionally per-segment (rather than cross-referenced
codes -\> segments) so the structured output stays simple and the
provenance from segment back to its inductive code stays one-to-one.


      {
        "coded_segments": [
          { "segment_index": int, "code_name": string, "code_description": string }
        ]
      }
