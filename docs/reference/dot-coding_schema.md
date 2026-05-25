# Schema for the per-entry coding response (.code_entry_progressive)

Returned shape:


      {
        "skipped": boolean,
        "skip_reason": string,                  // "" when not skipped
        "coded_segments": [
          {
            "text": string,                     // verbatim from entry
            "start_char": integer,
            "end_char": integer,
            "code": string,                     // existing code or "NEW: name"
            "code_description": string,         // required for NEW codes
            "code_type": "descriptive"|"emotional"|"process"|"in_vivo"
          }, ...
        ]
      }

## Usage

``` r
.coding_schema()
```

## Details

Strict-mode contract: when skipped = TRUE the model still returns
coded_segments (as \[\]) and skip_reason (as a non-empty string). When
skipped = FALSE, skip_reason is "".
