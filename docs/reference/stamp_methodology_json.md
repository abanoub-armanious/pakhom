# Stamp a JSON file with a methodology envelope

JSON files cannot accept comment-style headers (the format has no
comment syntax), so the stamp is added as a top-level
`_methodology_stamp` key on an envelope object that wraps the original
payload as `_payload`. Idempotent: re-stamping a file that already has a
`_methodology_stamp` envelope no-ops.

## Usage

``` r
stamp_methodology_json(json_path, mode, run_id = NULL)
```

## Arguments

- json_path:

  Path to a JSON file (will be re-written with the stamp).

- mode:

  Character methodology mode.

- run_id:

  Optional character run identifier.

## Value

Invisibly returns `json_path`.

## Details

Output shape:


    {
      "_methodology_stamp": {
        "mode": "reflexive_scaffold",
        "label": "M1 - Reflexive Scaffold",
        "run_id": "run_2026-...",
        "stamped_at": "2026-..."
      },
      "_payload": <original JSON object>
    }

Consumers reading the original payload should look at
`json[["_payload"]]` when the envelope is present, falling back to the
document root otherwise. Per AC4 every output gets a stamp; per AC1 the
consumer's parser is the one place the envelope is acknowledged.
