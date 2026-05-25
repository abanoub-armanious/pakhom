# Construct a Provocation object

A Provocation is an extractive AI-generated question/observation that
challenges the researcher's framing of a theme. Each provocation carries
a `QuoteProvenance` object so the cited evidence is verifiable – per
AC7, no provocation may cite a fabricated quote.

## Usage

``` r
make_provocation(
  category,
  theme_name,
  reason,
  provenance,
  extra = list(),
  ai_model = NA_character_,
  ai_call_id = NA_character_
)
```

## Arguments

- category:

  One of `.VALID_PROVOCATION_CATEGORIES`.

- theme_name:

  Character: the theme this provocation challenges.

- reason:

  Character: one-line explanation of the provocation.

- provenance:

  A `QuoteProvenance` object: the cited evidence (entry_id, char range,
  exact_text, verification_status).

- extra:

  Optional named list of category-specific fields.

- ai_model:

  Optional character: model that produced the provocation.

- ai_call_id:

  Optional character: request_id of the AI call.

## Value

A `Provocation` S3 object.
