# ==============================================================================
# Structured Output Schemas (Sprint-4 T1.2)
# ==============================================================================
# JSON Schemas for the six AI tasks that previously relied on prompt-based
# JSON-mode coercion. With T1.2 the providers enforce these schemas
# server-side: OpenAI via response_format = list(type = "json_schema",
# strict = TRUE, ...), Anthropic via a forced tool-use call whose
# input_schema is the same R list.
#
# Why server-side schemas matter:
# 1. Eliminates parse failures (the legacy parse_json_safely fallback in
#    R/03_json_utils.R was kept on the hot path because GPT-4o would
#    occasionally emit JSON with trailing commas, prose preambles, or
#    truncated objects). A 3% parse-failure rate on a 1000-call run is 30
#    silently-dropped decisions; structured outputs reduce this to 0.
# 2. Eliminates schema-drift bugs (the legacy code accepted whatever shape
#    the model returned, so a model-update could silently change the shape
#    and the caller would crash on missing fields downstream).
# 3. Makes auditing tractable (raw_responses cached under T1.4 can be
#    schema-validated at replay time; pre-T1.2 logs predate strict
#    schemas and can't be).
#
# Design constraints these schemas obey (driven by OpenAI strict mode --
# which is the stricter of the two providers; Anthropic tool-use accepts
# everything OpenAI strict accepts):
# - Every object has additionalProperties = FALSE.
# - Every property listed in `properties` is also in `required`. Optional
#   fields use nullable types via list("string", "null") rather than being
#   omitted from required.
# - No `oneOf`, `anyOf`, `not`, `pattern`, `format`, `minLength`, etc.
# - Enums are declared as list() (not c()) so jsonlite::toJSON with
#   auto_unbox = TRUE doesn't collapse single-element enums to scalars.
# - Required arrays use list() for the same reason.
#
# Each schema is a function (not a constant) so call-site code can
# customize emotion enums, code_type taxonomies, etc., as the package
# evolves without breaking back-compat with cached schemas in older runs.
# ==============================================================================

#' Schema for the per-entry coding response (.code_entry_progressive)
#'
#' Returned shape:
#' \preformatted{
#'   {
#'     "skipped": boolean,
#'     "skip_reason": string,                  // "" when not skipped
#'     "coded_segments": [
#'       {
#'         "text": string,                     // verbatim from entry
#'         "start_char": integer,
#'         "end_char": integer,
#'         "code": string,                     // existing code or "NEW: name"
#'         "code_description": string,         // required for NEW codes
#'         "code_type": "descriptive"|"emotional"|"process"|"in_vivo"
#'       }, ...
#'     ]
#'   }
#' }
#'
#' Strict-mode contract: when skipped = TRUE the model still returns
#' coded_segments (as []) and skip_reason (as a non-empty string).
#' When skipped = FALSE, skip_reason is "".
#' @keywords internal
.coding_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("skipped", "skip_reason", "coded_segments"),
    properties = list(
      skipped     = list(type = "boolean"),
      skip_reason = list(type = "string"),
      coded_segments = list(
        type  = "array",
        items = list(
          type                  = "object",
          additionalProperties  = FALSE,
          required = list("text", "start_char", "end_char",
                          "code", "code_description", "code_type"),
          properties = list(
            text             = list(type = "string"),
            start_char       = list(type = "integer"),
            end_char         = list(type = "integer"),
            code             = list(type = "string"),
            code_description = list(type = "string"),
            code_type        = list(
              type = "string",
              enum = list("descriptive", "emotional", "process", "in_vivo")
            )
          )
        )
      )
    )
  )
}

#' Schema for the per-entry coding response in Mode 3 (Framework Applied)
#'
#' Mirrors \code{.coding_schema()} but constrains \code{construct_id} to
#' the framework's allowed construct ids plus the literal "anomaly" --
#' the model can label a segment with a construct from the framework OR
#' flag it as resisting the framework, but cannot invent new constructs.
#' Per AC2 and AC8, Mode 3 is a CONFIGURATION of the same schema-based
#' coding architecture, not a separate code path: same shape, different
#' enum + an \code{anomaly_reason} field for non-fitting segments.
#'
#' Returned shape:
#' \preformatted{
#'   {
#'     "skipped": boolean,
#'     "skip_reason": string,
#'     "coded_segments": [
#'       {
#'         "text": string,
#'         "start_char": integer,
#'         "end_char": integer,
#'         "construct_id": string (one of framework constructs OR "anomaly"),
#'         "anomaly_reason": string (non-empty when construct_id="anomaly"; "" otherwise)
#'       }, ...
#'     ]
#'   }
#' }
#'
#' @param construct_ids Character vector of allowed construct ids from
#'   the loaded \code{FrameworkSpec}. The schema enforces that
#'   \code{construct_id} is one of these or the literal "anomaly".
#' @keywords internal
.coding_schema_framework <- function(construct_ids) {
  stopifnot(is.character(construct_ids), length(construct_ids) > 0L)
  enum <- as.list(c(construct_ids, "anomaly"))
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("skipped", "skip_reason", "coded_segments"),
    properties = list(
      skipped     = list(type = "boolean"),
      skip_reason = list(type = "string"),
      coded_segments = list(
        type  = "array",
        items = list(
          type                  = "object",
          additionalProperties  = FALSE,
          required = list("text", "start_char", "end_char",
                          "construct_id", "anomaly_reason"),
          properties = list(
            text           = list(type = "string"),
            start_char     = list(type = "integer"),
            end_char       = list(type = "integer"),
            construct_id   = list(type = "string", enum = enum),
            anomaly_reason = list(type = "string")
          )
        )
      )
    )
  )
}

#' Schema for the AI saturation-check response (.ai_saturation_check)
#'
#' \preformatted{
#'   { "novel_patterns_remaining": boolean, "reasoning": string }
#' }
#' @keywords internal
.saturation_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("novel_patterns_remaining", "reasoning"),
    properties = list(
      novel_patterns_remaining = list(type = "boolean"),
      reasoning                = list(type = "string")
    )
  )
}

#' Schema for the per-batch sentiment response (analyze_sentiment)
#'
#' \preformatted{
#'   {
#'     "results": [
#'       {
#'         "id": integer,                      // matches the batch entry index
#'         "sentiment_score": number,          // -1 to 1
#'         "confidence": number,               // 0 to 1
#'         "emotions": [string],               // multi-label, ordered strongest first
#'         "emotion_intensity": number         // 0 to 1
#'       }, ...
#'     ]
#'   }
#' }
#'
#' @param emotion_categories Character vector of allowed emotion labels.
#'   Defaults to the eight Plutchik primaries plus "neutral". Pass
#'   \code{config$analysis$sentiment$emotion_categories} when you want the
#'   schema's enum to match the prompt's enum.
#' @keywords internal
.sentiment_schema <- function(emotion_categories = c("joy", "sadness", "anger",
                                                      "fear", "surprise",
                                                      "disgust", "trust",
                                                      "anticipation", "neutral")) {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("results"),
    properties = list(
      results = list(
        type  = "array",
        items = list(
          type                  = "object",
          additionalProperties  = FALSE,
          required = list("id", "sentiment_score", "confidence",
                          "emotions", "emotion_intensity"),
          properties = list(
            id               = list(type = "integer"),
            sentiment_score  = list(type = "number"),
            confidence       = list(type = "number"),
            emotions = list(
              type  = "array",
              items = list(
                type = "string",
                enum = as.list(unique(emotion_categories))
              )
            ),
            emotion_intensity = list(type = "number")
          )
        )
      )
    )
  )
}

#' Schema for the per-item theming merge-decision response (.run_merge_pass)
#'
#' \preformatted{
#'   {
#'     "action": "merge"|"standalone",
#'     "merge_into": integer|null,             // 1-based cluster index when merging
#'     "updated_label": string|null,
#'     "updated_description": string|null,
#'     "rationale": string
#'   }
#' }
#'
#' merge_into / updated_label / updated_description are nullable because
#' they're meaningless when action = "standalone". Strict mode requires
#' them in the schema; nullable lets the model emit null cleanly.
#' @keywords internal
.theming_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("action", "merge_into", "updated_label",
                                  "updated_description", "rationale"),
    properties = list(
      action = list(
        type = "string",
        enum = list("merge", "standalone")
      ),
      merge_into          = list(type = list("integer", "null")),
      updated_label       = list(type = list("string", "null")),
      updated_description = list(type = list("string", "null")),
      rationale           = list(type = "string")
    )
  )
}

#' Schema for the insight-generation response (generate_insights)
#'
#' \preformatted{
#'   {
#'     "key_findings": [{ "insight": string, "explanation": string }],
#'     "theoretical_implications": string,
#'     "practical_implications": string,
#'     "limitations": [string],
#'     "future_directions": [string]
#'   }
#' }
#' @keywords internal
.insight_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("key_findings", "theoretical_implications",
                                  "practical_implications", "limitations",
                                  "future_directions"),
    properties = list(
      key_findings = list(
        type  = "array",
        items = list(
          type                 = "object",
          additionalProperties = FALSE,
          required             = list("insight", "explanation"),
          properties = list(
            insight     = list(type = "string"),
            explanation = list(type = "string")
          )
        )
      ),
      theoretical_implications = list(type = "string"),
      practical_implications   = list(type = "string"),
      limitations              = list(type = "array",
                                       items = list(type = "string")),
      future_directions        = list(type = "array",
                                       items = list(type = "string"))
    )
  )
}

#' Schema for the executive-summary synthesis response (generate_ai_synthesis)
#'
#' \preformatted{
#'   { "executive_summary": string, "conclusion": string }
#' }
#' @keywords internal
.synthesis_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("executive_summary", "conclusion"),
    properties = list(
      executive_summary = list(type = "string"),
      conclusion        = list(type = "string")
    )
  )
}

# ==============================================================================
# Schema validation helper (lightweight; used in tests + sanity checks)
# ==============================================================================

#' Lightweight check that a list looks like a valid JSON Schema for our use
#'
#' Not a full JSON Schema validator -- just enough to catch the OpenAI
#' strict-mode pitfalls at package-load / test time so a malformed schema
#' is caught before it hits the API. Specifically:
#' \itemize{
#'   \item Top-level must be an object schema with type = "object".
#'   \item Every object must have additionalProperties = FALSE.
#'   \item Every object's `required` must list every key in `properties`.
#'   \item Required and enum arrays must be lists (not character vectors)
#'     to avoid jsonlite auto_unbox collapsing single-element arrays.
#' }
#'
#' @param schema A schema list (e.g., the output of \code{.coding_schema()}).
#' @param path Internal: schema path, used for error messages.
#' @return TRUE invisibly if the schema is well-formed; otherwise stops
#'   with a descriptive error pointing at the violating subschema.
#' @keywords internal
.validate_schema <- function(schema, path = "$") {
  if (!is.list(schema)) {
    stop(sprintf("Schema at %s must be a list", path), call. = FALSE)
  }

  # Type can be a string ("object", "string", ...) or a list of strings
  # for nullable types (e.g., list("integer", "null")). Check both shapes.
  type <- schema$type
  if (is.list(type)) {
    if (!all(vapply(type, is.character, logical(1)))) {
      stop(sprintf("Schema at %s: type list must contain only strings", path),
           call. = FALSE)
    }
  }

  is_object <- identical(type, "object") ||
               (is.list(type) && "object" %in% unlist(type))

  if (is_object) {
    if (!isFALSE(schema$additionalProperties)) {
      stop(sprintf("Schema at %s: additionalProperties must be FALSE for OpenAI strict mode", path),
           call. = FALSE)
    }
    if (is.null(schema$properties) || !is.list(schema$properties)) {
      stop(sprintf("Schema at %s: object schemas must declare properties", path),
           call. = FALSE)
    }
    if (!is.list(schema$required) ||
        any(vapply(schema$required, function(r) !is.character(r), logical(1)))) {
      stop(sprintf("Schema at %s: required must be a list() of property name strings (not c())", path),
           call. = FALSE)
    }
    prop_names <- names(schema$properties)
    req_names  <- vapply(schema$required, identity, character(1))
    missing_in_required <- setdiff(prop_names, req_names)
    if (length(missing_in_required) > 0) {
      stop(sprintf("Schema at %s: properties not in required (OpenAI strict mode forbids optional fields): %s",
                   path, paste(missing_in_required, collapse = ", ")),
           call. = FALSE)
    }
    extra_in_required <- setdiff(req_names, prop_names)
    if (length(extra_in_required) > 0) {
      stop(sprintf("Schema at %s: required references unknown properties: %s",
                   path, paste(extra_in_required, collapse = ", ")),
           call. = FALSE)
    }

    # Recurse into each property
    for (nm in prop_names) {
      .validate_schema(schema$properties[[nm]],
                       path = sprintf("%s.properties.%s", path, nm))
    }
  }

  # Recurse into array items
  if (identical(type, "array") && !is.null(schema$items)) {
    .validate_schema(schema$items, path = sprintf("%s.items", path))
  }

  # Recurse into enum: ensure it's a list (not c()) so single-element enums
  # don't collapse under jsonlite auto_unbox.
  if (!is.null(schema$enum)) {
    if (!is.list(schema$enum)) {
      stop(sprintf("Schema at %s: enum must be a list() (not c()) so single-element enums survive jsonlite auto_unbox", path),
           call. = FALSE)
    }
  }

  invisible(TRUE)
}
