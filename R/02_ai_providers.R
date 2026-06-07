# ==============================================================================
# AI Provider Abstraction — OpenAI + Anthropic Unified Interface
# ==============================================================================
# No global state. Provider object is passed to every AI function.
# ==============================================================================

#' Create an AI provider client
#'
#' @param provider Character: "openai" or "anthropic"
#' @param config Full ThematicConfig, or the ai section of config
#' @return An AIProvider S3 object
#' @importFrom httr2 request req_headers req_body_json req_timeout req_options req_retry req_error req_perform resp_status resp_body_string resp_body_json
#' @export
create_ai_provider <- function(provider = "openai", config = NULL) {
  stopifnot(provider %in% c("openai", "anthropic"))

  # Extract provider-specific config
  if (inherits(config, "ThematicConfig")) {
    provider_config <- config$ai[[provider]]
    max_tokens <- config$ai$max_tokens
    temperature <- config$ai$temperature
  } else if (is.list(config)) {
    provider_config <- config[[provider]] %||% config
    max_tokens <- config$max_tokens %||% list()
    temperature <- config$temperature %||% list()
  } else {
    provider_config <- list()
    max_tokens <- list()
    temperature <- list()
  }

  # Resolve API key: check direct api_key first, then env var
  api_key <- provider_config$api_key %||% ""
  if (nchar(api_key) == 0 || api_key == "PASTE_YOUR_KEY_HERE") {
    key_env <- provider_config$api_key_env %||%
      if (provider == "openai") "OPENAI_API_KEY" else "ANTHROPIC_API_KEY"
    api_key <- Sys.getenv(key_env)
    if (nchar(api_key) == 0) {
      stop(sprintf("API key not found. Either set ai.%s.api_key in config.yaml or set env var: Sys.setenv(%s = 'your-key')",
                   provider, key_env))
    }
  }

  key_env <- new.env(parent = emptyenv())
  key_env$key <- api_key

  obj <- list(
    provider = provider,
    key_env = key_env,
    models = provider_config$models %||% .default_models(provider),
    rate_limits = provider_config$rate_limits %||% .default_rate_limits(provider),
    anthropic_api_version = provider_config$anthropic_api_version %||% "2023-06-01",
    max_tokens = max_tokens,
    temperature = temperature,
    context_window = as.integer(provider_config$context_window %||%
      if (provider == "openai") 128000L else 200000L),
    # T1.6 (AC9): methodology rules generated from the config and injected
    # as a system-prompt prefix on every ai_complete() call. When `config`
    # is a full ThematicConfig with a methodology block they are generated
    # eagerly; otherwise this stays empty (legacy / test contexts continue
    # to work bit-for-bit). Rules are stable for the lifetime of the
    # provider object -- the run is the unit of methodology declaration.
    methodology_rules = .resolve_methodology_rules(config)
  )
  class(obj) <- "AIProvider"

  log_info("AI provider initialized: {provider} (primary model: {obj$models$primary})")
  obj
}

#' Resolve methodology rules text from a config (helper for create_ai_provider)
#'
#' Returns the rules string from \code{generate_methodology_rules(config)}
#' when \code{config} is a ThematicConfig (or a list with a methodology
#' block). Returns "" otherwise -- the empty string is a no-op when
#' prepended to a system prompt, so legacy / test contexts continue to
#' work without changes.
#' @keywords internal
.resolve_methodology_rules <- function(config) {
  if (is.null(config)) return("")
  if (!inherits(config, "ThematicConfig") && !is.list(config)) return("")
  has_methodology <- !is.null(tryCatch(config$methodology,
                                        error = function(e) NULL))
  if (!has_methodology) return("")
  tryCatch(generate_methodology_rules(config), error = function(e) {
    log_warn("Could not generate methodology rules: {e$message}")
    ""
  })
}

#' High-level AI completion with retry and error handling
#'
#' Returns a structured list with the response \code{$content} alongside
#' provenance metadata (model, usage, raw_response, finish_reason,
#' prompt_hash, request_id). Callers that only need the response text should
#' extract \code{$content}; downstream audit-log capture (T1.4) and
#' \code{replay_run()} consume the other fields.
#'
#' Note: This is the T1.1 refactor. Prior to T1.1 this function
#' returned a bare character string; the structured-list return is the
#' single most leveraged change because it unblocks T1.4
#' (audit log raw-response capture), T1.2 (Structured Outputs migration),
#' and replay (replay_run from cached responses) simultaneously. The function
#' is internal (not exported), so the change touches only in-package callers.
#'
#' @param provider AIProvider object
#' @param prompt User prompt text
#' @param system_prompt Optional system prompt
#' @param task Task name for looking up max_tokens/temperature defaults
#' @param model Model override (NULL uses models$primary)
#' @param temperature Temperature override
#' @param max_tokens Max tokens override
#' @param json_mode Logical: request JSON response format
#' @param max_retries Number of retry attempts on failure
#' @param response_schema Optional JSON Schema (as an R list) for the response
#'   shape. When provided, the providers enforce the schema
#'   server-side: OpenAI via \code{response_format = list(type = "json_schema",
#'   strict = TRUE, ...)}, Anthropic via a forced tool-use call whose
#'   \code{input_schema} is the schema. The returned \code{$content} is a JSON
#'   string that is guaranteed to parse and conform to the schema, so
#'   downstream \code{parse_json_safely()} is a near-certain success path
#'   rather than a failure-tolerant fallback. When NULL, falls back to the
#'   pre-T1.2 \code{json_mode} path (see \code{R/structured_schemas.R} for
#'   the six task schemas the in-package callers use). Reasoning models
#'   (o1/o3/o4) silently fall back to \code{json_mode} because they don't
#'   support strict json_schema as of writing.
#' @param documents Optional list of source documents to enable Anthropic's
#'   Citations API. Each element is a named list with
#'   \code{$id} (character, internal pakhom identifier preserved on the
#'   returned citations for downstream bridging), \code{$text} (character,
#'   the document content), and optional \code{$title} (character, becomes
#'   Anthropic's document title; defaults to \code{$id}). When non-empty,
#'   the user message is built as a content array with one
#'   \code{document} block per entry (\code{citations.enabled=TRUE}) followed
#'   by a \code{text} block carrying \code{prompt}. The model's response is
#'   parsed for citations; the returned \code{$citations} is a normalized list
#'   of citation objects. \strong{Provider compatibility:} Citations API is
#'   Anthropic-only. Passing \code{documents} to an OpenAI provider raises
#'   an error. \strong{Combining with \code{response_schema}:} Anthropic's
#'   Citations API is incompatible with the newer Structured Outputs
#'   (\code{output_config.format}); pakhom uses forced tool_use for
#'   \code{response_schema}, which is not formally documented as
#'   incompatible but produces no text blocks for citations to attach to.
#'   When both are passed the request is sent as-is and \code{$citations}
#'   will typically be empty -- callers should choose one mode or the other
#'   per the architecture (the Anthropic path uses citations alone).
#' @param methodology_override Optional character (default NULL).
#'   When NULL, the call uses \code{provider$methodology_rules} as the
#'   system-prompt prefix (AC9 default). When a non-NULL string is
#'   supplied, it replaces that prefix for this single call only --
#'   used by the inductive emergent-themes pass to swap in the
#'   inductive variant of the Mode 3 rule (\code{generate_methodology_rules
#'   (config, inductive_pass = TRUE)}) instead of the default deductive
#'   rule that forbids new-construct generation. Empty string (\code{""})
#'   suppresses the rules prefix entirely.
#' @return A list with the following fields (canonical shape, normalized
#'   across OpenAI and Anthropic):
#'   \itemize{
#'     \item \code{content}: character. The response text.
#'     \item \code{model}: character. Model that generated the response
#'       (echoed from the API; may differ from the requested model if the
#'       provider resolved an alias such as \code{gpt-4o} ->
#'       \code{gpt-4o-2024-08-06}).
#'     \item \code{usage}: list with integer fields \code{prompt_tokens},
#'       \code{completion_tokens}, \code{total_tokens}. Anthropic's
#'       \code{input_tokens}/\code{output_tokens} are remapped to the
#'       OpenAI-style names; total is computed when missing.
#'     \item \code{finish_reason}: character. Normalized to \code{"stop"},
#'       \code{"length"}, or \code{"tool_use"} (Anthropic's
#'       \code{end_turn}/\code{max_tokens}/\code{stop_sequence}/\code{tool_use}
#'       are remapped).
#'     \item \code{raw_response}: list. Full parsed API response body, for
#'       replay and debugging.
#'     \item \code{prompt_hash}: character. SHA-256 hex digest of the request
#'       inputs (prompt + system_prompt + model + temperature + max_tokens +
#'       json_mode + response_schema + documents). Used as the cache key for
#'       the planned \code{replay_run()}; stable across R versions and platforms because
#'       the underlying hash is computed over a JSON serialization of the
#'       inputs, not the R object.
#'     \item \code{request_id}: character or \code{NA_character_}.
#'       Provider-assigned request identifier (from the \code{x-request-id}
#'       header for OpenAI or \code{request-id} for Anthropic; falls back to
#'       \code{$id} from the response body).
#'     \item \code{citations}: list. Normalized citations extracted from
#'       text blocks in the response (Anthropic Citations API only). Each
#'       element is a list whose field names exactly mirror Anthropic's
#'       citation schema (\code{type}, \code{cited_text}, \code{document_index},
#'       \code{document_title}, plus type-specific fields:
#'       \code{start_char_index}/\code{end_char_index} for char_location,
#'       \code{start_page_number}/\code{end_page_number} for page_location,
#'       \code{start_block_index}/\code{end_block_index} for
#'       content_block_location). Empty list when \code{documents} was NULL
#'       or no citations were returned. The
#'       \code{make_quotes_from_citations()} converts these to
#'       \code{QuoteProvenance} objects with
#'       \code{citation_source = "anthropic_citations_api"}.
#'   }
#' @keywords internal
ai_complete <- function(provider, prompt, system_prompt = NULL,
                        task = "coding", model = NULL,
                        temperature = NULL, max_tokens = NULL,
                        json_mode = FALSE, max_retries = 3,
                        response_schema = NULL,
                        documents = NULL,
                        methodology_override = NULL) {
  validate_class(provider, "AIProvider")

  # Resolve parameters from task defaults
  if (is.null(model)) model <- provider$models$primary
  if (is.null(temperature)) temperature <- provider$temperature[[task]] %||% 0.3
  if (is.null(max_tokens)) max_tokens <- provider$max_tokens[[task]] %||% 2000

  # Validate documents shape eagerly (before retries). Empty list is treated
  # the same as NULL; a malformed list errors out with a clear message rather
  # than producing an opaque API 400.
  documents <- .validate_documents(documents)

  # T1.6 (AC9): inject methodology rules as a system-prompt prefix on every
  # call. The provider carries the generated rules from the run's config;
  # the caller's system_prompt (task-specific instructions) follows. Rules
  # come FIRST so they "frame" the call before any task-specific
  # instruction can pull the model toward mode-violating behavior.
  #
  # methodology_override is an
  # opt-in per-call replacement of the provider's default rules.
  # The inductive emergent-themes pass uses this to swap in
  # the inductive variant of the Mode 3 rule (which permits new code
  # generation on anomaly residuals) instead of the default deductive
  # rule (which forbids it). NULL = use provider default; non-NULL
  # character = use that string instead.
  rules <- if (!is.null(methodology_override)) {
    as.character(methodology_override)
  } else {
    provider$methodology_rules %||% ""
  }
  if (nzchar(rules)) {
    system_prompt <- if (is.null(system_prompt) || !nzchar(system_prompt)) {
      rules
    } else {
      paste0(rules, "\n\n", system_prompt)
    }
  }

  last_error <- NULL

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      if (provider$provider == "openai") {
        .openai_completion(provider, prompt, system_prompt, model,
                            temperature, max_tokens, json_mode,
                            response_schema = response_schema,
                            documents = documents)
      } else {
        .anthropic_completion(provider, prompt, system_prompt, model,
                               temperature, max_tokens, json_mode,
                               response_schema = response_schema,
                               documents = documents)
      }
    }, error = function(e) {
      last_error <<- e
      log_warn("AI request failed (attempt {attempt}/{max_retries}): {e$message}")

      # Rate limit backoff (capped at 60s)
      if (grepl("429|rate.limit", e$message, ignore.case = TRUE)) {
        wait_time <- min(2^attempt, 60)
        log_info("Rate limited. Waiting {wait_time}s...")
        Sys.sleep(wait_time)
      } else if (attempt < max_retries) {
        Sys.sleep(1)
      }

      NULL
    })

    if (!is.null(result)) return(result)
  }

  stop(sprintf("AI request failed after %d attempts: %s",
               max_retries, last_error$message %||% "unknown error"))
}

#' Send a quick completion using the fast/cheap model
#'
#' Thin wrapper around \code{\link{ai_complete}} that selects the provider's
#' \code{models$fast} model. Returns the same structured list shape as
#' \code{ai_complete()} -- callers extract \code{$content} for the text.
#'
#' @param provider AIProvider object
#' @param prompt User prompt
#' @param system_prompt System prompt
#' @param task Task name
#' @param json_mode JSON mode
#' @param response_schema Optional JSON schema (see \code{\link{ai_complete}})
#' @param documents Optional source documents for Anthropic Citations API
#'   (see \code{\link{ai_complete}})
#' @return List of the same shape as \code{\link{ai_complete}}.
#' @keywords internal
ai_complete_fast <- function(provider, prompt, system_prompt = NULL,
                              task = "sentiment", json_mode = FALSE,
                              response_schema = NULL,
                              documents = NULL) {
  ai_complete(provider, prompt, system_prompt,
              task = task,
              model = provider$models$fast %||% provider$models$primary,
              json_mode = json_mode,
              response_schema = response_schema,
              documents = documents)
}

# ==============================================================================
# OpenAI Implementation
# ==============================================================================

.openai_completion <- function(provider, prompt, system_prompt, model,
                                temperature, max_tokens, json_mode,
                                response_schema = NULL,
                                documents = NULL) {
  if (length(documents) > 0L) {
    stop(
      "Source documents (Citations API) are Anthropic-only. ",
      "Pass an Anthropic AIProvider, or use the schema-level offsets-only ",
      "discipline (start_char/end_char without a `text` field) which works ",
      "for any provider via the verification ladder. ",
      "Citations API: https://docs.anthropic.com/en/docs/build-with-claude/citations",
      call. = FALSE
    )
  }
  is_reasoning <- .is_reasoning_model(model)

  messages <- list()
  if (!is.null(system_prompt) && nchar(system_prompt) > 0) {
    # Reasoning models (o1/o3/o4) use "developer" role instead of "system"
    role <- if (is_reasoning) "developer" else "system"
    messages <- c(messages, list(list(role = role, content = system_prompt)))
  }
  messages <- c(messages, list(list(role = "user", content = prompt)))

  body <- list(
    model = model,
    messages = messages
  )

  if (is_reasoning) {
    # Reasoning models use max_completion_tokens and don't support temperature
    body$max_completion_tokens <- max_tokens
  } else {
    body$temperature <- temperature
    body$max_tokens <- max_tokens
  }

  # Pass OpenAI's `seed` field for best-effort
  # determinism. OpenAI's docs say `seed` is best-effort given a stable
  # `system_fingerprint`; that fingerprint is preserved inside the cached
  # raw_response (when capture_raw_responses is on), where a divergent
  # fingerprint between runs can be inspected. Default 42 to match the R-side
  # test_mode seed; users can override via config$ai$openai$seed.
  openai_seed <- provider$openai_seed %||%
                 (if (is.list(provider$config)) provider$config$ai$openai$seed) %||%
                 42L
  body$seed <- as.integer(openai_seed)

  # T1.2: structured outputs via json_schema (strict mode). Reasoning models
  # don't support strict json_schema as of writing; they fall back to
  # json_mode if the caller asked for either. The schema is validated at
  # call time so a malformed schema fails fast with a useful error rather
  # than producing an opaque OpenAI 400.
  use_schema <- !is.null(response_schema) && !is_reasoning
  if (use_schema) {
    .validate_schema(response_schema, path = "$response_schema")
    body$response_format <- list(
      type = "json_schema",
      json_schema = list(
        name   = "structured_response",
        strict = TRUE,
        schema = response_schema
      )
    )
  } else if (isTRUE(json_mode)) {
    body$response_format <- list(type = "json_object")
  }

  resp <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers(
      Authorization = paste("Bearer", provider$key_env$key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(body) |>
    req_timeout(120) |>
    req_options(connecttimeout = 15, low_speed_limit = 1, low_speed_time = 60) |>
    req_retry(max_tries = 1) |>
    req_error(is_error = ~ FALSE) |>
    req_perform()

  status <- resp_status(resp)
  if (status != 200) {
    body_text <- resp_body_string(resp)
    stop(sprintf("OpenAI API error (HTTP %d): %s", status, substr(body_text, 1, 500)))
  }

  parsed <- resp_body_json(resp)
  if (length(parsed$choices) == 0) {
    stop("OpenAI API returned empty choices array")
  }
  content <- parsed$choices[[1]]$message$content
  finish_reason <- parsed$choices[[1]]$finish_reason %||% "stop"

  if (finish_reason == "length") {
    log_warn("Response truncated (max_tokens={max_tokens}). Consider increasing.")
  }

  request_id <- httr2::resp_header(resp, "x-request-id") %||%
                parsed$id %||% NA_character_

  list(
    content       = content,
    model         = parsed$model %||% model,
    usage         = .normalize_usage_openai(parsed$usage),
    finish_reason = finish_reason,
    raw_response  = parsed,
    prompt_hash   = .compute_prompt_hash(prompt, system_prompt, model,
                                          temperature, max_tokens, json_mode,
                                          response_schema = response_schema,
                                          documents = NULL),
    request_id    = request_id,
    # OpenAI doesn't have a Citations API; this errors earlier when documents is
    # non-NULL. Always-present empty list keeps the canonical shape.
    citations     = list()
  )
}

# ==============================================================================
# Anthropic Implementation
# ==============================================================================

.anthropic_completion <- function(provider, prompt, system_prompt, model,
                                   temperature, max_tokens, json_mode,
                                   response_schema = NULL,
                                   documents = NULL) {
  # Defensive guard: forced tool_use (response_schema) produces only a
  # tool_use block in the response with no text blocks, which means
  # citations have nowhere to attach. The empty-citations result would be
  # silently fine at the API level but would invisibly downgrade every
  # quote to citation_source = "model_freeform" -- making the dashboard
  # falsely report "model freeform (detection only)" for an Anthropic
  # run that thought it was using the prevention layer. Refuse rather
  # than silently mismatch what the report claims.
  if (!is.null(response_schema) && length(documents) > 0L) {
    stop(
      "Anthropic Citations API (`documents`) cannot be combined with ",
      "`response_schema` (forced tool_use): the model returns only a ",
      "tool_use block with no text blocks for citations to attach to, ",
      "so citations would be silently empty and every quote would ",
      "downgrade to citation_source = 'model_freeform'. Choose one mode: ",
      "use `documents` alone (JSON-mode prompt) for the prevention layer, ",
      "or `response_schema` alone (without documents) for the schema-only ",
      "path. The .code_entry_progressive dispatch picks the right ",
      "one per provider.",
      call. = FALSE
    )
  }

  # Anthropic uses 'system' as a top-level parameter, not in messages.
  # When documents are supplied, the user message
  # content becomes a content array of one document block per source
  # (citations.enabled=TRUE) followed by a text block carrying the prompt.
  # When documents is NULL/empty, the legacy string-content shape is kept
  # bit-for-bit so existing callers' request bodies don't change.
  user_content <- .anthropic_build_user_content(prompt, documents)
  messages <- list(list(
    role    = "user",
    content = user_content %||% prompt
  ))

  effective_system <- system_prompt %||% ""

  # T1.2: structured outputs via forced tool-use. Anthropic doesn't have an
  # OpenAI-style response_format; the canonical pattern is to register a
  # tool whose input_schema matches the desired shape and force the model
  # to call it via tool_choice. The schema is validated at call time.
  use_schema <- !is.null(response_schema)
  if (use_schema) {
    .validate_schema(response_schema, path = "$response_schema")
  } else if (isTRUE(json_mode)) {
    # Legacy json_mode: append instruction to system prompt
    effective_system <- paste0(effective_system,
      "\n\nIMPORTANT: Respond with valid JSON only. No additional text or markdown.")
  }

  body <- list(
    model = model,
    messages = messages,
    max_tokens = max_tokens,
    temperature = temperature
  )

  if (nchar(effective_system) > 0) {
    body$system <- effective_system
  }

  if (use_schema) {
    body$tools <- list(list(
      name         = "record_analysis",
      description  = "Record the analysis result in the structured format. ALWAYS call this tool exactly once with the result; do not respond in plain text.",
      input_schema = response_schema
    ))
    body$tool_choice <- list(type = "tool", name = "record_analysis")
  }

  resp <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      `x-api-key` = provider$key_env$key,
      `anthropic-version` = provider$anthropic_api_version %||% "2023-06-01",
      `Content-Type` = "application/json"
    ) |>
    req_body_json(body) |>
    req_timeout(120) |>
    req_options(connecttimeout = 15, low_speed_limit = 1, low_speed_time = 60) |>
    req_retry(max_tries = 1) |>
    req_error(is_error = ~ FALSE) |>
    req_perform()

  status <- resp_status(resp)
  if (status != 200) {
    body_text <- resp_body_string(resp)
    stop(sprintf("Anthropic API error (HTTP %d): %s", status, substr(body_text, 1, 500)))
  }

  parsed <- resp_body_json(resp)
  if (length(parsed$content) == 0) {
    stop("Anthropic API returned empty content array")
  }

  # T1.2: when response_schema is set tool_use was forced, so extract the
  # tool input and JSON-stringify it into $content (caller's contract is
  # "content is a string"; downstream parse_json_safely round-trips it).
  # When no schema, take the first text block as before.
  #
  # T0.1 part 3b: in either case walk the full content array for any
  # citations attached to text blocks. Forced tool_use produces no text
  # blocks (so citations is empty) -- callers that want citations must
  # avoid response_schema. Free-text responses with documents enabled
  # return citations.
  if (use_schema) {
    tool_block <- NULL
    for (item in parsed$content) {
      if (identical(item$type, "tool_use") &&
          identical(item$name, "record_analysis")) {
        tool_block <- item
        break
      }
    }
    if (is.null(tool_block) || is.null(tool_block$input)) {
      stop("Anthropic API: forced tool_use returned no record_analysis call")
    }
    content <- as.character(jsonlite::toJSON(tool_block$input, auto_unbox = TRUE))
    # tool_use was forced so stop_reason is always "tool_use" -- map to "stop"
    # because semantically this was a normal completion, not the model
    # choosing to invoke an external tool.
    finish_reason <- "stop"
  } else {
    # Concatenate text from ALL text blocks in the response (when citations
    # are enabled the model emits multiple text blocks interleaved with
    # citation-attached blocks; the caller's contract is a single string).
    # When citations are disabled this collapses to the legacy single-block
    # behavior because parsed$content has exactly one text block.
    text_chunks <- character(0)
    for (item in parsed$content) {
      if (identical(item$type, "text") && !is.null(item$text)) {
        text_chunks <- c(text_chunks, as.character(item$text))
      }
    }
    content <- paste(text_chunks, collapse = "")
    raw_finish <- parsed$stop_reason %||% "end_turn"
    finish_reason <- .normalize_anthropic_finish(raw_finish)
  }

  citations <- .anthropic_extract_citations(parsed$content)

  if (finish_reason == "length") {
    log_warn("Response truncated (max_tokens={max_tokens}). Consider increasing.")
  }

  request_id <- httr2::resp_header(resp, "request-id") %||%
                parsed$id %||% NA_character_

  list(
    content       = content,
    model         = parsed$model %||% model,
    usage         = .normalize_usage_anthropic(parsed$usage),
    finish_reason = finish_reason,
    raw_response  = parsed,
    prompt_hash   = .compute_prompt_hash(prompt, system_prompt, model,
                                          temperature, max_tokens, json_mode,
                                          response_schema = response_schema,
                                          documents = documents),
    request_id    = request_id,
    citations     = citations
  )
}

# ==============================================================================
# Internal Helpers -- Response Structuring
# ==============================================================================
# These helpers shape the structured return value of ai_complete(). The goal
# is to give the audit log (T1.4) and replay_run() deterministic,
# provider-agnostic access to provenance metadata while keeping the bare
# content extraction trivial for existing callers (response$content).

#' Compute a deterministic SHA-256 hash of an AI request's inputs
#'
#' Hashes the JSON serialization of the request rather than the R object
#' itself, so the digest is stable across R versions, platforms, and
#' serialization-format changes. The set of fields hashed is exactly those
#' that determine the response: prompt + system_prompt + model + temperature
#' + max_tokens + json_mode + response_schema + documents. Used as the cache
#' key for replay_run().
#'
#' T1.2 added response_schema; T0.1 part 3b added documents. Pre-
#' addition callers (NULL for the new arg) produce the same hashes as
#' before because NULL serializes to "null" and the field was implicitly
#' absent.
#'
#' @param prompt User prompt string
#' @param system_prompt System prompt string (NULL becomes "")
#' @param model Model name
#' @param temperature Numeric temperature
#' @param max_tokens Integer max tokens
#' @param json_mode Logical
#' @param response_schema Optional JSON Schema (R list); NULL when no
#'   structured output was requested.
#' @param documents Optional list of source documents (Anthropic Citations
#'   API). NULL or empty list when citations were not requested. Hashing
#'   documents is required because the same prompt over different source
#'   corpora must produce different cache keys (otherwise replay_run()
#'   would silently return a citation-less response for a citations
#'   request, or vice versa).
#' @return Character: SHA-256 hex digest (64 chars)
#' @keywords internal
.compute_prompt_hash <- function(prompt, system_prompt, model,
                                  temperature, max_tokens, json_mode,
                                  response_schema = NULL,
                                  documents = NULL) {
  # Normalize empty documents list to NULL so cache keys for "no documents"
  # are stable regardless of whether the caller passed NULL or list().
  if (length(documents) == 0L) documents <- NULL
  key <- jsonlite::toJSON(list(
    prompt          = prompt,
    system_prompt   = system_prompt %||% "",
    model           = model,
    temperature     = temperature,
    max_tokens      = max_tokens,
    json_mode       = isTRUE(json_mode),
    response_schema = response_schema,
    documents       = documents
  ), auto_unbox = TRUE, null = "null")
  digest::digest(as.character(key), algo = "sha256", serialize = FALSE)
}

#' Normalize OpenAI usage payload to the canonical pakhom shape
#' @param usage Parsed list from OpenAI response \code{$usage}
#' @return list with integer fields prompt_tokens, completion_tokens,
#'   total_tokens (NA_integer_ when payload missing or fields absent)
#' @keywords internal
.normalize_usage_openai <- function(usage) {
  if (is.null(usage)) {
    return(list(prompt_tokens     = NA_integer_,
                completion_tokens = NA_integer_,
                total_tokens      = NA_integer_))
  }
  list(
    prompt_tokens     = as.integer(usage$prompt_tokens %||% NA),
    completion_tokens = as.integer(usage$completion_tokens %||% NA),
    total_tokens      = as.integer(usage$total_tokens %||% NA)
  )
}

#' Normalize Anthropic usage payload to the canonical pakhom shape
#'
#' Anthropic returns \code{input_tokens} and \code{output_tokens} (no total).
#' Maps to the OpenAI-style \code{prompt_tokens}/\code{completion_tokens}
#' naming and computes the total. If either count is missing the total is
#' \code{NA_integer_} (NA propagates through integer addition).
#'
#' @param usage Parsed list from Anthropic response \code{$usage}
#' @return list with integer fields prompt_tokens, completion_tokens, total_tokens
#' @keywords internal
.normalize_usage_anthropic <- function(usage) {
  if (is.null(usage)) {
    return(list(prompt_tokens     = NA_integer_,
                completion_tokens = NA_integer_,
                total_tokens      = NA_integer_))
  }
  input  <- as.integer(usage$input_tokens  %||% NA)
  output <- as.integer(usage$output_tokens %||% NA)
  list(
    prompt_tokens     = input,
    completion_tokens = output,
    total_tokens      = input + output
  )
}

#' Map Anthropic stop_reason to canonical finish_reason
#'
#' Canonical values: "stop" (normal completion), "length" (truncated by
#' max_tokens), "tool_use" (model invoked a tool). Unknown values pass
#' through unchanged for forward compatibility with new stop_reasons.
#'
#' @param stop_reason Anthropic \code{stop_reason} value
#' @return Canonical finish_reason character
#' @keywords internal
.normalize_anthropic_finish <- function(stop_reason) {
  switch(stop_reason,
    "end_turn"      = "stop",
    "max_tokens"    = "length",
    "stop_sequence" = "stop",
    "tool_use"      = "tool_use",
    stop_reason
  )
}

# ==============================================================================
# Citations API helpers
# ==============================================================================
# T0.1 anti-fabrication has two layers: DETECTION (verification ladder in
# R/quote_provenance.R, shipped phases 17-19) and PREVENTION (Anthropic
# Citations API, this phase). Citations API guarantees server-side that
# returned indices are valid pointers into the provided document -- the
# model literally cannot return a span that doesn't exist in the source.
# This module provides the provider-side primitives; the bridge from
# Anthropic citations to pakhom's QuoteProvenance schema lives in
# R/quote_provenance.R and the caller wiring in R/09_coding.R
#

#' Validate and normalize the documents argument
#'
#' Accepts NULL, an empty list, or a list of named lists with required
#' \code{$id} and \code{$text} fields and an optional \code{$title} field.
#' Returns the normalized list (or NULL when empty); errors with a clear
#' message on a malformed input rather than producing an opaque API 400.
#'
#' Each document gets a defaulted \code{$title = $id} when not supplied,
#' because the model's citations include a \code{document_title} that
#' downstream code uses for human-readable display; without a title the
#' field would round-trip as NULL, complicating the bridge.
#'
#' @param documents Caller-supplied documents list (or NULL).
#' @return NULL when input is NULL/empty, otherwise the normalized list.
#' @keywords internal
.validate_documents <- function(documents) {
  if (is.null(documents)) return(NULL)
  if (!is.list(documents)) {
    stop("`documents` must be a list of named lists (one per source document)",
         call. = FALSE)
  }
  if (length(documents) == 0L) return(NULL)

  for (i in seq_along(documents)) {
    d <- documents[[i]]
    if (!is.list(d) || is.null(names(d))) {
      stop(sprintf("documents[[%d]] must be a named list with $id and $text",
                   i), call. = FALSE)
    }
    if (is.null(d$id) || !is.character(d$id) || length(d$id) != 1L ||
        !nzchar(d$id)) {
      stop(sprintf("documents[[%d]]$id must be a non-empty string", i),
           call. = FALSE)
    }
    if (is.null(d$text) || !is.character(d$text) || length(d$text) != 1L) {
      stop(sprintf("documents[[%d]]$text must be a single string", i),
           call. = FALSE)
    }
    if (!is.null(d$title) &&
        (!is.character(d$title) || length(d$title) != 1L)) {
      stop(sprintf("documents[[%d]]$title must be a single string when supplied",
                   i), call. = FALSE)
    }
    if (is.null(d$title)) documents[[i]]$title <- d$id
  }
  documents
}

#' Build the Anthropic user-message content array for a Citations API request
#'
#' When \code{documents} is non-empty, the user message must be a content
#' array of one \code{document} block per source (with
#' \code{citations.enabled=TRUE}) followed by a \code{text} block carrying
#' the prompt. When NULL/empty, returns NULL so the caller falls back to
#' the legacy \code{content = prompt} string shape -- this keeps existing
#' (non-citations) request bodies bit-for-bit identical.
#'
#' Anthropic accepts plain-text source via
#' \code{source = list(type="text", media_type="text/plain", data=text)}
#' and chunks it into sentences; returned citations carry char_location
#' indices into the original text. The Anthropic path uses this mode exclusively;
#' custom_content / PDF / file-id sources can be added later by extending
#' this helper without breaking callers.
#'
#' @param prompt User prompt string (the question/instruction to the model).
#' @param documents Validated documents list from
#'   \code{\link{.validate_documents}} or NULL.
#' @return List of content blocks, or NULL when no documents.
#' @keywords internal
.anthropic_build_user_content <- function(prompt, documents) {
  if (length(documents) == 0L) return(NULL)
  blocks <- vector("list", length(documents) + 1L)
  for (i in seq_along(documents)) {
    d <- documents[[i]]
    blocks[[i]] <- list(
      type   = "document",
      source = list(
        type       = "text",
        media_type = "text/plain",
        data       = d$text
      ),
      title = d$title %||% d$id,
      citations = list(enabled = TRUE)
    )
  }
  blocks[[length(documents) + 1L]] <- list(type = "text", text = prompt)
  blocks
}

#' Extract citations from a parsed Anthropic response content array
#'
#' Walks the \code{parsed$content} list and collects every citation
#' attached to a text block. Each citation is preserved with Anthropic's
#' field names exactly (no remapping) so the bridge in
#' \code{R/quote_provenance.R} can dispatch on \code{type}:
#' \itemize{
#'   \item \code{char_location}: \code{start_char_index},
#'     \code{end_char_index} (0-indexed, exclusive end) -- pakhom's
#'     QuoteProvenance schema uses the same convention.
#'   \item \code{page_location}: \code{start_page_number},
#'     \code{end_page_number} (1-indexed, exclusive end) -- PDF sources.
#'   \item \code{content_block_location}: \code{start_block_index},
#'     \code{end_block_index} (0-indexed, exclusive end) -- custom_content
#'     sources.
#' }
#' All three types share \code{type}, \code{cited_text},
#' \code{document_index}, \code{document_title}.
#'
#' Robustness: skips non-text blocks (tool_use blocks have no citations);
#' skips text blocks whose \code{citations} is NULL or empty; preserves
#' unknown citation types unchanged for forward compatibility.
#'
#' @param parsed_content The \code{parsed$content} list from the API
#'   response (may be a list of blocks, or empty).
#' @return List of citation objects in the order they were emitted across
#'   all text blocks. Empty list when no citations were returned.
#' @keywords internal
.anthropic_extract_citations <- function(parsed_content) {
  if (is.null(parsed_content) || length(parsed_content) == 0L) return(list())
  out <- list()
  for (block in parsed_content) {
    if (!is.list(block)) next
    if (!identical(block$type, "text")) next
    cites <- block$citations
    if (is.null(cites) || length(cites) == 0L) next
    for (cite in cites) {
      if (!is.list(cite)) next
      out[[length(out) + 1L]] <- .normalize_anthropic_citation(cite)
    }
  }
  out
}

#' Normalize a single citation into the canonical pakhom shape
#'
#' Coerces numeric index fields to integer (jsonlite::fromJSON returns them
#' as numeric by default), handles missing-field defaults, and preserves
#' Anthropic's field names verbatim so the bridge in 21b doesn't need a
#' field-name lookup table.
#' @keywords internal
.normalize_anthropic_citation <- function(cite) {
  type <- as.character(cite$type %||% NA_character_)[1]
  base <- list(
    type           = type,
    cited_text     = as.character(cite$cited_text %||% NA_character_)[1],
    document_index = as.integer(cite$document_index %||% NA_integer_)[1],
    document_title = as.character(cite$document_title %||% NA_character_)[1]
  )
  # Type-specific fields. Defaults are NA when the field is absent so the
  # bridge can dispatch on `type` without missing-field surprises.
  switch(type,
    "char_location" = c(base, list(
      start_char_index = as.integer(cite$start_char_index %||% NA_integer_)[1],
      end_char_index   = as.integer(cite$end_char_index   %||% NA_integer_)[1]
    )),
    "page_location" = c(base, list(
      start_page_number = as.integer(cite$start_page_number %||% NA_integer_)[1],
      end_page_number   = as.integer(cite$end_page_number   %||% NA_integer_)[1]
    )),
    "content_block_location" = c(base, list(
      start_block_index = as.integer(cite$start_block_index %||% NA_integer_)[1],
      end_block_index   = as.integer(cite$end_block_index   %||% NA_integer_)[1]
    )),
    # Unknown citation types: pass through with their raw fields stripped of
    # NULL so downstream code at least has something predictable to inspect.
    c(base, cite[setdiff(names(cite),
                         c("type", "cited_text", "document_index",
                           "document_title"))])
  )
}

# ==============================================================================
# Embedding API
# ==============================================================================

#' Compute text embeddings via AI provider
#'
#' Calls the embedding model to compute vector representations of text.
#' Currently supports OpenAI's text-embedding models. Falls back gracefully
#' if the provider doesn't support embeddings.
#'
#' @param provider AIProvider object
#' @param texts Character vector of texts to embed
#' @param model Embedding model override (NULL uses provider default)
#' @return Numeric matrix (rows = texts, cols = dimensions), or NULL on failure
#' @keywords internal
compute_embeddings <- function(provider, texts, model = NULL) {
  validate_class(provider, "AIProvider")

  # Test-mock short-circuit: unit tests use fake keys like
  # `sk-test-fake-key-for-unit-tests` (see tests/testthat/helper.R).
  # Returning NULL early avoids making real HTTP requests to
  # api.openai.com that would 401 and slow the test suite. Production
  # keys never start with "sk-test-" or "sk-ant-test-".
  test_key <- isTRUE(grepl("^sk-(ant-)?test-",
                            provider$key_env$key %||% "",
                            ignore.case = TRUE))
  if (test_key) return(NULL)

  if (provider$provider != "openai") {
    log_debug("Embeddings only supported for OpenAI provider; skipping")
    return(NULL)
  }

  model <- model %||% provider$models$embedding %||% "text-embedding-3-small"

  # Batch texts (OpenAI allows up to 2048 inputs per call)
  batch_size <- 100L
  n <- length(texts)
  all_embeddings <- list()

  for (start in seq(1, n, by = batch_size)) {
    end <- min(start + batch_size - 1L, n)
    batch <- texts[start:end]

    result <- tryCatch({
      body <- list(
        model = model,
        input = as.list(batch)
      )

      resp <- request("https://api.openai.com/v1/embeddings") |>
        req_headers(
          Authorization = paste("Bearer", provider$key_env$key),
          `Content-Type` = "application/json"
        ) |>
        req_body_json(body) |>
        req_timeout(60) |>
        req_error(is_error = ~ FALSE) |>
        req_perform()

      status <- resp_status(resp)
      if (status != 200) {
        log_warn("Embedding API error (HTTP {status})")
        return(NULL)
      }

      parsed <- resp_body_json(resp)
      # Extract embedding vectors (sorted by index)
      sorted_data <- parsed$data[order(vapply(parsed$data, function(d) d$index, integer(1)))]
      lapply(sorted_data, function(d) unlist(d$embedding))
    }, error = function(e) {
      log_warn("Embedding request failed: {e$message}")
      NULL
    })

    if (is.null(result)) return(NULL)
    all_embeddings <- c(all_embeddings, result)
  }

  # Convert to matrix
  tryCatch({
    do.call(rbind, all_embeddings)
  }, error = function(e) {
    log_warn("Failed to build embedding matrix: {e$message}")
    NULL
  })
}

#' Compute cosine similarity matrix from embeddings
#' @param embeddings Numeric matrix (rows = items, cols = dimensions)
#' @return Numeric matrix of pairwise cosine similarities
#' @keywords internal
.cosine_similarity_matrix <- function(embeddings) {
  # Normalize rows to unit vectors
  norms <- sqrt(rowSums(embeddings^2))
  norms[norms == 0] <- 1  # avoid division by zero
  normed <- embeddings / norms
  # Cosine similarity = dot product of unit vectors
  tcrossprod(normed)
}

# ==============================================================================
# Defaults
# ==============================================================================

#' Check if a model is a reasoning model (o1/o3/o4 series)
#' These need different API parameters (no temperature, max_completion_tokens)
#' @param model Model name string
#' @keywords internal
.is_reasoning_model <- function(model) {
  grepl("^(o1|o3|o4)", model, ignore.case = TRUE)
}

.default_models <- function(provider) {
  if (provider == "openai") {
    list(primary = "gpt-4o", fast = "gpt-4o-mini", reasoning = "o3-mini",
         embedding = "text-embedding-3-small")
  } else {
    list(primary = "claude-sonnet-4-20250514", fast = "claude-haiku-4-5-20251001")
  }
}

.default_rate_limits <- function(provider) {
  if (provider == "openai") {
    list(requests_per_minute = 5000, tokens_per_minute = 800000,
         batch_size = 20, delay_between_batches = 0.5)
  } else {
    list(requests_per_minute = 1000, tokens_per_minute = 400000,
         batch_size = 10, delay_between_batches = 1.0)
  }
}

#' Print method for AIProvider
#' @param x AIProvider object
#' @param ... Additional arguments
#' @export
print.AIProvider <- function(x, ...) {
  cat(sprintf("AIProvider [%s]\n", x$provider))
  cat(sprintf("  Primary model:  %s\n", x$models$primary))
  cat(sprintf("  Fast model:     %s\n", x$models$fast %||% "(same)"))
  cat(sprintf("  Context window: %s tokens\n", format(x$context_window, big.mark = ",")))
  cat(sprintf("  Batch size:     %d\n", x$rate_limits$batch_size))
  invisible(x)
}
