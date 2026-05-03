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
      if (provider == "openai") 128000L else 200000L)
  )
  class(obj) <- "AIProvider"

  log_info("AI provider initialized: {provider} (primary model: {obj$models$primary})")
  obj
}

#' High-level AI completion with retry and error handling
#'
#' Returns a structured list with the response \code{$content} alongside
#' provenance metadata (model, usage, raw_response, finish_reason,
#' prompt_hash, request_id). Callers that only need the response text should
#' extract \code{$content}; downstream audit-log capture (T1.4) and
#' \code{replay_run()} (OS.5) consume the other fields.
#'
#' Note: This is a Sprint-4 T1.1 refactor. Prior to T1.1 this function
#' returned a bare character string; the structured-list return is the
#' single most leveraged change in Sprint-4 because it unblocks T1.4
#' (audit log raw-response capture), T1.2 (Structured Outputs migration),
#' and OS.5 (replay_run from cached responses) simultaneously. The function
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
#'   shape. When provided (Sprint-4 T1.2), the providers enforce the schema
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
#'       replay (OS.5) and debugging.
#'     \item \code{prompt_hash}: character. SHA-256 hex digest of the request
#'       inputs (prompt + system_prompt + model + temperature + max_tokens +
#'       json_mode). Used as the cache key for \code{replay_run()}; stable
#'       across R versions and platforms because the underlying hash is
#'       computed over a JSON serialization of the inputs, not the R object.
#'     \item \code{request_id}: character or \code{NA_character_}.
#'       Provider-assigned request identifier (from the \code{x-request-id}
#'       header for OpenAI or \code{request-id} for Anthropic; falls back to
#'       \code{$id} from the response body).
#'   }
#' @keywords internal
ai_complete <- function(provider, prompt, system_prompt = NULL,
                        task = "coding", model = NULL,
                        temperature = NULL, max_tokens = NULL,
                        json_mode = FALSE, max_retries = 3,
                        response_schema = NULL) {
  validate_class(provider, "AIProvider")

  # Resolve parameters from task defaults
  if (is.null(model)) model <- provider$models$primary
  if (is.null(temperature)) temperature <- provider$temperature[[task]] %||% 0.3
  if (is.null(max_tokens)) max_tokens <- provider$max_tokens[[task]] %||% 2000

  last_error <- NULL

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      if (provider$provider == "openai") {
        .openai_completion(provider, prompt, system_prompt, model,
                            temperature, max_tokens, json_mode,
                            response_schema = response_schema)
      } else {
        .anthropic_completion(provider, prompt, system_prompt, model,
                               temperature, max_tokens, json_mode,
                               response_schema = response_schema)
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
#' @return List of the same shape as \code{\link{ai_complete}}.
#' @keywords internal
ai_complete_fast <- function(provider, prompt, system_prompt = NULL,
                              task = "sentiment", json_mode = FALSE,
                              response_schema = NULL) {
  ai_complete(provider, prompt, system_prompt,
              task = task,
              model = provider$models$fast %||% provider$models$primary,
              json_mode = json_mode,
              response_schema = response_schema)
}

# ==============================================================================
# OpenAI Implementation
# ==============================================================================

.openai_completion <- function(provider, prompt, system_prompt, model,
                                temperature, max_tokens, json_mode,
                                response_schema = NULL) {
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
                                          response_schema = response_schema),
    request_id    = request_id
  )
}

# ==============================================================================
# Anthropic Implementation
# ==============================================================================

.anthropic_completion <- function(provider, prompt, system_prompt, model,
                                   temperature, max_tokens, json_mode,
                                   response_schema = NULL) {
  # Anthropic uses 'system' as a top-level parameter, not in messages
  messages <- list(list(role = "user", content = prompt))

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

  # T1.2: when response_schema is set we forced tool_use, so extract the
  # tool input and JSON-stringify it into $content (caller's contract is
  # "content is a string"; downstream parse_json_safely round-trips it).
  # When no schema, take the first text block as before.
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
    # We forced tool_use so stop_reason is always "tool_use" -- map to "stop"
    # because semantically this was a normal completion, not the model
    # choosing to invoke an external tool.
    finish_reason <- "stop"
  } else {
    content <- parsed$content[[1]]$text
    raw_finish <- parsed$stop_reason %||% "end_turn"
    finish_reason <- .normalize_anthropic_finish(raw_finish)
  }

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
                                          response_schema = response_schema),
    request_id    = request_id
  )
}

# ==============================================================================
# Internal Helpers -- Response Structuring (Sprint-4 T1.1)
# ==============================================================================
# These helpers shape the structured return value of ai_complete(). The goal
# is to give the audit log (T1.4) and replay_run() (OS.5) deterministic,
# provider-agnostic access to provenance metadata while keeping the bare
# content extraction trivial for existing callers (response$content).

#' Compute a deterministic SHA-256 hash of an AI request's inputs
#'
#' Hashes the JSON serialization of the request rather than the R object
#' itself, so the digest is stable across R versions, platforms, and
#' serialization-format changes. The set of fields hashed is exactly those
#' that determine the response: prompt + system_prompt + model + temperature
#' + max_tokens + json_mode + response_schema. Used as the cache key for
#' replay_run() (OS.5).
#'
#' Sprint-4 T1.2 added response_schema to the hash inputs. Pre-T1.2
#' callers (response_schema = NULL) produce the same hashes as before
#' because NULL serializes to "null" and the field was implicitly absent.
#'
#' @param prompt User prompt string
#' @param system_prompt System prompt string (NULL becomes "")
#' @param model Model name
#' @param temperature Numeric temperature
#' @param max_tokens Integer max tokens
#' @param json_mode Logical
#' @param response_schema Optional JSON Schema (R list); NULL when no
#'   structured output was requested.
#' @return Character: SHA-256 hex digest (64 chars)
#' @keywords internal
.compute_prompt_hash <- function(prompt, system_prompt, model,
                                  temperature, max_tokens, json_mode,
                                  response_schema = NULL) {
  key <- jsonlite::toJSON(list(
    prompt          = prompt,
    system_prompt   = system_prompt %||% "",
    model           = model,
    temperature     = temperature,
    max_tokens      = max_tokens,
    json_mode       = isTRUE(json_mode),
    response_schema = response_schema
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
