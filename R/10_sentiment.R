# ==============================================================================
# Sentiment Analysis -- Hybrid Batched Processing
# ==============================================================================
# Sends 10-20 entries per API call (vs. 1 per call in the old script).
# ~90% fewer API calls for this step.
# ==============================================================================

#' Run batch sentiment analysis on all entries
#'
#' @param data Standardized tibble with std_text column
#' @param provider AIProvider object
#' @param config Sentiment config section
#' @param checkpoint CheckpointManager (or NULL). On resume after a crash,
#'   entries already scored in the step's partial checkpoint are adopted
#'   (matched by \code{std_id}) and skipped, so only unscored entries are
#'   re-sent to the provider.
#' @param research_focus Research focus string
#' @param coding_state ProgressiveCodingState (or NULL). When provided,
#'   only processes entries in the analytic sample (those with codes) and
#'   includes assigned codes as context for more accurate sentiment scoring.
#' @param audit_log An AuditLog object (from \code{init_audit_log}) for
#'   recording each sentiment-assignment decision, or NULL to disable
#'   audit logging for this step.
#' @param response_cache An optional ResponseCache object (from
#'   \code{\link{init_response_cache}}). When provided, raw API responses
#'   for each per-batch sentiment ai_complete_fast() call are written to
#'   the cache and referenced from the audit log (T1.4). Pass \code{NULL}
#'   to skip raw-response capture.
#' @return tibble with sentiment_score, confidence,
#'   all_emotions (semicolon-separated), emotion_intensity columns added
#' @export
analyze_sentiment <- function(data, provider, config = list(),
                               checkpoint = NULL, research_focus = "",
                               coding_state = NULL,
                               audit_log = NULL,
                               response_cache = NULL) {
  validate_data_columns(data, c("std_text", "std_id"), "analyze_sentiment")
  validate_provider(provider, caller = "analyze_sentiment")

  # Code-aware mode: build lookup of codes per entry for context injection
  code_aware <- isTRUE(config$code_aware) && !is.null(coding_state) &&
    inherits(coding_state, "ProgressiveCodingState")
  entry_codes_lookup <- list()
  if (code_aware) {
    for (eid in names(coding_state$entry_results)) {
      er <- coding_state$entry_results[[eid]]
      if (!isTRUE(er$skipped) && length(er$codes_assigned) > 0) {
        code_names <- vapply(er$codes_assigned, function(key) {
          coding_state$codebook[[key]]$code_name %||% key
        }, character(1))
        entry_codes_lookup[[eid]] <- code_names
      }
    }
    log_info("Code-aware sentiment: {length(entry_codes_lookup)} entries with code context")
  }

  batch_size <- config$batch_size %||% 15
  dynamic_batching <- config$dynamic_batching %||% TRUE
  emotions <- config$emotion_categories %||%
    c("joy", "sadness", "anger", "fear", "surprise", "disgust", "trust", "anticipation")
  emotions_str <- paste(emotions, collapse = ", ")

  # Initialize columns
  data$sentiment_score <- NA_real_
  data$confidence <- NA_real_
  data$all_emotions <- NA_character_
  data$emotion_intensity <- NA_real_

  # Resume from a partial checkpoint: a crash mid-sentiment leaves
  # sentiment_done_partial.rds behind; adopting its scored rows avoids
  # re-paying the LLM cost for entries already analyzed. Merge is BY
  # std_id, never by row position -- the analytic sample can change
  # between crash and resume (e.g. a codebook-review edit).
  .SENTIMENT_VALUE_COLS <- c("sentiment_score", "confidence",
                             "all_emotions", "emotion_intensity")
  if (!is.null(checkpoint)) {
    partial_path <- file.path(checkpoint$checkpoint_dir,
                              "sentiment_done_partial.rds")
    if (file.exists(partial_path)) {
      partial <- tryCatch(readRDS(partial_path), error = function(e) NULL)
      # Recognition guard covers names AND types: a foreign/hand-edited
      # partial with a character sentiment_score would silently flip the
      # whole column to character on adoption; a factor all_emotions
      # would adopt integer codes. Type-drifted partials start fresh.
      if (is.list(partial) && is.data.frame(partial$data) &&
          all(c("std_id", .SENTIMENT_VALUE_COLS) %in% names(partial$data)) &&
          is.numeric(partial$data$sentiment_score) &&
          is.numeric(partial$data$confidence) &&
          is.numeric(partial$data$emotion_intensity)) {
        p <- partial$data[!is.na(partial$data$sentiment_score), , drop = FALSE]
        m <- match(as.character(p$std_id), as.character(data$std_id))
        keep <- !is.na(m)
        if (any(keep)) {
          data$sentiment_score[m[keep]]   <- p$sentiment_score[keep]
          data$confidence[m[keep]]        <- p$confidence[keep]
          data$emotion_intensity[m[keep]] <- p$emotion_intensity[keep]
          # as.character: never adopt a factor's integer codes
          data$all_emotions[m[keep]]      <- as.character(p$all_emotions[keep])
          log_info(paste0("Resuming sentiment: {sum(keep)} of {nrow(data)} ",
                          "entries already scored in a partial checkpoint"))
        }
      } else {
        log_warn("Sentiment partial checkpoint unrecognized or unreadable -- starting fresh")
      }
    }
  }

  # Compute batch indices over the PENDING (unscored) rows only -- dynamic
  # or fixed. Each batch carries true row indices of `data`, so the
  # prompt's [%d] entry ids and .assign_sentiment_results stay unchanged.
  pending <- which(is.na(data$sentiment_score))
  if (length(pending) == 0L) {
    n_batches <- 0L
    batch_indices <- list()
    log_info("Sentiment analysis: all {nrow(data)} entries already scored; nothing to do")
  } else if (isTRUE(dynamic_batching) && !is.null(provider$context_window)) {
    # Budget: ~20K tokens of entry content per batch (conservative for sentiment)
    max_batch_tokens <- config$max_batch_tokens %||% 20000L
    batch_indices <- compute_dynamic_batches(
      data$std_text[pending], max_batch_tokens = max_batch_tokens,
      max_batch_size = batch_size, chars_per_entry = 800
    )
    batch_indices <- lapply(batch_indices, function(ix) pending[ix])
    n_batches <- length(batch_indices)
    log_info("Starting sentiment analysis for {length(pending)} entries ({n_batches} dynamic batches)...")
  } else {
    n_batches <- ceiling(length(pending) / batch_size)
    batch_indices <- lapply(seq_len(n_batches), function(b) {
      start <- (b - 1) * batch_size + 1
      end <- min(b * batch_size, length(pending))
      pending[start:end]
    })
    log_info("Starting sentiment analysis for {length(pending)} entries (batch size: {batch_size})...")
  }
  tic("Sentiment analysis")

  pb <- if (n_batches > 0L) {
    safe_progress_bar(
      format = "  Sentiment [:bar] :current/:total (:percent) eta: :eta",
      total = n_batches
    )
  } else {
    NULL
  }

  last_checkpoint_idx <- 0L

  for (batch_idx in seq_len(n_batches)) {
    batch_rows <- batch_indices[[batch_idx]]

    # Build batch prompt with multiple entries (with code context if available)
    entries_block <- paste(vapply(batch_rows, function(i) {
      safe_text <- gsub('"', '\\"', substr(data$std_text[i], 1, 800), fixed = TRUE)
      entry_block <- sprintf('[%d] "%s"', i, safe_text)
      # Inject code context for code-aware sentiment
      if (code_aware) {
        eid <- as.character(data$std_id[i])
        codes <- entry_codes_lookup[[eid]]
        if (!is.null(codes) && length(codes) > 0) {
          entry_block <- paste0(entry_block,
            "\n  Codes assigned: ", paste(codes, collapse = "; "))
        }
      }
      entry_block
    }, character(1)), collapse = "\n\n")

    system_prompt <- paste0(
      "You are an expert sentiment analyzer for qualitative research.\n\n",
      if (nchar(research_focus) > 0) paste0("Research focus: ", research_focus, "\n",
        "Analyze each text for sentiment and emotional content within this research domain. ",
        "Note that the emotional valence of statements may depend on the specific research ",
        "context, and that domain-specific terms may carry connotations that differ from ",
        "their everyday usage.\n\n") else "",
      if (code_aware) paste0(
        "Some entries include 'Codes assigned' showing qualitative codes from prior analysis. ",
        "Use these codes to understand which aspects of the experience the entry discusses, ",
        "and assess sentiment in the context of those specific aspects.\n\n"
      ) else "",
      # Inject reflexivity block
      config$reflexivity_block %||% "",
      "Be precise and consistent.\n\n",
      "For each entry, provide:\n",
      "- sentiment_score: -1 (very negative) to 1 (very positive)\n",
      "- confidence: 0 to 1\n",
      "- emotions: an ARRAY of ALL emotions present in the text, from: ",
      emotions_str, ", neutral. A single entry often expresses multiple ",
      "overlapping emotions (e.g., relief mixed with frustration). List every ",
      "emotion that is genuinely present, ordered from strongest to weakest. ",
      "Do NOT reduce to a single emotion -- capturing emotional complexity is ",
      "methodologically essential.\n",
      "- emotion_intensity: 0 (weak) to 1 (intense) -- overall emotional intensity\n\n",
      "Return one results entry per input entry, preserving the input id. ",
      "The response shape is enforced by the structured-output schema."
    )

    prompt <- paste0("Analyze sentiment for these entries.\n\n",
                     entries_block)

    result <- tryCatch({
      ai_result <- ai_complete_fast(provider, prompt, system_prompt,
                                     task = "sentiment",
                                     response_schema = .sentiment_schema(
                                       emotion_categories = c(emotions, "neutral")
                                     ))
      if (!is.null(audit_log)) {
        log_ai_request(audit_log, "sentiment", ai_result, response_cache,
                        batch_idx = batch_idx, batch_size = length(batch_rows))
      }
      parse_json_safely(ai_result$content, expected_key = "results")
    }, error = function(e) {
      log_warn("Sentiment batch {batch_idx} failed: {e$message}")
      NULL
    })

    # Parse and assign results
    if (!is.null(result) && !is.null(result$results)) {
      data <- .assign_sentiment_results(data, result$results, batch_rows)

      # Audit log: record sentiment assignments for this batch
      if (!is.null(audit_log)) {
        for (ri in batch_rows) {
          if (!is.na(data$sentiment_score[ri])) {
            log_ai_decision(audit_log, "sentiment", "sentiment_assignment",
                            entry_id = as.character(data$std_id[ri]),
                            sentiment_score = data$sentiment_score[ri],
                            all_emotions = data$all_emotions[ri],
                            confidence = data$confidence[ri])
          }
        }
      }
    }

    # Handle entries that didn't get results (assign NA, will be counted)
    pb$tick()

    if (batch_idx < n_batches) {
      Sys.sleep(provider$rate_limits$delay_between_batches %||% 1)
    }

    # Partial checkpoint at regular intervals
    batch_end <- max(batch_rows)
    if (!is.null(checkpoint) && (batch_end - last_checkpoint_idx) >= .PARTIAL_CHECKPOINT_INTERVAL) {
      save_partial_checkpoint(checkpoint, "sentiment_done", data, batch_end)
      last_checkpoint_idx <- batch_end
    }
  }

  toc()

  success_rate <- mean(!is.na(data$sentiment_score)) * 100
  mean_sent <- mean(data$sentiment_score, na.rm = TRUE)
  log_info("Sentiment analysis complete:")
  log_info("  Success rate: {round(success_rate, 1)}%")
  log_info("  Mean sentiment: {round(mean_sent, 3)}")

  data
}

#' Assign parsed sentiment results back to data
#' @keywords internal
.assign_sentiment_results <- function(data, results_data, batch_rows) {
  if (is.data.frame(results_data)) {
    for (j in seq_len(nrow(results_data))) {
      idx <- results_data$id[j]
      if (!is.na(idx) && idx %in% batch_rows) {
        data$sentiment_score[idx] <- pmin(1, pmax(-1, as.numeric(results_data$sentiment_score[j])))
        data$confidence[idx] <- pmin(1, pmax(0, as.numeric(results_data$confidence[j])))
        data$emotion_intensity[idx] <- pmin(1, pmax(0, as.numeric(results_data$emotion_intensity[j])))
        # Multi-label emotions: extract from the "emotions" array
        data$all_emotions[idx] <- .extract_emotions(results_data, j, is_dataframe = TRUE)
      }
    }
  } else if (is.list(results_data)) {
    for (item in results_data) {
      idx <- item$id
      if (!is.null(idx) && !is.na(idx) && idx %in% batch_rows) {
        data$sentiment_score[idx] <- pmin(1, pmax(-1, as.numeric(item$sentiment_score)))
        data$confidence[idx] <- pmin(1, pmax(0, as.numeric(item$confidence)))
        data$emotion_intensity[idx] <- pmin(1, pmax(0, as.numeric(item$emotion_intensity)))
        # Multi-label emotions: extract from the "emotions" array
        data$all_emotions[idx] <- .extract_emotions(item, NULL, is_dataframe = FALSE)
      }
    }
  }

  # Warn if some batch entries didn't receive sentiment
  n_assigned <- sum(!is.na(data$sentiment_score[batch_rows]))
  n_expected <- length(batch_rows)
  if (n_assigned < n_expected) {
    log_warn("Sentiment batch: {n_assigned}/{n_expected} entries assigned (some IDs out of range or missing)")
  }

  data
}

#' Extract multi-label emotions from AI response
#'
#' Handles the structured-outputs "emotions" array format (T1.2 schema lock:
#' .sentiment_schema requires `emotions` and forbids extra properties, so a
#' legacy `primary_emotion` field is architecturally unreachable here).
#' Returns a semicolon-separated all_emotions string.
#' @param item Data frame row or list item from parsed AI response
#' @param j Row index (only used when is_dataframe = TRUE)
#' @param is_dataframe Whether item is a data.frame (TRUE) or list (FALSE)
#' @return character scalar: semicolon-separated emotions string, or NA_character_
#' @keywords internal
.extract_emotions <- function(item, j, is_dataframe = FALSE) {
  emotions_raw <- NULL

  if (is_dataframe) {
    # "emotions" column containing JSON array or character vector
    if ("emotions" %in% names(item)) {
      val <- item$emotions[j]
      if (is.list(val)) {
        emotions_raw <- unlist(val)
      } else if (is.character(val) && !is.na(val)) {
        emotions_raw <- val
      }
    }
    # Pre-joined "all_emotions" column (semicolon-separated string)
    if ((is.null(emotions_raw) || length(emotions_raw) == 0) &&
        "all_emotions" %in% names(item)) {
      val <- as.character(item$all_emotions[j])
      if (!is.na(val) && nchar(val) > 0) {
        emotions_raw <- trimws(unlist(strsplit(val, ";\\s*")))
      }
    }
  } else {
    # List item: "emotions" as a character vector
    if (!is.null(item$emotions)) {
      emotions_raw <- unlist(item$emotions)
    }
    # Pre-joined all_emotions string
    if ((is.null(emotions_raw) || length(emotions_raw) == 0) &&
        !is.null(item$all_emotions)) {
      val <- as.character(item$all_emotions)
      if (!is.na(val) && nchar(val) > 0) {
        emotions_raw <- trimws(unlist(strsplit(val, ";\\s*")))
      }
    }
  }

  # Process the emotions array
  if (!is.null(emotions_raw) && length(emotions_raw) > 0) {
    clean <- trimws(as.character(emotions_raw))
    clean <- clean[nchar(clean) > 0 & !is.na(clean)]
    if (length(clean) > 0) {
      return(paste(clean, collapse = "; "))
    }
  }

  NA_character_
}
