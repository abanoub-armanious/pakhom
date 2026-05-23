# ==============================================================================
# Correlation Analysis -- Fixed Methodology with Multi-Label Support
# ==============================================================================
# Uses theme_membership columns (non-exclusive) instead of binary exclusive
# dummies, producing meaningful correlations instead of artifacts.
# ==============================================================================

#' Prepare data for correlation analysis
#'
#' @param data tibble with theme assignments and sentiment columns
#' @param theme_set ThemeSet object
#' @param config Correlation config section
#' @return tibble of numeric columns ready for correlation
prepare_correlation_data <- function(data, theme_set, config = list()) {
  # Require either theme_membership_* columns or emerged_themes
  has_membership <- any(grepl("^theme_membership_", names(data)))
  has_assigned <- "emerged_themes" %in% names(data)
  if (!has_membership && !has_assigned) {
    stop("[prepare_correlation_data] No theme columns found. ",
         "Expected theme_membership_* or emerged_themes columns.")
  }

  config$use_multi_label <- config$use_multi_label %||% TRUE
  config$min_theme_entries <- config$min_theme_entries %||% 5
  config$min_observations <- config$min_observations %||% 30

  log_info("Preparing correlation data...")

  # Start with substantive sentiment columns.
  #
  # 'confidence' is intentionally excluded. It's elicited from the AI in the
  # same single sentiment call as 'emotion_intensity' (see R/10_sentiment.R)
  # and is essentially a self-reported certainty score about the same
  # judgment. Empirically across the user's development runs, the
  # confidence x emotion_intensity correlation was always r >= 0.83 and was
  # reported as the #1 'large effect' finding in every report -- a structural
  # artifact of prompt design, not a finding about the data. Confidence is
  # retained in the exported sentiment_scores.csv as a per-entry diagnostic
  # but is not a substantive variable for correlation analysis.
  corr_data <- data |>
    select(any_of(c("sentiment_score", "emotion_intensity")))

  # Add theme columns
  valid_names <- theme_names(theme_set)

  if (isTRUE(config$use_multi_label)) {
    # Use multi-label membership columns (non-exclusive)
    membership_cols <- paste0("theme_membership_", make.names(valid_names))
    available_membership <- membership_cols[membership_cols %in% names(data)]

    if (length(available_membership) > 0) {
      for (col in available_membership) {
        corr_data[[col]] <- data[[col]]
      }
      log_info("Using multi-label theme membership ({length(available_membership)} themes)")
    } else {
      log_warn("No multi-label columns found, falling back to exclusive assignment")
      config$use_multi_label <- FALSE
    }
  }

  if (!isTRUE(config$use_multi_label)) {
    # Fallback: build binary membership from emerged_themes (still multi-label)
    log_warn("No multi-label columns found, building from emerged_themes")
    for (tn in valid_names) {
      safe_col <- paste0("theme_membership_", make.names(tn))
      if ("emerged_themes" %in% names(data)) {
        corr_data[[safe_col]] <- as.integer(
          !is.na(data$emerged_themes) & grepl(tn, data$emerged_themes, fixed = TRUE)
        )
      } else {
        corr_data[[safe_col]] <- 0L
      }
      entry_count <- sum(corr_data[[safe_col]])
      if (entry_count < config$min_theme_entries) {
        log_info("Excluding '{tn}' (only {entry_count} entries)")
        corr_data[[safe_col]] <- NULL
      }
    }
  }

  # Phase 50b: dataset-agnostic metric detection. The original site had
  # a hardcoded allowlist that broke novel corpora. Phase 55 consolidated
  # the detection helper into R/16_report_helpers.R so correlations +
  # the per-subtheme paper-style table share one definition of "what
  # counts as a metric." Pass the local `config$metric_columns` as the
  # explicit override; the helper falls back to auto-detect otherwise.
  metric_columns <- .detect_metric_columns(data, explicit = config$metric_columns)
  for (mc in metric_columns) {
    if (mc %in% names(data) && is.numeric(data[[mc]])) {
      corr_data[[mc]] <- data[[mc]]
    } else if (mc %in% names(data) && !is.numeric(data[[mc]])) {
      log_warn(paste0(
        "Metric column '", mc, "' is not numeric (type: ",
        class(data[[mc]])[1], "); skipping for correlation analysis."
      ))
    }
  }

  # Remove columns with insufficient data
  corr_data <- corr_data |>
    select(where(~ sum(!is.na(.x)) >= config$min_observations))

  # Remove zero/low-variance columns
  low_var <- vapply(corr_data, function(col) {
    if (!is.numeric(col)) return(TRUE)
    v <- var(col, na.rm = TRUE)
    is.na(v) || v < 0.001
  }, logical(1))

  if (any(low_var)) {
    excluded <- names(corr_data)[low_var]
    log_info("Removing {sum(low_var)} low-variance columns: {paste(excluded, collapse=', ')}")
    corr_data <- corr_data[, !low_var, drop = FALSE]
  }

  # Warn about binary variables with very small cell counts
  membership_cols <- grep("^theme_membership_", names(corr_data), value = TRUE)
  for (col in membership_cols) {
    n_positive <- sum(corr_data[[col]] == 1, na.rm = TRUE)
    if (n_positive > 0 && n_positive < config$min_theme_entries) {
      theme_label <- sub("^theme_membership_", "", col)
      log_warn("Theme '{theme_label}' has only {n_positive} member(s) -- correlations may be unreliable despite n={nrow(corr_data)}")
    }
  }

  log_info("Correlation matrix will include {ncol(corr_data)} variables")
  corr_data
}

#' Detect variable types for dynamic correlation method selection
#'
#' Classifies each column as "binary", "ordinal" (<=7 unique values), or "continuous".
#'
#' @param corr_data Numeric tibble from prepare_correlation_data()
#' @return Named character vector with types per column
#' @export
detect_variable_types <- function(corr_data) {
  vapply(names(corr_data), function(col) {
    vals <- corr_data[[col]][!is.na(corr_data[[col]])]
    unique_vals <- sort(unique(vals))
    if (length(unique_vals) <= 2 && all(unique_vals %in% c(0, 1))) {
      "binary"
    } else if (length(unique_vals) <= 7) {
      "ordinal"
    } else {
      "continuous"
    }
  }, character(1))
}

#' Select appropriate correlation method for a variable pair
#'
#' @param x Numeric vector
#' @param y Numeric vector
#' @param type_x Variable type ("binary", "ordinal", "continuous")
#' @param type_y Variable type ("binary", "ordinal", "continuous")
#' @return Character: "pearson" or "spearman"
#' @keywords internal
.select_pair_method <- function(x, y, type_x, type_y) {
  # Both binary -> Pearson (phi coefficient)
  if (type_x == "binary" && type_y == "binary") return("pearson")

  # One binary + one continuous -> Pearson (point-biserial)
  if ((type_x == "binary" && type_y == "continuous") ||
      (type_x == "continuous" && type_y == "binary")) return("pearson")

  # Any ordinal involved -> Spearman
  if (type_x == "ordinal" || type_y == "ordinal") return("spearman")

  # Both continuous -> normality test
  if (type_x == "continuous" && type_y == "continuous") {
    complete <- complete.cases(x, y)
    x_complete <- x[complete]
    y_complete <- y[complete]
    n <- length(x_complete)

    if (n < 8) return("spearman")  # too few for Shapiro-Wilk

    # Shapiro-Wilk on subsample if n > 5000 (seeded for reproducibility)
    test_n <- min(n, 5000)
    if (test_n < n) {
      withr_available <- requireNamespace("withr", quietly = TRUE)
      if (withr_available) {
        idx <- withr::with_seed(42, sample(n, test_n))
      } else {
        idx <- seq(1, n, length.out = test_n) |> round() |> unique()
      }
      x_test <- x_complete[idx]
      y_test <- y_complete[idx]
    } else {
      x_test <- x_complete
      y_test <- y_complete
    }

    x_normal <- tryCatch(shapiro.test(x_test)$p.value > 0.05, error = function(e) FALSE)
    y_normal <- tryCatch(shapiro.test(y_test)$p.value > 0.05, error = function(e) FALSE)

    if (x_normal && y_normal) "pearson" else "spearman"
  } else {
    "spearman"  # fallback
  }
}

#' Compute multiple p-value adjustments (raw, BH FDR, Bonferroni)
#'
#' Internal helper computing raw, Benjamini-Hochberg FDR, and Bonferroni
#' adjustments simultaneously over a vector of p-values. Used by
#' \code{calculate_correlations}, \code{compare_theme_groups}, and
#' \code{test_theme_cooccurrence} to provide a tiered presentation aligned
#' with the package's exploratory-analysis framing.
#'
#' Rationale: themes are inductively derived from the same data the
#' correlations are computed on, so single-method p-adjustment can mislead.
#' Reporting raw + BH + Bonferroni alongside effect sizes lets reviewers
#' judge associations under multiple inferential regimes (cf. Rothman 1990,
#' Epidemiology 1(1):43-46; ScienceDirect S0895435625000216, J Clin
#' Epidemiol 2025; PMC12359981 on intra-correlation pitfalls for BH).
#'
#' @param p_values Numeric vector of raw p-values (NAs preserved)
#' @return Named list with three numeric vectors of the same length:
#'   \code{raw}, \code{bh}, \code{bonferroni}
#' @keywords internal
.compute_p_adjustments <- function(p_values) {
  if (length(p_values) == 0) {
    return(list(raw = numeric(0), bh = numeric(0), bonferroni = numeric(0)))
  }

  non_na <- !is.na(p_values)
  bh_full <- p_values
  bonf_full <- p_values

  if (any(non_na)) {
    bh_full[non_na] <- p.adjust(p_values[non_na], method = "BH")
    bonf_full[non_na] <- p.adjust(p_values[non_na], method = "bonferroni")
  }

  list(raw = p_values, bh = bh_full, bonferroni = bonf_full)
}

#' Calculate correlation matrix with p-values
#'
#' @param corr_data Numeric tibble from prepare_correlation_data()
#' @param method "spearman" or "pearson" (used when dynamic_method is FALSE)
#' @param adjust_method P-value adjustment method (e.g., "bonferroni")
#' @param var_types Optional named character vector from detect_variable_types()
#' @param dynamic_method If TRUE, select method per variable pair based on types
#' @return CorrelationResults list
#' @export
calculate_correlations <- function(corr_data, method = "spearman",
                                    adjust_method = "bonferroni",
                                    var_types = NULL, dynamic_method = FALSE) {

  n_vars <- ncol(corr_data)
  n_obs <- nrow(corr_data)

  # Guard: nothing to correlate

  if (n_vars < 2) {
    log_warn("Fewer than 2 variables for correlation -- skipping")
    empty_matrix <- matrix(nrow = 0, ncol = 0)
    result <- list(
      correlation_matrix = empty_matrix,
      p_values = empty_matrix,
      n_observations = n_obs,
      method = method,
      adjust_method = adjust_method,
      n_variables = n_vars
    )
    class(result) <- "CorrelationResults"
    return(result)
  }

  if (isTRUE(dynamic_method) && !is.null(var_types)) {
    log_info("Calculating correlations with dynamic method selection...")

    corr_matrix <- matrix(NA_real_, nrow = n_vars, ncol = n_vars,
                          dimnames = list(names(corr_data), names(corr_data)))
    p_values <- matrix(NA_real_, nrow = n_vars, ncol = n_vars,
                       dimnames = list(names(corr_data), names(corr_data)))
    methods_used <- matrix(NA_character_, nrow = n_vars, ncol = n_vars,
                           dimnames = list(names(corr_data), names(corr_data)))

    for (i in seq_len(n_vars)) {
      corr_matrix[i, i] <- 1
      p_values[i, i] <- 1
      methods_used[i, i] <- "identity"
    }

    for (i in seq_len(n_vars - 1)) {
      for (j in (i + 1):n_vars) {
        pair_method <- .select_pair_method(
          corr_data[[i]], corr_data[[j]],
          var_types[names(corr_data)[i]], var_types[names(corr_data)[j]]
        )
        test <- tryCatch(
          cor.test(corr_data[[i]], corr_data[[j]], method = pair_method),
          error = function(e) NULL
        )
        if (!is.null(test)) {
          corr_matrix[i, j] <- test$estimate
          corr_matrix[j, i] <- test$estimate
          p_values[i, j] <- test$p.value
          p_values[j, i] <- test$p.value
        }
        methods_used[i, j] <- pair_method
        methods_used[j, i] <- pair_method
      }
    }
  } else {
    log_info("Calculating {method} correlations...")

    corr_matrix <- cor(corr_data, use = "pairwise.complete.obs", method = method)
    methods_used <- NULL

    # Compute p-values
    p_values <- matrix(NA_real_, nrow = n_vars, ncol = n_vars,
                       dimnames = list(names(corr_data), names(corr_data)))

    for (i in seq_len(n_vars - 1)) {
      for (j in (i + 1):n_vars) {
        test <- tryCatch(
          cor.test(corr_data[[i]], corr_data[[j]], method = method),
          error = function(e) NULL
        )
        if (!is.null(test)) {
          p_values[i, j] <- test$p.value
          p_values[j, i] <- test$p.value
        }
      }
    }
    diag(p_values) <- 1
  }

  # Warn about degenerate values
  if (any(is.nan(corr_matrix), na.rm = TRUE)) {
    log_warn("Correlation matrix contains NaN values -- some variable pairs may have insufficient variation")
  }

  # Adjust p-values: compute raw, BH (FDR), and Bonferroni simultaneously.
  # The 'p_adjusted' field is back-compat (the matrix corresponding to the
  # legacy 'adjust_method' parameter); the new 'p_adjustments' list field
  # exposes all three for the reframed exploratory presentation.
  upper_p <- p_values[upper.tri(p_values)]
  n_tests <- sum(!is.na(upper_p))

  # Helper: fold an adjusted upper triangle back into a symmetric matrix
  .fold_upper <- function(template_p, adjusted_clean, full_upper) {
    m <- template_p
    slot <- full_upper
    slot[!is.na(slot)] <- adjusted_clean
    m[upper.tri(m)] <- slot
    m[lower.tri(m)] <- t(m)[lower.tri(m)]
    diag(m) <- 1
    m
  }

  # Always start with raw (= p_values, with diag=1)
  raw_full <- p_values
  diag(raw_full) <- 1

  p_adjustments <- list(
    raw = raw_full,
    bh = raw_full,
    bonferroni = raw_full
  )

  if (n_tests > 0) {
    upper_p_clean <- upper_p[!is.na(upper_p)]
    p_adjustments$bh <- .fold_upper(p_values,
                                    p.adjust(upper_p_clean, method = "BH"),
                                    upper_p)
    p_adjustments$bonferroni <- .fold_upper(p_values,
                                            p.adjust(upper_p_clean, method = "bonferroni"),
                                            upper_p)
  }

  # Back-compat: 'p_adjusted' is the matrix corresponding to adjust_method.
  # Defaults to bonferroni. Aliases: 'fdr' -> 'bh', 'none' -> 'raw'.
  p_adjusted <- switch(tolower(adjust_method),
    "bonferroni" = p_adjustments$bonferroni,
    "bh"         = p_adjustments$bh,
    "fdr"        = p_adjustments$bh,
    "raw"        = p_adjustments$raw,
    "none"       = p_adjustments$raw,
    p_adjustments$bonferroni  # any other value falls back to bonferroni
  )

  result <- list(
    correlation_matrix = corr_matrix,
    p_values = p_values,
    p_adjusted = p_adjusted,
    p_adjustments = p_adjustments,
    n_observations = n_obs,
    n_tests = n_tests,
    method = if (isTRUE(dynamic_method)) "dynamic" else method,
    adjustment = adjust_method
  )

  if (!is.null(methods_used)) {
    result$methods_used <- methods_used
  }

  result
}

#' Extract significant correlations as tidy tibble
#'
#' @param results CorrelationResults from calculate_correlations()
#' @param p_threshold Significance threshold (default 0.05)
#' @param corr_data Optional numeric tibble for computing confidence intervals via cor.test
#' @return tibble: var1, var2, correlation, p_value, significant, effect_size
extract_significant <- function(results, p_threshold = 0.05, corr_data = NULL) {
  cm <- results$correlation_matrix
  pa <- results$p_adjusted
  p_adj <- results$p_adjustments  # may be NULL for legacy result objects
  method <- results$method %||% "spearman"

  methods_used <- results$methods_used  # NULL when not dynamic

  # Guard: empty correlation matrix
  if (is.null(cm) || nrow(cm) < 2) {
    log_info("No correlation pairs to extract (fewer than 2 variables)")
    return(tibble::tibble(
      var1 = character(), var2 = character(), correlation = numeric(),
      p_value = numeric(), p_raw = numeric(), p_bh = numeric(),
      p_bonferroni = numeric(), effect_size = character(),
      significant = logical(), meaningful_effect = logical(),
      method = character(), ci_lower = numeric(), ci_upper = numeric()
    ))
  }

  pairs <- list()
  for (i in seq_len(nrow(cm) - 1)) {
    for (j in (i + 1):ncol(cm)) {
      r <- cm[i, j]
      p <- pa[i, j]
      if (!is.na(r) && !is.na(p)) {
        effect <- if (abs(r) >= 0.5) "large" else if (abs(r) >= 0.3) "medium" else "small"

        # Determine method used for this pair
        pair_method <- if (!is.null(methods_used)) {
          methods_used[i, j]
        } else {
          method
        }

        # Compute confidence intervals if raw data is available
        ci <- c(NA_real_, NA_real_)
        if (!is.null(corr_data)) {
          ci <- tryCatch({
            ct <- cor.test(corr_data[[rownames(cm)[i]]], corr_data[[colnames(cm)[j]]],
                           method = pair_method, conf.level = 0.95)
            if (!is.null(ct$conf.int)) {
              c(ct$conf.int[1], ct$conf.int[2])
            } else {
              .fisher_z_ci(r, nrow(corr_data), conf_level = 0.95)
            }
          }, error = function(e) c(NA_real_, NA_real_))
        }

        # Pull raw / BH / Bonferroni p-values from the new p_adjustments field
        # (falls back to the legacy single matrix when not present)
        p_raw <- if (!is.null(p_adj$raw)) p_adj$raw[i, j] else results$p_values[i, j]
        p_bh <- if (!is.null(p_adj$bh)) p_adj$bh[i, j] else NA_real_
        p_bonf <- if (!is.null(p_adj$bonferroni)) p_adj$bonferroni[i, j] else NA_real_

        pair_entry <- list(
          var1 = rownames(cm)[i], var2 = colnames(cm)[j],
          correlation = round(r, 3),
          p_value = p,                               # back-compat (= p_adjusted)
          p_raw = p_raw,                             # raw, no adjustment
          p_bh = p_bh,                               # Benjamini-Hochberg FDR
          p_bonferroni = p_bonf,                     # Bonferroni FWER
          significant = p < p_threshold,             # back-compat (uses p_adjusted)
          meaningful_effect = abs(r) >= 0.10,        # Cohen's small-effect threshold
          effect_size = effect,
          ci_lower = round(ci[1], 3), ci_upper = round(ci[2], 3)
        )
        if (!is.null(methods_used)) {
          pair_entry$method <- pair_method
        }
        pairs[[length(pairs) + 1]] <- pair_entry
      }
    }
  }

  df <- bind_rows(pairs) |> arrange(desc(abs(.data$correlation)))
  n_sig <- sum(df$significant)
  n_meaningful <- sum(df$meaningful_effect, na.rm = TRUE)
  log_info("Extracted {nrow(df)} associations: {n_meaningful} with |r| >= 0.10, ",
           "{n_sig} significant after Bonferroni (p < {p_threshold}). ",
           "All three p-adjustments (raw, BH, Bonferroni) reported per pair.")
  df
}

#' Generate AI insights from correlation findings
#'
#' @param correlations_df Significant correlations tibble
#' @param theme_set ThemeSet object
#' @param provider AIProvider object
#' @param research_focus Research focus string
#' @param config Correlation config section (e.g. \code{config$analysis$correlations}).
#'   Used here for the reflexivity_block injected into the insight system
#'   prompt; pass an empty list to skip.
#' @param audit_log An optional AuditLog object (T1.4). When provided, the
#'   insight-generation AI call is recorded as an \code{ai_request} audit
#'   decision with full provenance.
#' @param response_cache An optional ResponseCache object (T1.4). When
#'   provided, the raw API response is written to the cache and referenced
#'   from the audit log.
#' @return Insights list
generate_insights <- function(correlations_df, theme_set, provider,
                               research_focus = "", config = list(),
                               audit_log = NULL,
                               response_cache = NULL) {
  sig <- correlations_df |> filter(significant) |> head(20)

  if (nrow(sig) == 0) {
    return(list(
      key_findings = list(),
      theoretical_implications = "No significant correlations found.",
      practical_implications = "",
      limitations = list("Small sample size or weak relationships"),
      future_directions = list()
    ))
  }

  corr_summary <- sig |>
    mutate(desc = paste0(var1, " <-> ", var2, ": r=", correlation, " (", effect_size, ")")) |>
    pull(desc) |> paste(collapse = "\n")

  system_prompt <- paste0(
    "Interpret correlation findings from a thematic analysis.\n",
    "Research focus: ", research_focus, "\n",
    config$reflexivity_block %||% "",
    "\nProvide: key_findings (an array of {insight, explanation} pairs), ",
    "theoretical_implications (string), practical_implications (string), ",
    "limitations (array of strings), and future_directions (array of strings). ",
    "Use empty arrays [] when a section has no content. The response shape ",
    "is enforced by the structured-output schema."
  )

  prompt <- paste0("Interpret these exploratory associations.\n\n",
                    corr_summary)

  tryCatch({
    ai_result <- ai_complete(provider, prompt, system_prompt,
                              task = "insight",
                              response_schema = .insight_schema())
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "insight", ai_result, response_cache,
                      n_correlations_input = nrow(sig))
    }
    parse_json_safely(ai_result$content) %||% .default_insights()
  }, error = function(e) {
    log_warn("Insight generation failed: {e$message}")
    .default_insights()
  })
}

#' Create correlation plot
#'
#' Renders a correlation heatmap for small matrices, OR a top-N
#' effect-size lollipop chart for large matrices (Phase 58 Tier 5 C-10).
#' Pre-Phase-58 the corrplot heatmap was unconditional, producing a
#' 14,280x14,280 PNG (4.8 MB, browser-illegible) on the 228-variable
#' Phase 57 saturation run. Above the \code{max_inline_vars} threshold
#' the function now switches to a ggplot2 horizontal lollipop showing
#' the top-N pairs ranked by absolute correlation, with significance
#' encoded by point color.
#'
#' @param results CorrelationResults from calculate_correlations()
#' @param output_path File path for PNG output
#' @param methodology_mode Optional character (T1.7 / AC4): when supplied,
#'   adds a footer caption identifying the methodology mode + run.
#' @param run_id Optional character: run identifier.
#' @param max_inline_vars Integer; correlation matrices with more
#'   variables than this render as a top-N lollipop instead of a
#'   heatmap. Default 30L.
create_correlation_plot <- function(results, output_path,
                                      methodology_mode = NULL,
                                      run_id = NULL,
                                      max_inline_vars = 30L) {
  log_info("Creating correlation plot...")

  cm <- results$correlation_matrix
  pa <- results$p_adjusted

  if (ncol(cm) < 2) {
    log_warn("Correlation matrix has fewer than 2 variables -- skipping plot")
    return(invisible(NULL))
  }

  # Humanize variable names for the plot
  .humanize_var <- function(x) {
    x <- gsub("theme_membership_", "", x)
    x <- gsub("[_.]", " ", x)
    # Truncate long names to keep plot readable
    ifelse(nchar(x) > 35, paste0(substr(x, 1, 32), "..."), x)
  }
  rownames(cm) <- .humanize_var(rownames(cm))
  colnames(cm) <- .humanize_var(colnames(cm))
  rownames(pa) <- .humanize_var(rownames(pa))
  colnames(pa) <- .humanize_var(colnames(pa))

  n_vars <- ncol(cm)
  top_n <- as.integer(max_inline_vars %||% 30L)
  # Tier 5 audit followup H3: cross-knob consistency. max_inline_themes
  # uses `< 1L` (a 0 / negative / NA value falls back to default 30L);
  # match that here. A user value of 1 dispatches to the lollipop on a
  # 2+ variable matrix, which renders 1 pair (still useful for the
  # degenerate case).
  if (is.na(top_n) || top_n < 1L) top_n <- 30L

  # Phase 58 Tier 5 C-10: large-matrix branch. The corrplot heatmap
  # is illegible (and crashes browsers) above ~30 variables; switch
  # to a top-N effect-size lollipop chart. Heatmap path remains for
  # small matrices where it remains the best visualization.
  if (n_vars > top_n) {
    .create_correlation_lollipop(
      cm = cm, pa = pa, output_path = output_path,
      top_n = top_n, n_total_vars = n_vars,
      methodology_mode = methodology_mode, run_id = run_id
    )
    return(invisible(NULL))
  }

  # Phase 36 (CRAN prep): corrplot moved to Suggests. Skip the plot
  # (with a friendly log line) when the package isn't installed,
  # rather than crashing -- the rest of the pipeline produces full
  # correlation results in correlations.csv regardless.
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    log_warn("corrplot package not installed; skipping correlation plot. ",
             "Install with: install.packages('corrplot')")
    return(invisible(NULL))
  }

  # Scale plot size based on number of variables
  plot_size <- max(1000, 600 + n_vars * 60)
  bottom_margin <- max(2, ceiling(max(nchar(colnames(cm))) * 0.15))

  png(output_path, width = plot_size, height = plot_size, res = 120)

  tryCatch({
    corrplot::corrplot(
      cm, method = "color", type = "upper", order = "hclust",
      tl.col = "black", tl.srt = 45, tl.cex = max(0.5, 0.9 - n_vars * 0.03),
      addCoef.col = "black", number.cex = max(0.4, 0.7 - n_vars * 0.02),
      col = grDevices::colorRampPalette(
        c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200),
      p.mat = pa, sig.level = 0.05, insig = "blank",
      title = "Correlation Matrix with Significance",
      mar = c(bottom_margin, 0, 2, 0)
    )
  }, error = function(e) {
    log_warn("Corrplot failed: {e$message}")
  })

  # T1.7 (AC4): methodology stamp footer
  if (!is.null(methodology_mode)) {
    graphics::mtext(
      methodology_plot_caption(methodology_mode, run_id),
      side = 1, line = bottom_margin - 1, cex = 0.7, col = "#7F8C8D", adj = 1,
      outer = FALSE
    )
  }

  grDevices::dev.off()
  log_info("Correlation plot saved: {output_path}")
}

#' Top-N effect-size lollipop chart for large correlation matrices
#'
#' Phase 58 Tier 5 C-10 fallback: when the variable count exceeds the
#' heatmap legibility threshold (\code{max_inline_vars}), render the
#' top-N unique pairs by \code{|r|} as a horizontal lollipop. Pairs
#' are extracted from the upper triangle of the correlation matrix
#' (each pair appears once). Significance, when available, is encoded
#' by point color (Bonferroni-adjusted \code{p < 0.05} vs not).
#'
#' @param cm Correlation matrix (rownames and colnames already humanized
#'   by the caller).
#' @param pa Adjusted-p matrix aligned to \code{cm}; NAs treated as
#'   non-significant.
#' @param output_path File path for PNG output.
#' @param top_n Integer; number of top pairs to show.
#' @param n_total_vars Integer; total variables in the underlying
#'   matrix (used in the subtitle to make the filter explicit).
#' @param methodology_mode AC4 caption.
#' @param run_id AC4 caption.
#' @keywords internal
.create_correlation_lollipop <- function(cm, pa, output_path, top_n,
                                          n_total_vars,
                                          methodology_mode = NULL,
                                          run_id = NULL) {
  n <- ncol(cm)
  if (n < 2L) return(invisible(NULL))

  # Extract upper-triangle pairs. Vectorize via upper.tri() rather
  # than nested loops so this stays O(n^2) without R's per-iteration
  # interpreter overhead.
  ut <- upper.tri(cm, diag = FALSE)
  row_idx <- row(cm)[ut]
  col_idx <- col(cm)[ut]
  r_vals  <- cm[ut]
  p_vals  <- if (!is.null(pa)) pa[ut] else rep(NA_real_, length(r_vals))

  keep <- !is.na(r_vals)
  if (!any(keep)) {
    log_warn("No usable correlation pairs for lollipop chart")
    return(invisible(NULL))
  }
  row_idx <- row_idx[keep]
  col_idx <- col_idx[keep]
  r_vals  <- r_vals[keep]
  p_vals  <- p_vals[keep]

  abs_r <- abs(r_vals)
  ord <- order(-abs_r)
  ord <- ord[seq_len(min(length(ord), as.integer(top_n)))]
  row_idx <- row_idx[ord]
  col_idx <- col_idx[ord]
  r_vals  <- r_vals[ord]
  p_vals  <- p_vals[ord]

  pair_label <- paste(rownames(cm)[row_idx], "<->", colnames(cm)[col_idx])
  signif_label <- ifelse(!is.na(p_vals) & p_vals < 0.05,
                          "p < 0.05",
                          "n.s.")

  df <- data.frame(
    label = factor(pair_label, levels = rev(pair_label)),
    r = r_vals,
    significant = factor(signif_label, levels = c("p < 0.05", "n.s.")),
    stringsAsFactors = FALSE
  )

  caption_text <- if (!is.null(methodology_mode)) {
    methodology_plot_caption(methodology_mode, run_id)
  } else NULL

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$r, y = .data$label,
                                          colour = .data$significant)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = .data$r,
                    y = .data$label, yend = .data$label),
      linewidth = 0.5, na.rm = TRUE
    ) +
    ggplot2::geom_point(size = 3, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = 0, colour = "#7F8C8D",
                         linewidth = 0.3) +
    ggplot2::scale_colour_manual(
      values = c("p < 0.05" = "#3498DB", "n.s." = "#BDC3C7"),
      drop = FALSE,
      name = "Significance"
    ) +
    ggplot2::labs(
      title    = sprintf("Top %d correlations by effect size", nrow(df)),
      subtitle = sprintf("Showing %d highest-|r| pairs out of %d variables (%d unique pairs)",
                          nrow(df), n_total_vars,
                          as.integer(n_total_vars * (n_total_vars - 1L) / 2L)),
      x        = "Correlation (r)",
      y        = NULL,
      caption  = caption_text
    ) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.major.x = ggplot2::element_line(colour = "#EAECEE",
                                                  linewidth = 0.4),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.line          = ggplot2::element_line(colour = "#2C3E50",
                                                  linewidth = 0.3),
      axis.text          = ggplot2::element_text(colour = "#7F8C8D",
                                                  size = 10),
      axis.title         = ggplot2::element_text(colour = "#2C3E50",
                                                  size = 11, face = "bold"),
      plot.title         = ggplot2::element_text(colour = "#2C3E50",
                                                  size = 15, face = "bold",
                                                  margin = ggplot2::margin(b = 6)),
      plot.subtitle      = ggplot2::element_text(colour = "#7F8C8D",
                                                  size = 10,
                                                  margin = ggplot2::margin(b = 12)),
      legend.position    = "top",
      legend.background  = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin        = ggplot2::margin(15, 15, 15, 15),
      plot.caption       = ggplot2::element_text(colour = "#7F8C8D",
                                                  size = 7, hjust = 1)
    )

  height_px <- max(500L, as.integer(28L * nrow(df) + 200L))
  grDevices::png(output_path, width = 1200L, height = height_px, res = 120)
  tryCatch(
    print(p),
    error = function(e) log_warn("Correlation lollipop failed: {e$message}")
  )
  grDevices::dev.off()
  log_info("Correlation lollipop plot saved: {output_path}")
  invisible(NULL)
}

#' Create theme co-occurrence network visualization
#'
#' Builds a network graph where nodes are themes and edges represent
#' co-occurrence strength (entries assigned to both themes). Requires
#' multi-label assignment columns (\code{theme_membership_*}).
#'
#' Phase 58 Tier 5 AH-9/V-1: at scale the unfiltered network was an
#' unreadable hairball (Phase 57 audit observed 417 themes plotted at
#' once with no legend). The \code{max_inline_themes} parameter caps
#' the visible network at the top-N most-connected themes (ranked by
#' weighted degree) and adds an inline legend explaining node size +
#' edge width encoding.
#'
#' @param data Tibble with theme_membership_* columns
#' @param theme_set ThemeSet object
#' @param output_path File path for PNG output
#' @param min_cooccurrence Minimum co-occurrence count to draw an edge (default 3)
#' @param methodology_mode Optional character (T1.7 / AC4): when supplied,
#'   adds a footer caption identifying the mode + run.
#' @param run_id Optional character: run identifier.
#' @param max_inline_themes Integer; when the graph has more nodes than
#'   this after isolated-vertex removal, the network is filtered to the
#'   top-N by weighted degree (sum of edge weights). Default 30L.
#' @return Invisible adjacency matrix, or NULL if igraph unavailable
#' @export
create_theme_network <- function(data, theme_set, output_path = "theme_network.png",
                                  min_cooccurrence = 3,
                                  methodology_mode = NULL,
                                  run_id = NULL,
                                  max_inline_themes = 30L) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    log_warn("igraph not installed -- skipping theme network plot")
    return(invisible(NULL))
  }

  valid_names <- theme_names(theme_set)
  membership_cols <- paste0("theme_membership_", make.names(valid_names))
  available <- membership_cols[membership_cols %in% names(data)]

  if (length(available) < 2) {
    log_warn("Need at least 2 theme membership columns for network plot")
    return(invisible(NULL))
  }

  # Build co-occurrence matrix
  mat <- as.matrix(data[, available])
  mat[is.na(mat)] <- 0
  cooccur <- t(mat) %*% mat

  # Clean up names (remove prefix)
  clean_names <- gsub("^theme_membership_", "", colnames(cooccur))
  clean_names <- gsub("\\.", " ", clean_names)
  colnames(cooccur) <- clean_names
  rownames(cooccur) <- clean_names

  # Zero out diagonal (self-co-occurrence)
  diag(cooccur) <- 0

  # Apply minimum threshold
  cooccur[cooccur < min_cooccurrence] <- 0

  if (sum(cooccur) == 0) {
    log_info("No theme co-occurrences above threshold ({min_cooccurrence})")
    return(invisible(cooccur))
  }

  # Build graph
  g <- igraph::graph_from_adjacency_matrix(
    cooccur, mode = "undirected", weighted = TRUE, diag = FALSE
  )

  # Remove isolated nodes
  g <- igraph::delete_vertices(g, igraph::degree(g) == 0)

  if (igraph::vcount(g) == 0) {
    log_info("All themes isolated -- no network to plot")
    return(invisible(cooccur))
  }

  # Phase 58 Tier 5 AH-9/V-1: top-N filter by weighted degree (sum of
  # incident edge weights). At 400+ themes the pre-Phase-58 plot was
  # an unreadable hairball with no legend. We keep the most-connected
  # subgraph and report what was filtered in the subtitle so the
  # reader knows this isn't the full network.
  top_n <- as.integer(max_inline_themes %||% 30L)
  if (is.na(top_n) || top_n < 1L) top_n <- 30L
  n_pre_filter <- igraph::vcount(g)
  if (n_pre_filter > top_n) {
    weighted_deg <- igraph::strength(g)
    keep_ord <- order(-weighted_deg)
    keep_vertices <- igraph::V(g)[utils::head(keep_ord, top_n)]
    g <- igraph::induced_subgraph(g, vids = keep_vertices)
    log_info(
      "theme_network.png: filtered {n_pre_filter} nodes to top-{top_n} by weighted degree"
    )
  }
  n_post_filter <- igraph::vcount(g)
  n_filtered <- n_pre_filter - n_post_filter

  if (n_post_filter == 0L) {
    log_info("All filtered themes had zero degree -- no network to plot")
    return(invisible(cooccur))
  }

  # Node size proportional to theme entry count
  node_counts <- diag(t(mat) %*% mat)
  names(node_counts) <- clean_names
  node_sizes <- node_counts[igraph::V(g)$name]
  # Defensive: a vertex name not found in node_counts would surface as
  # NA and crash the plot. Hold to 5 (minimum visible) for any missing.
  node_sizes[is.na(node_sizes)] <- 0
  max_count <- max(node_sizes, na.rm = TRUE)
  node_sizes <- if (max_count > 0) 5 + 20 * (node_sizes / max_count) else rep(5, length(node_sizes))

  # Edge width proportional to co-occurrence
  edge_weights <- igraph::E(g)$weight
  max_weight <- max(edge_weights, na.rm = TRUE)
  edge_widths <- if (max_weight > 0) 1 + 4 * (edge_weights / max_weight) else rep(1, length(edge_weights))

  # Per Phase 58 Tier 5 V-1: build a real legend explaining node size
  # + edge width encoding so the chart is interpretable without
  # external documentation. Three representative node-size + edge-
  # weight markers anchor the visual scale.
  plot_title <- if (n_filtered > 0L) {
    sprintf("Theme Co-occurrence Network (top %d of %d themes)",
             n_post_filter, n_pre_filter)
  } else {
    "Theme Co-occurrence Network"
  }

  # Phase 58 Tier 5 cross-tier audit J2: seed the Fruchterman-Reingold
  # layout RNG so identical inputs produce byte-identical PNGs across
  # runs (AC10 replay-equivalence). The pre-Phase-58 implementation
  # called layout_with_fr() with no seed control, so even on identical
  # data the network plot rendered with different node positions.
  # withr::with_seed pattern matches R/06_manuscript_learning.R:237
  # and R/14_correlations.R:185 (existing internal convention).
  fr_layout <- withr::with_seed(42L, igraph::layout_with_fr(g))

  png(output_path, width = 1400, height = 1100, res = 120)
  tryCatch({
    # Reserve space at the bottom for the legend strip.
    par(mar = c(5, 1, 3, 1))
    igraph::plot.igraph(
      g,
      layout = fr_layout,
      vertex.size = node_sizes,
      vertex.color = grDevices::adjustcolor("#4477AA", alpha.f = 0.7),
      vertex.frame.color = "#2255AA",
      vertex.label.cex = 0.7,
      vertex.label.color = "black",
      edge.width = edge_widths,
      edge.color = grDevices::adjustcolor("#999999", alpha.f = 0.6),
      edge.label = edge_weights,
      edge.label.cex = 0.6,
      main = plot_title
    )

    # Legend: top-left, transparent background. Explains the visual
    # encoding directly on the chart (Phase 58 Tier 5 V-1 -- pre-Phase-58
    # the chart had no legend at all).
    legend_lines <- c(
      "Node size: # entries in theme",
      "Edge width: co-occurrence count",
      sprintf("Edge labels: # entries (>= %d shown)", min_cooccurrence)
    )
    if (n_filtered > 0L) {
      legend_lines <- c(legend_lines,
                         sprintf("Filtered: %d themes by weighted degree",
                                  n_filtered))
    }
    graphics::legend(
      "topleft", legend = legend_lines,
      bty = "o", bg = grDevices::adjustcolor("white", alpha.f = 0.85),
      cex = 0.75, text.col = "#2C3E50",
      box.col = "#BDC3C7", box.lwd = 0.5
    )
  }, error = function(e) {
    log_warn("Theme network plot failed: {e$message}")
  })

  # T1.7 (AC4): methodology stamp footer
  if (!is.null(methodology_mode)) {
    graphics::mtext(
      methodology_plot_caption(methodology_mode, run_id),
      side = 1, line = 3.5, cex = 0.7, col = "#7F8C8D", adj = 1
    )
  }

  grDevices::dev.off()

  log_info("Theme network plot saved: {output_path}")
  invisible(cooccur)
}

#' Compute confidence interval for a correlation using Fisher z-transformation
#'
#' Approximates a confidence interval when cor.test does not provide one
#' (i.e., for Spearman and Kendall correlations). Uses the Fisher
#' z-transformation: z = atanh(r), SE = 1/sqrt(n-3), then back-transforms.
#'
#' @param r Observed correlation coefficient
#' @param n Number of observations
#' @param conf_level Confidence level (default 0.95)
#' @return Numeric vector of length 2: c(lower, upper), or c(NA, NA) if n < 4
#' @keywords internal
.fisher_z_ci <- function(r, n, conf_level = 0.95) {
  if (n < 4 || is.na(r) || abs(r) >= 1) return(c(NA_real_, NA_real_))
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  alpha <- 1 - conf_level
  z_crit <- qnorm(1 - alpha / 2)
  lower <- tanh(z - z_crit * se)
  upper <- tanh(z + z_crit * se)
  c(round(lower, 3), round(upper, 3))
}

.default_insights <- function() {
  list(
    key_findings = list(),
    theoretical_implications = "Analysis pending.",
    practical_implications = "",
    limitations = list(),
    future_directions = list()
  )
}

# ==============================================================================
# Theme Group Comparisons (Mann-Whitney U)
# ==============================================================================

#' Compare continuous variables across theme groups using Mann-Whitney U tests
#'
#' For each binary theme membership column, tests whether sentiment, emotion
#' intensity, and confidence differ between theme members and non-members.
#'
#' @param data Tibble with theme_membership_* and sentiment columns
#' @param theme_set ThemeSet object
#' @param config Correlation config section
#' @return Tibble with test results per theme-variable pair
#' @export
compare_theme_groups <- function(data, theme_set, config = list()) {

  min_group <- config$min_theme_entries %||% 5
  # 'confidence' excluded for the same reason as in prepare_correlation_data:
  # it co-varies with emotion_intensity by design (same single AI sentiment
  # call), so any 'confidence differs across themes' result is contaminated
  # by the underlying emotion_intensity difference.
  continuous_vars <- c("sentiment_score", "emotion_intensity")
  continuous_vars <- continuous_vars[continuous_vars %in% names(data)]

  if (length(continuous_vars) == 0) {
    log_warn("No continuous variables found for theme group comparison")
    return(tibble::tibble())
  }

  membership_cols <- grep("^theme_membership_", names(data), value = TRUE)
  if (length(membership_cols) == 0) {
    log_warn("No theme membership columns found")
    return(tibble::tibble())
  }

  results <- list()
  valid_names <- theme_names(theme_set)

  for (mcol in membership_cols) {
    theme_label <- sub("^theme_membership_", "", mcol)
    theme_label <- gsub("\\.", " ", theme_label)

    members <- data[[mcol]] == 1
    non_members <- data[[mcol]] == 0
    n_members <- sum(members, na.rm = TRUE)
    n_non_members <- sum(non_members, na.rm = TRUE)

    if (n_members < min_group || n_non_members < min_group) next

    for (cv in continuous_vars) {
      vals_members <- data[[cv]][members]
      vals_non <- data[[cv]][non_members]
      vals_members <- vals_members[!is.na(vals_members)]
      vals_non <- vals_non[!is.na(vals_non)]

      if (length(vals_members) < min_group || length(vals_non) < min_group) next

      test_result <- tryCatch({
        wt <- wilcox.test(vals_members, vals_non, exact = FALSE)
        n_total <- length(vals_members) + length(vals_non)
        z_val <- qnorm(wt$p.value / 2)
        effect_r <- abs(z_val) / sqrt(n_total)

        mean_m <- round(mean(vals_members), 3)
        mean_n <- round(mean(vals_non), 3)
        direction <- if (mean_m > mean_n) "Higher in theme" else "Lower in theme"

        list(
          theme = theme_label,
          variable = gsub("_", " ", cv),
          mean_members = mean_m,
          mean_non_members = mean_n,
          w_statistic = round(wt$statistic, 1),
          p_value = wt$p.value,
          effect_r = round(effect_r, 3),
          direction = direction,
          n_members = n_members,
          n_non_members = n_non_members
        )
      }, error = function(e) NULL)

      if (!is.null(test_result)) {
        results[[length(results) + 1]] <- test_result
      }
    }
  }

  if (length(results) == 0) return(tibble::tibble())

  df <- tibble::tibble(
    theme = vapply(results, `[[`, character(1), "theme"),
    variable = vapply(results, `[[`, character(1), "variable"),
    mean_members = vapply(results, `[[`, numeric(1), "mean_members"),
    mean_non_members = vapply(results, `[[`, numeric(1), "mean_non_members"),
    w_statistic = vapply(results, `[[`, numeric(1), "w_statistic"),
    p_value = vapply(results, `[[`, numeric(1), "p_value"),
    effect_r = vapply(results, `[[`, numeric(1), "effect_r"),
    direction = vapply(results, `[[`, character(1), "direction")
  )

  # Multi-method p-value adjustments (raw + BH FDR + Bonferroni FWER).
  # 'p_adjusted' / 'significant' kept for back-compat (= Bonferroni at α=0.05);
  # 'meaningful_effect' is the new effect-size-based exploratory flag.
  adjustments <- .compute_p_adjustments(df$p_value)
  df$p_raw <- adjustments$raw
  df$p_bh <- adjustments$bh
  df$p_bonferroni <- adjustments$bonferroni
  df$p_adjusted <- df$p_bonferroni                   # back-compat
  df$significant <- df$p_adjusted < 0.05             # back-compat
  df$meaningful_effect <- df$effect_r >= 0.10        # Cohen's small-effect threshold
  df <- df[order(-abs(df$effect_r)), ]               # sort by effect size

  log_info("Theme group comparisons: {nrow(df)} tests; ",
           "{sum(df$meaningful_effect, na.rm = TRUE)} with effect_r >= 0.10, ",
           "{sum(df$significant)} significant after Bonferroni (p < 0.05). ",
           "All three p-adjustments (raw, BH, Bonferroni) reported per test.")
  df
}

# ==============================================================================
# Theme Co-occurrence (Chi-Square)
# ==============================================================================

#' Test theme co-occurrence patterns with chi-square tests of independence
#'
#' For each pair of themes, tests whether co-occurrence is significantly different
#' from expected by chance.
#'
#' @param data Tibble with theme_membership_* columns
#' @param theme_set ThemeSet object
#' @param min_expected Minimum expected cell count for chi-square (default 5)
#' @return Tibble with co-occurrence test results
#' @export
test_theme_cooccurrence <- function(data, theme_set, min_expected = 5) {

  membership_cols <- grep("^theme_membership_", names(data), value = TRUE)
  if (length(membership_cols) < 2) {
    log_warn("Need at least 2 themes for co-occurrence analysis")
    return(tibble::tibble())
  }

  n_total <- nrow(data)
  results <- list()

  pairs <- utils::combn(membership_cols, 2, simplify = FALSE)

  for (pair in pairs) {
    col1 <- pair[1]
    col2 <- pair[2]
    label1 <- gsub("\\.", " ", sub("^theme_membership_", "", col1))
    label2 <- gsub("\\.", " ", sub("^theme_membership_", "", col2))

    a <- data[[col1]]
    b <- data[[col2]]
    valid <- !is.na(a) & !is.na(b)
    a <- a[valid]
    b <- b[valid]
    n <- length(a)
    if (n < 10) next

    # 2x2 contingency table
    ct <- table(factor(a, levels = c(0, 1)), factor(b, levels = c(0, 1)))
    observed_both <- ct[2, 2]
    expected_both <- round(sum(a == 1) * sum(b == 1) / n, 1)

    # Check expected cell counts
    expected_mat <- outer(rowSums(ct), colSums(ct)) / n
    use_fisher <- any(expected_mat < min_expected)

    test_result <- tryCatch({
      if (use_fisher) {
        ft <- fisher.test(ct)
        list(stat = NA_real_, p_value = ft$p.value, method = "Fisher")
      } else {
        chi <- chisq.test(ct, correct = FALSE)
        list(stat = round(chi$statistic, 3), p_value = chi$p.value, method = "Chi-square")
      }
    }, error = function(e) NULL)

    if (is.null(test_result)) next

    # Cramer's V
    cramers_v <- if (!is.na(test_result$stat)) {
      round(sqrt(test_result$stat / n), 3)
    } else {
      NA_real_
    }

    results[[length(results) + 1]] <- list(
      theme1 = label1,
      theme2 = label2,
      observed_both = as.integer(observed_both),
      expected_both = expected_both,
      statistic = test_result$stat %||% NA_real_,
      p_value = test_result$p_value,
      cramers_v = cramers_v,
      method = test_result$method,
      n = n
    )
  }

  if (length(results) == 0) return(tibble::tibble())

  df <- tibble::tibble(
    theme1 = vapply(results, `[[`, character(1), "theme1"),
    theme2 = vapply(results, `[[`, character(1), "theme2"),
    observed_both = vapply(results, `[[`, integer(1), "observed_both"),
    expected_both = vapply(results, `[[`, numeric(1), "expected_both"),
    statistic = vapply(results, `[[`, numeric(1), "statistic"),
    p_value = vapply(results, `[[`, numeric(1), "p_value"),
    cramers_v = vapply(results, `[[`, numeric(1), "cramers_v"),
    method = vapply(results, `[[`, character(1), "method")
  )

  # Multi-method p-value adjustments (raw + BH FDR + Bonferroni FWER).
  # 'p_adjusted' / 'significant' kept for back-compat; 'meaningful_effect'
  # uses |Cramer's V| >= 0.10 as the exploratory effect-size threshold.
  adjustments <- .compute_p_adjustments(df$p_value)
  df$p_raw <- adjustments$raw
  df$p_bh <- adjustments$bh
  df$p_bonferroni <- adjustments$bonferroni
  df$p_adjusted <- df$p_bonferroni                   # back-compat
  df$significant <- df$p_adjusted < 0.05             # back-compat
  df$meaningful_effect <- abs(df$cramers_v) >= 0.10  # Cohen's small-effect threshold
  df <- df[order(-abs(df$cramers_v)), ]              # sort by effect size

  log_info("Theme co-occurrence: {nrow(df)} pairs; ",
           "{sum(df$meaningful_effect, na.rm = TRUE)} with |Cramer's V| >= 0.10, ",
           "{sum(df$significant)} significant after Bonferroni (p < 0.05). ",
           "All three p-adjustments (raw, BH, Bonferroni) reported per pair.")
  df
}
