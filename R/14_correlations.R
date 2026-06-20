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
  # C4 (dataset-agnostic): include EVERY numeric
  # metric column from the corpus in the correlation matrix, not just
  # the two pakhom-engineered sentiment columns. .detect_metric_columns()
  # honors explicit config$data$column_mappings$metric_columns first,
  # then falls back to auto-detect (any numeric column not in the
  # internal exclusion list and not theme_membership_*). A clinical
  # corpus with `age` and `tenure_months` now correlates those against
  # every theme; a Reddit corpus with `score` + `num_comments`
  # correlates those; etc. See pakhom/R/16_report_helpers.R::.detect_metric_columns.
  metric_cols <- .detect_metric_columns(data, config = config)
  base_cols <- intersect(c("sentiment_score", "emotion_intensity"), names(data))
  corr_data <- data |>
    select(any_of(unique(c(base_cols, metric_cols))))

  # Add theme columns
  valid_names <- theme_names(theme_set)

  if (isTRUE(config$use_multi_label)) {
    # Use multi-label membership columns (non-exclusive)
    membership_cols <- paste0("theme_membership_", make.names(valid_names))
    available_membership <- membership_cols[membership_cols %in% names(data)]

    if (length(available_membership) > 0) {
      # apply min_theme_entries filter consistently
      # with compare_theme_groups + test_theme_cooccurrence so the three
      # statistical layers all denominate against the same theme cohort.
      # An earlier multi-label path admitted ANY membership column
      # (frequency filter only fired on the emerged-themes fallback path),
      # so the correlation matrix and theme-group / co-occurrence tibbles
      # reported counts over slightly different denominators on the
      # large saturation run. The fallback path (lines 62-79 below)
      # already applies the same filter.
      kept <- 0L
      excluded <- 0L
      for (col in available_membership) {
        n_pos <- sum(data[[col]] == 1L, na.rm = TRUE)
        if (n_pos >= config$min_theme_entries) {
          corr_data[[col]] <- data[[col]]
          kept <- kept + 1L
        } else {
          excluded <- excluded + 1L
        }
      }
      log_info(paste0(
        "Using multi-label theme membership: {kept} themes admitted ",
        "(>= {config$min_theme_entries} members); ",
        "{excluded} excluded for low frequency."
      ))
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
          .entry_in_theme(data$emerged_themes, tn)
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

  # Dataset-agnostic metric detection. The original site had
  # a hardcoded allowlist that broke novel corpora. A later refactor consolidated
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
#' Classifies each column as "binary", "ordinal", or "continuous". The
#' ordinal threshold defaults to \code{<=21 unique values}, which covers
#' (a) the VADER-shaped sentiment scale \code{[-1, 1]} quantized at 0.1
#' (21 distinct levels), (b) the Likert-style 5/7/9/11-point scales
#' common in survey research, and (c) AI-elicited intensity / confidence
#' scores on a small integer grid. The earlier threshold of 7
#' silently classified VADER sentiment as \emph{continuous} on the
#' large run, which then dispatched to Pearson (point-biserial for
#' binary x quantized-sentiment pairs) -- methodologically wrong for an
#' ordinal support. Spearman is correct when either variable is
#' rank-orderable but not interval-scaled .
#'
#' @param corr_data Numeric tibble from prepare_correlation_data()
#' @param ordinal_max Integer; upper bound on distinct values for the
#'   ordinal classification. Default 21L. Datasets with finer-grained
#'   ordinal scales (e.g. 0-50 Likert) can override.
#' @return Named character vector with types per column
#' @export
detect_variable_types <- function(corr_data, ordinal_max = 21L) {
  vapply(names(corr_data), function(col) {
    vals <- corr_data[[col]][!is.na(corr_data[[col]])]
    unique_vals <- sort(unique(vals))
    if (length(unique_vals) <= 2 && all(unique_vals %in% c(0, 1))) {
      "binary"
    } else if (length(unique_vals) <= as.integer(ordinal_max)) {
      "ordinal"
    } else {
      "continuous"
    }
  }, character(1))
}

#' Select appropriate correlation method for a variable pair
#'
#' Binary x ordinal pairs now route
#' through Spearman. The ordinal side is rank-orderable but not
#' interval-scaled, so the interval assumption behind point-biserial
#' (Pearson with a binary variable) does not hold; the reported
#' coefficient is Spearman's rho computed on (mid)ranks and is labeled
#' as such. They previously routed to Pearson via the
#' general binary+non-binary rule, which produces point-biserial -- a
#' coefficient that assumes the non-binary side is interval-scaled. For
#' AI-elicited sentiment / intensity / Likert scores the support is
#' genuinely ordinal, not interval, and point-biserial is
#' methodologically suspect. Binary x continuous remains Pearson
#' (point-biserial is appropriate when the support genuinely is
#' continuous).
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

  # binary + ordinal -> Spearman (rho on midranks; ordinal side is
  # not interval-scaled).
  if ((type_x == "binary" && type_y == "ordinal") ||
      (type_x == "ordinal" && type_y == "binary")) return("spearman")

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
      # .with_seed yields the same seeded random subsample with or without withr
      # (its fallback no longer switches to a different deterministic stride).
      idx <- .with_seed(42, sample(n, test_n))
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

  # Guard: empty correlation matrix (fewer than 2 variables). Shares the single
  # empty-result constructor with the all-NA-pairwise guard below, so every
  # empty path matches the populated path's schema exactly (one source of truth).
  if (is.null(cm) || nrow(cm) < 2) {
    log_info("No correlation pairs to extract (fewer than 2 variables)")
    return(.empty_significant_correlations(include_method = !is.null(methods_used)))
  }

  pairs <- list()
  for (i in seq_len(nrow(cm) - 1)) {
    for (j in (i + 1):ncol(cm)) {
      r <- cm[i, j]
      p <- pa[i, j]
      if (!is.na(r) && !is.na(p)) {
        # add "negligible" tier below Cohen's
        # small-effect threshold (|r| < 0.10). An earlier version
        # classifier labeled trivially small effects (e.g. |r| = 0.04
        # with N > 5,000 passing Bonferroni) as "small", misleading
        # readers about substantive magnitude.
        effect <- if (abs(r) >= 0.5) "large"
                  else if (abs(r) >= 0.3) "medium"
                  else if (abs(r) >= 0.10) "small"
                  else "negligible"

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
            xj <- corr_data[[rownames(cm)[i]]]
            yj <- corr_data[[colnames(cm)[j]]]
            ct <- cor.test(xj, yj, method = pair_method, conf.level = 0.95)
            if (!is.null(ct$conf.int)) {
              c(ct$conf.int[1], ct$conf.int[2])
            } else {
              # cor.test returns no CI for Spearman/Kendall -> Fisher-z fallback,
              # using the PAIRWISE-complete n (the matrix is pairwise.complete.obs,
              # so the full row count would overstate n) and the method-appropriate
              # standard error.
              n_pair <- sum(stats::complete.cases(xj, yj))
              .fisher_z_ci(r, n_pair, conf_level = 0.95, method = pair_method)
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

  # Guard the degenerate all-NA-pairwise case: if every pairwise correlation
  # was NA (e.g. a constant / zero-variance metric in every theme), `pairs` is
  # empty and bind_rows() yields a 0-column tibble, so arrange(correlation)
  # errors BEFORE the flag-column branch below. Return a well-formed,
  # full-schema 0-row result so the CSV export + every downstream consumer
  # (and compare_runs) see a consistent shape on degenerate corpora.
  # (Complements e613841, which guards the no-correlations case downstream in
  # the report renderer; this guards the producer itself, which is not
  # tryCatch-wrapped at its pipeline call site.)
  if (length(pairs) == 0L) {
    return(.empty_significant_correlations(include_method = !is.null(methods_used)))
  }
  df <- bind_rows(pairs) |> arrange(desc(abs(.data$correlation)))

  # FLAG analyst-internal / circular correlation pairs as
  # `excluded_from_findings` rather than DROPPING them. Two artifact classes:
  #  (1) within-AI-sentiment-instrument -- sentiment_score / emotion_intensity /
  #      confidence are elicited in ONE AI sentiment call (R/10_sentiment.R), so
  #      their MUTUAL correlation is a prompt-coupling artifact (was 045cde4).
  #  (2) affect-instrument x theme_membership_* -- both are the AI analyst's OWN
  #      codings of the SAME text, so the correlation measures internal coding
  #      consistency, fully circular when the theme is affect-defined (this was
  #      the #1 "Key Finding" on a real run while every INDEPENDENT-measure pair
  #      was non-significant; the per-theme sentiment_tendency already reports
  #      affect-by-theme descriptively, so nothing substantive is hidden).
  # KEEP these rows (with their real correlation + p-values + an
  # exclusion_reason) so the exported correlations.csv is a COMPLETE, auditable
  # matrix a reviewer can inspect -- and zero BOTH finding-flags (significant,
  # meaningful_effect) for them so every downstream findings/insights/section
  # consumer (which all key off those flags) excludes them with NO consumer
  # changes and no leak. Substantive pairs (engagement-metadata x theme,
  # metric x metric, theme co-occurrence, affect x metadata) are untouched.
  if (nrow(df) > 0L) {
    fam <- c("sentiment_score", "emotion_intensity", "confidence")
    .is_tm <- function(v) grepl("^theme_membership_", v)
    within_family  <- df$var1 %in% fam & df$var2 %in% fam
    affect_x_theme <- (df$var1 %in% fam & .is_tm(df$var2)) |
                      (df$var2 %in% fam & .is_tm(df$var1))
    df$exclusion_reason <- ifelse(within_family,  "within_affect_instrument",
                            ifelse(affect_x_theme, "affect_x_theme_membership",
                                   NA_character_))
    df$excluded_from_findings <- !is.na(df$exclusion_reason)
    if (any(df$excluded_from_findings)) {
      df$significant[df$excluded_from_findings]       <- FALSE
      df$meaningful_effect[df$excluded_from_findings] <- FALSE
      log_info(sprintf(
        "Flagged %d analyst-internal/circular correlation pair(s) excluded_from_findings (KEPT in the exported matrix with their p-values; removed from significant findings): %s",
        sum(df$excluded_from_findings),
        paste(sprintf("%s~%s[%s]", df$var1[df$excluded_from_findings],
                      df$var2[df$excluded_from_findings],
                      df$exclusion_reason[df$excluded_from_findings]), collapse = ", ")))
    }
  }
  # Re-scope the multiple-comparison family to the
  # SUBSTANTIVE (non-excluded) pairs. calculate_correlations runs the
  # BH/Bonferroni adjustment over the FULL matrix, BEFORE the circular /
  # within-instrument pairs are flagged above; those artifact pairs carry
  # tiny p-values by construction and would contaminate the family (under BH,
  # inflating the count of small p-values and pulling genuine pairs below the
  # FDR threshold; under Bonferroni, over-penalizing every pair). Recompute
  # both adjustments over the kept pairs' RAW p-values so the reported adjusted
  # p-values + significance reflect the correct family. Excluded pairs keep
  # their real raw p in the exported matrix but get NA adjusted p (they belong
  # to no findings family). When there are no excluded pairs the family was
  # already correct, so this branch is a no-op (byte-identical) for those runs.
  if (nrow(df) > 0L && "excluded_from_findings" %in% names(df) &&
      "p_raw" %in% names(df) && any(df$excluded_from_findings)) {
    keep <- !df$excluded_from_findings & !is.na(df$p_raw)
    df$p_bh         <- NA_real_
    df$p_bonferroni <- NA_real_
    if (any(keep)) {
      df$p_bh[keep]         <- stats::p.adjust(df$p_raw[keep], method = "BH")
      df$p_bonferroni[keep] <- stats::p.adjust(df$p_raw[keep], method = "bonferroni")
    }
    # Recompute `significant` from the re-scoped adjustment that p_adjusted
    # represents (Bonferroni by default; detected by object identity so a
    # configured adjust_method = BH / raw is honored). Excluded pairs stay FALSE.
    sel <- if (!is.null(p_adj)) {
      if (!is.null(p_adj$bh) && identical(pa, p_adj$bh)) "bh"
      else if (!is.null(p_adj$raw) && identical(pa, p_adj$raw)) "raw"
      else "bonferroni"
    } else "bonferroni"
    sig_p <- switch(sel, bh = df$p_bh, raw = df$p_raw, df$p_bonferroni)
    df$significant <- keep & !is.na(sig_p) & (sig_p < p_threshold)
    df$p_value <- sig_p  # back-compat alias (= the adjusted p used for significance)
  }
  n_sig <- sum(df$significant)
  n_meaningful <- sum(df$meaningful_effect, na.rm = TRUE)
  log_info("Extracted {nrow(df)} associations: {n_meaningful} with |r| >= 0.10, ",
           "{n_sig} significant after Bonferroni (p < {p_threshold}). ",
           "All three p-adjustments (raw, BH, Bonferroni) reported per pair.")
  df
}

#' Empty, full-schema significant-correlations tibble
#'
#' Returned by [extract_significant()] when no pairwise correlation was
#' computable (every off-diagonal r was NA). Carries every column the
#' populated path emits -- including the `exclusion_reason`
#' and `excluded_from_findings` flags -- so the CSV export, the report
#' consumers, and compare_runs see a consistent schema on degenerate corpora.
#'
#' @param include_method Logical; whether to include the per-pair `method`
#'   column (present only when the caller computed per-pair methods).
#' @return A 0-row tibble with the full extract_significant column set.
#' @keywords internal
.empty_significant_correlations <- function(include_method = FALSE) {
  base <- tibble::tibble(
    var1 = character(0), var2 = character(0),
    correlation = numeric(0),
    p_value = numeric(0), p_raw = numeric(0),
    p_bh = numeric(0), p_bonferroni = numeric(0),
    significant = logical(0), meaningful_effect = logical(0),
    effect_size = character(0),
    ci_lower = numeric(0), ci_upper = numeric(0)
  )
  if (isTRUE(include_method)) base$method <- character(0)
  base$exclusion_reason       <- character(0)
  base$excluded_from_findings <- logical(0)
  base
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
#' @param audit_log An optional AuditLog object. When provided, the
#'   insight-generation AI call is recorded as an \code{ai_request} audit
#'   decision with full provenance.
#' @param response_cache An optional ResponseCache object. When
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
    "\nThese are EXPLORATORY, cross-sectional associations -- hypothesis-generating, ",
    "NOT causal or confirmatory. Do NOT use causal or directional language ",
    "('drives', 'causes', 'leads to', 'increases X', 'because'); use associative, ",
    "tentative phrasing ('is associated with', 'co-occurs with', 'may warrant further ",
    "study'). Frame implications as hypotheses to test, not conclusions.\n",
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
#' effect-size lollipop chart for large matrices.
#' An earlier corrplot heatmap was unconditional, producing a
#' 14,280x14,280 PNG (4.8 MB, browser-illegible) on a 228-variable
#' run. Above the \code{max_inline_vars} threshold
#' the function now switches to a ggplot2 horizontal lollipop showing
#' the top-N pairs ranked by absolute correlation, with significance
#' encoded by point color.
#'
#' @param results CorrelationResults from calculate_correlations()
#' @param output_path File path for PNG output
#' @param methodology_mode Optional character. When supplied,
#'   adds a footer caption identifying the methodology mode + run.
#' @param run_id Optional character: run identifier.
#' @param max_inline_vars Integer; correlation matrices with more
#'   variables than this render as a top-N lollipop instead of a
#'   heatmap. Default 30L.
#' @param excluded_pairs Optional data frame (the [extract_significant()]
#'   result) carrying `var1`, `var2`, and `excluded_from_findings`. Pairs
#'   flagged TRUE (analyst-internal / circular, e.g. an affect instrument x
#'   a theme-membership column) are shown but never presented as a
#'   significant finding -- the heatmap blanks them (and discloses the count)
#'   and the lollipop marks them "excluded (circular)". NULL (default)
#'   reproduces the pre-#2b plot exactly.
create_correlation_plot <- function(results, output_path,
                                      methodology_mode = NULL,
                                      run_id = NULL,
                                      max_inline_vars = 30L,
                                      excluded_pairs = NULL) {
  log_info("Creating correlation plot...")

  cm <- results$correlation_matrix
  pa <- results$p_adjusted

  if (ncol(cm) < 2) {
    log_warn("Correlation matrix has fewer than 2 variables -- skipping plot")
    return(invisible(NULL))
  }

  # For consistency: flag analyst-internal / circular pairs
  # (excluded_from_findings upstream) so neither the heatmap nor the lollipop
  # presents them as a significant finding. Built on the ORIGINAL variable
  # names but index-aligned to cm/pa, so it survives the name-humanizing below.
  orig_var_names <- rownames(cm) %||% colnames(cm)
  excluded_mat <- .build_excluded_pair_matrix(orig_var_names, excluded_pairs)
  n_excluded_shown <- sum(excluded_mat[upper.tri(excluded_mat)])

  # M2 consistency: overlay the re-scoped (family-corrected) adjusted p-values
  # so the heatmap/lollipop significance matches the report correlation table.
  # Uses the ORIGINAL variable names (the df's var names), before the humanizing
  # below; a no-op (byte-identical) when nothing was excluded.
  pa <- .rescope_plot_pvalues(pa, orig_var_names, excluded_pairs)

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
  # Cross-knob consistency. max_inline_themes
  # uses `< 1L` (a 0 / negative / NA value falls back to default 30L);
  # match that here. A user value of 1 dispatches to the lollipop on a
  # 2+ variable matrix, which renders 1 pair (still useful for the
  # degenerate case).
  if (is.na(top_n) || top_n < 1L) top_n <- 30L

  # large-matrix branch. The corrplot heatmap
  # is illegible (and crashes browsers) above ~30 variables; switch
  # to a top-N effect-size lollipop chart. Heatmap path remains for
  # small matrices where it remains the best visualization.
  if (n_vars > top_n) {
    .create_correlation_lollipop(
      cm = cm, pa = pa, output_path = output_path,
      top_n = top_n, n_total_vars = n_vars,
      methodology_mode = methodology_mode, run_id = run_id,
      excluded_mat = excluded_mat
    )
    return(invisible(NULL))
  }

  # For CRAN, corrplot moved to Suggests. Skip the plot
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
  # Guarantee the graphics device closes even if a downstream call errors. A
  # failed *cosmetic* correlation plot must never leak a device or abort a run
  # whose coding + theming are already done.
  on.exit(if (grDevices::dev.cur() > 1L) grDevices::dev.off(), add = TRUE)

  # #2b consistency: blank excluded (analyst-internal / circular) pairs so they
  # are not shown as significant. Their colour + coefficient would otherwise
  # read as a finding; they remain in correlations.csv with their real values
  # and exclusion_reason.
  pa_plot <- .mask_excluded_pvalues(pa, excluded_mat)

  # order = "hclust" runs dist()/hclust() on the matrix, which dies with
  # "NA/NaN/Inf in foreign function call" on any non-finite cell. Real corpora
  # produce zero-variance code pairs -> NA correlations, so fall back to the
  # unordered layout when the matrix isn't all-finite; the plot still renders.
  cm_order <- if (all(is.finite(cm))) "hclust" else "original"

  plot_drawn <- FALSE
  tryCatch({
    corrplot::corrplot(
      cm, method = "color", type = "upper", order = cm_order,
      tl.col = "black", tl.srt = 45, tl.cex = max(0.5, 0.9 - n_vars * 0.03),
      addCoef.col = "black", number.cex = max(0.4, 0.7 - n_vars * 0.02),
      col = grDevices::colorRampPalette(
        c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200),
      p.mat = pa_plot, sig.level = 0.05, insig = "blank",
      title = "Correlation Matrix with Significance",
      mar = c(bottom_margin, 0, 2, 0)
    )
    plot_drawn <- TRUE
  }, error = function(e) {
    log_warn("Corrplot failed: {e$message}")
  })

  # The mtext footers require an ACTIVE high-level plot; calling mtext on an
  # empty device throws "plot.new has not been called yet", which (uncaught)
  # previously aborted the whole pipeline after a failed corrplot. Only add the
  # captions when corrplot actually drew a plot.
  if (plot_drawn && !is.null(methodology_mode)) {
    # T1.7 (AC4): methodology stamp footer
    graphics::mtext(
      methodology_plot_caption(methodology_mode, run_id),
      side = 1, line = bottom_margin - 1, cex = 0.7, col = "#7F8C8D", adj = 1,
      outer = FALSE
    )
  }

  # #2b consistency: disclose the blanked circular pairs (kept in the CSV) so
  # the heatmap's omission is transparent rather than silent.
  if (plot_drawn && n_excluded_shown > 0L) {
    graphics::mtext(
      sprintf("%d analyst-internal/circular pair(s) excluded from findings (shown in correlations.csv with exclusion_reason).",
              n_excluded_shown),
      side = 1, line = bottom_margin, cex = 0.6, col = "#E67E22", adj = 0,
      outer = FALSE
    )
  }

  grDevices::dev.off()
  if (plot_drawn) {
    log_info("Correlation plot saved: {output_path}")
  } else {
    log_warn("Correlation plot not rendered (corrplot failed); ",
             "correlations.csv still has the full results.")
  }
}

#' Top-N effect-size lollipop chart for large correlation matrices
#'
#' Fallback: when the variable count exceeds the
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
#' @param excluded_mat Logical matrix from [.build_excluded_pair_matrix()]
#'   marking analyst-internal / circular pairs (or NULL). Such pairs are
#'   labelled "excluded (circular)" instead of being shown as a finding.
#' @keywords internal
.create_correlation_lollipop <- function(cm, pa, output_path, top_n,
                                          n_total_vars,
                                          methodology_mode = NULL,
                                          run_id = NULL,
                                          excluded_mat = NULL) {
  n <- ncol(cm)
  if (n < 2L) return(invisible(NULL))

  # Rank the upper-triangle pairs by |r| and classify each pair's status.
  # #2b consistency: pairs flagged excluded_from_findings (analyst-internal /
  # circular, e.g. an affect instrument x a theme-membership column) are KEPT
  # visible but marked a distinct "excluded (circular)" category -- never
  # coloured as a "p < 0.05" finding (flag-don't-drop; mirrors the
  # correlations.csv + findings-text treatment). Pure data prep is factored
  # into a testable helper.
  df <- .correlation_lollipop_data(cm, pa, top_n, excluded_mat)
  if (is.null(df) || nrow(df) == 0L) {
    log_warn("No usable correlation pairs for lollipop chart")
    return(invisible(NULL))
  }

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
      values = c("p < 0.05" = "#3498DB", "n.s." = "#BDC3C7",
                 "excluded (circular)" = "#E67E22"),
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

#' Build an index-aligned logical matrix of excluded (circular) pairs
#'
#' Given the plot's variable names and the [extract_significant()] result,
#' returns an `n x n` logical matrix that is TRUE at `[i, j]` (and `[j, i]`)
#' whenever the unordered pair `(var_names[i], var_names[j])` was flagged
#' `excluded_from_findings`. The matrix is index-aligned to the correlation
#' matrix, so it survives the plot's variable-name humanizing. Returns an
#' all-FALSE matrix for NULL / empty / malformed input (back-compat no-op).
#'
#' @param var_names Character vector of the correlation matrix's variables
#'   (original, un-humanized names).
#' @param excluded_pairs The extract_significant() data frame, or NULL.
#' @return An `n x n` logical matrix.
#' @keywords internal
.build_excluded_pair_matrix <- function(var_names, excluded_pairs) {
  n <- length(var_names)
  m <- matrix(FALSE, n, n, dimnames = list(var_names, var_names))
  if (n == 0L || is.null(excluded_pairs) || !is.data.frame(excluded_pairs) ||
      nrow(excluded_pairs) == 0L ||
      !all(c("var1", "var2", "excluded_from_findings") %in% names(excluded_pairs))) {
    return(m)
  }
  ex <- excluded_pairs[excluded_pairs$excluded_from_findings %in% TRUE, , drop = FALSE]
  for (k in seq_len(nrow(ex))) {
    a <- ex$var1[k]; b <- ex$var2[k]
    if (!is.na(a) && !is.na(b) && a %in% var_names && b %in% var_names) {
      m[a, b] <- TRUE
      m[b, a] <- TRUE
    }
  }
  m
}

#' Mask excluded pairs' adjusted p-values to non-significant
#'
#' Sets the p-value of every excluded (circular) pair to 1 so corrplot's
#' `insig = "blank"` wipes its glyph -- the pair is not shown as a
#' significant finding in the heatmap (it remains in correlations.csv with
#' its real values). NULL `pa` or no exclusions returns `pa` unchanged.
#'
#' @param pa Adjusted-p matrix aligned to the correlation matrix (or NULL).
#' @param excluded_mat Logical matrix from [.build_excluded_pair_matrix()].
#' @return The (possibly masked) p-matrix.
#' @keywords internal
.mask_excluded_pvalues <- function(pa, excluded_mat) {
  if (is.null(pa) || is.null(excluded_mat) || !any(excluded_mat)) return(pa)
  pa[excluded_mat] <- 1  # > sig.level -> corrplot insig="blank" wipes the glyph
  pa
}

#' Overlay re-scoped (family-corrected) adjusted p-values onto the plot p-matrix
#'
#' extract_significant re-scopes the BH/Bonferroni multiple-comparison family to
#' the non-excluded (substantive) pairs (M2). The plot otherwise reads the
#' full-matrix \code{results$p_adjusted}, so a kept pair near the threshold could
#' read n.s. on the heatmap/lollipop while the report TABLE (which uses the
#' re-scoped df) calls it significant. This overlays the df's re-scoped adjusted
#' p (\code{p_value}) onto the plot matrix for kept pairs, keeping figure and
#' table consistent. Excluded pairs are left untouched (blanked separately via
#' [.mask_excluded_pvalues()]). A NULL / schema-incomplete df leaves the matrix
#' unchanged (byte-identical, e.g. when nothing was excluded).
#'
#' @param pa Adjusted-p matrix aligned to the correlation matrix (original names).
#' @param var_names Character vector of the matrix's ORIGINAL variable names.
#' @param rescoped_df The [extract_significant()] data frame (var1, var2, p_value).
#' @return The p-matrix with kept pairs' re-scoped adjusted p overlaid.
#' @keywords internal
.rescope_plot_pvalues <- function(pa, var_names, rescoped_df) {
  if (is.null(pa) || is.null(rescoped_df) ||
      !all(c("var1", "var2", "p_value") %in% names(rescoped_df))) return(pa)
  idx <- stats::setNames(seq_along(var_names), var_names)
  for (k in seq_len(nrow(rescoped_df))) {
    i <- idx[[rescoped_df$var1[k]]]; j <- idx[[rescoped_df$var2[k]]]
    pv <- rescoped_df$p_value[k]
    if (!is.null(i) && !is.null(j) && !is.na(pv)) {
      pa[i, j] <- pv; pa[j, i] <- pv
    }
  }
  pa
}

#' Rank + classify correlation pairs for the lollipop chart
#'
#' Pure data prep for [.create_correlation_lollipop()]: extracts the
#' upper-triangle pairs, keeps the non-NA ones, ranks them by `|r|`, takes
#' the top `top_n`, and labels each pair's status. A pair flagged in
#' `excluded_mat` is labelled `"excluded (circular)"` regardless of its
#' p-value, so an analyst-internal / circular pair is never coloured as a
#' `"p < 0.05"` finding -- but it is KEPT visible (flag-don't-drop). Returns
#' NULL when no pair has a usable correlation.
#'
#' @param cm Correlation matrix (names already humanized by the caller).
#' @param pa Adjusted-p matrix aligned to `cm` (NAs treated non-significant).
#' @param top_n Integer; number of top pairs to keep.
#' @param excluded_mat Logical matrix aligned to `cm` (or NULL).
#' @return A data frame (`label`, `r`, `significant` factor) or NULL.
#' @keywords internal
.correlation_lollipop_data <- function(cm, pa, top_n, excluded_mat = NULL) {
  ut <- upper.tri(cm, diag = FALSE)
  r_vals <- cm[ut]
  p_vals <- if (!is.null(pa)) pa[ut] else rep(NA_real_, length(r_vals))
  ex_vals <- if (!is.null(excluded_mat)) as.logical(excluded_mat[ut]) else rep(FALSE, length(r_vals))
  row_idx <- row(cm)[ut]
  col_idx <- col(cm)[ut]

  keep <- !is.na(r_vals)
  if (!any(keep)) return(NULL)
  row_idx <- row_idx[keep]; col_idx <- col_idx[keep]
  r_vals  <- r_vals[keep];  p_vals <- p_vals[keep]; ex_vals <- ex_vals[keep]

  ord <- order(-abs(r_vals))
  ord <- ord[seq_len(min(length(ord), as.integer(top_n)))]
  row_idx <- row_idx[ord]; col_idx <- col_idx[ord]
  r_vals  <- r_vals[ord];  p_vals <- p_vals[ord]; ex_vals <- ex_vals[ord]

  pair_label <- paste(rownames(cm)[row_idx], "<->", colnames(cm)[col_idx])
  status <- ifelse(ex_vals, "excluded (circular)",
                   ifelse(!is.na(p_vals) & p_vals < 0.05, "p < 0.05", "n.s."))

  data.frame(
    label = factor(pair_label, levels = rev(pair_label)),
    r = r_vals,
    significant = factor(status,
                         levels = c("p < 0.05", "n.s.", "excluded (circular)")),
    stringsAsFactors = FALSE
  )
}

#' Create theme co-occurrence network visualization
#'
#' Builds a network graph where nodes are themes and edges represent
#' co-occurrence strength (entries assigned to both themes). Requires
#' multi-label assignment columns (\code{theme_membership_*}).
#'
#' at scale the unfiltered network was an
#' unreadable hairball (a large run plotted 417 themes at
#' once with no legend). The \code{max_inline_themes} parameter caps
#' the visible network at the top-N most-connected themes (ranked by
#' weighted degree) and adds an inline legend explaining node size +
#' edge width encoding.
#'
#' @param data Tibble with theme_membership_* columns
#' @param theme_set ThemeSet object
#' @param output_path File path for PNG output
#' @param min_cooccurrence Minimum co-occurrence count to draw an edge (default 3)
#' @param methodology_mode Optional character. When supplied,
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

  # top-N filter by weighted degree (sum of
  # incident edge weights). At 400+ themes an earlier plot was
  # an unreadable hairball with no legend. Keep the most-connected
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

  # Build a real legend explaining node size
  # + edge width encoding so the chart is interpretable without
  # external documentation. Three representative node-size + edge-
  # weight markers anchor the visual scale.
  plot_title <- if (n_filtered > 0L) {
    sprintf("Theme Co-occurrence Network (top %d of %d themes)",
             n_post_filter, n_pre_filter)
  } else {
    "Theme Co-occurrence Network"
  }

  # Seed the Fruchterman-Reingold
  # layout RNG so identical inputs produce byte-identical PNGs across
  # runs (R7 replay-equivalence). An earlier implementation
  # called layout_with_fr() with no seed control, so even on identical
  # data the network plot rendered with different node positions.
  # .with_seed() uses withr when installed and a save/restore fallback
  # otherwise (withr is Suggests), so this never hard-depends on withr.
  fr_layout <- .with_seed(42L, igraph::layout_with_fr(g))

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
    # encoding directly on the chart (an earlier version of
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
#' z-transformation z = atanh(r), back-transformed via tanh. The standard
#' error is method-aware: Pearson (incl. point-biserial / phi) uses the
#' classic 1/sqrt(n-3); Spearman uses the Bonett & Wright (2000) standard
#' error sqrt((1 + r^2/2)/(n-3)), which is wider -- so rank-based CIs are
#' not anti-conservative.
#'
#' @param r Observed correlation coefficient
#' @param n Number of observations. Use the PAIRWISE-complete count (not the
#'   full table) when the data carry missing values, since the matrix is
#'   computed with use = "pairwise.complete.obs".
#' @param conf_level Confidence level (default 0.95)
#' @param method Correlation method ("pearson" default, or "spearman"); selects
#'   the standard-error formula.
#' @return Numeric vector of length 2: c(lower, upper), or c(NA, NA) if n < 4
#' @keywords internal
.fisher_z_ci <- function(r, n, conf_level = 0.95, method = "pearson") {
  if (n < 4 || is.na(r) || abs(r) >= 1) return(c(NA_real_, NA_real_))
  z <- atanh(r)
  se <- if (identical(method, "spearman")) {
    sqrt((1 + r^2 / 2) / (n - 3))
  } else {
    1 / sqrt(n - 3)
  }
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
  #
  # C4 (dataset-agnostic): test EVERY numeric
  # metric column for theme-group differences via Mann-Whitney U, not
  # just the two pakhom-engineered sentiment columns. A clinical
  # researcher with a `tenure_months` column now sees "Theme X has
  # significantly higher tenure_months than non-X"; before this fix
  # they never would. See pakhom/R/16_report_helpers.R::.detect_metric_columns.
  base_continuous <- intersect(c("sentiment_score", "emotion_intensity"), names(data))
  metric_cols <- .detect_metric_columns(data, config = config)
  continuous_vars <- unique(c(base_continuous, metric_cols))
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
    # explicit integer cast so the downstream tibble construction
    # via vapply(..., integer(1), ...) doesn't crash on the boundary
    # case where sum() returns double.
    n_members <- as.integer(sum(members, na.rm = TRUE))
    n_non_members <- as.integer(sum(non_members, na.rm = TRUE))

    if (n_members < min_group || n_non_members < min_group) next

    for (cv in continuous_vars) {
      vals_members <- data[[cv]][members]
      vals_non <- data[[cv]][non_members]
      vals_members <- vals_members[!is.na(vals_members)]
      vals_non <- vals_non[!is.na(vals_non)]

      if (length(vals_members) < min_group || length(vals_non) < min_group) next

      test_result <- tryCatch({
        wt <- wilcox.test(vals_members, vals_non, exact = FALSE)
        # Replace the earlier
        # z-from-p-value derivation with a direct rank-biserial
        # computation. The earlier effect_r was `abs(qnorm(p/2)) /
        # sqrt(n_total)`, which (a) loses sign and (b) blows up to
        # +/-Inf when p < 1e-300 (qnorm returns -Inf). Rank-biserial
        # is `(U_members / (n_m * n_n)) - (U_non / (n_m * n_n))` =
        # `2 * U_members / (n_m * n_n) - 1`, which is sign-aware and
        # numerically stable. Magnitude scales conventionally on
        # [-1, 1]; sign matches "higher rank in members".
        n_m <- length(vals_members)
        n_n <- length(vals_non)
        u_members <- as.numeric(wt$statistic)  # R's wilcox W = U for x
        rank_biserial <- (2 * u_members / (n_m * n_n)) - 1

        mean_m <- round(mean(vals_members), 3)
        mean_n <- round(mean(vals_non), 3)
        # Derive direction from the
        # rank-biserial sign rather than mean comparison. On skewed
        # distributions mean and rank centroid can disagree (e.g.
        # outliers move the mean opposite the median), which an earlier
        # version would render as "Higher in theme, r =
        # -0.9" -- internally contradictory. Mann-Whitney IS a rank
        # test, so the rank-based direction is the methodologically
        # consistent one.
        direction <- if (is.na(rank_biserial)) "Unknown"
                      else if (rank_biserial > 0) "Higher in theme"
                      else if (rank_biserial < 0) "Lower in theme"
                      else "No difference"

        list(
          theme = theme_label,
          variable = gsub("_", " ", cv),
          mean_members = mean_m,
          mean_non_members = mean_n,
          w_statistic = round(wt$statistic, 1),
          p_value = wt$p.value,
          effect_r = round(rank_biserial, 3),
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

  # emit n_members + n_non_members. The internal
  # results list already carried these (computed at lines above) but
  # the tibble construction dropped them earlier. Without them
  # consumers couldn't tell whether an effect_r = 0.05 came from
  # n_members = 5 (low power) or n_members = 500 (substantively
  # negligible) -- a 100x power variation invisible to the consumer.
  df <- tibble::tibble(
    theme = vapply(results, `[[`, character(1), "theme"),
    variable = vapply(results, `[[`, character(1), "variable"),
    n_members = vapply(results, `[[`, integer(1), "n_members"),
    n_non_members = vapply(results, `[[`, integer(1), "n_non_members"),
    mean_members = vapply(results, `[[`, numeric(1), "mean_members"),
    mean_non_members = vapply(results, `[[`, numeric(1), "mean_non_members"),
    w_statistic = vapply(results, `[[`, numeric(1), "w_statistic"),
    p_value = vapply(results, `[[`, numeric(1), "p_value"),
    effect_r = vapply(results, `[[`, numeric(1), "effect_r"),
    direction = vapply(results, `[[`, character(1), "direction")
  )

  # Multi-method p-value adjustments (raw + BH FDR + Bonferroni FWER).
  # 'p_adjusted' / 'significant' kept for back-compat (= Bonferroni at alpha=0.05);
  # 'meaningful_effect' is the new effect-size-based exploratory flag.
  adjustments <- .compute_p_adjustments(df$p_value)
  df$p_raw <- adjustments$raw
  df$p_bh <- adjustments$bh
  df$p_bonferroni <- adjustments$bonferroni
  df$p_adjusted <- df$p_bonferroni                   # back-compat
  df$significant <- df$p_adjusted < 0.05             # back-compat
  df$meaningful_effect <- abs(df$effect_r) >= 0.10   # Cohen's small-effect threshold (: sign-aware)
  # explicit effect-size label parallel to the
  # correlation tibble's effect_size column. negligible / small /
  # medium / large lets the report headline + downstream consumers
  # filter / annotate consistently across statistical methods.
  df$effect_size <- vapply(df$effect_r, function(r) {
    if (is.na(r))            NA_character_
    else if (abs(r) >= 0.5)  "large"
    else if (abs(r) >= 0.3)  "medium"
    else if (abs(r) >= 0.10) "small"
    else                     "negligible"
  }, character(1))
  df <- df[order(-abs(df$effect_r)), ]               # sort by effect size

  log_info("Theme group comparisons: {nrow(df)} tests; ",
           "{sum(df$meaningful_effect, na.rm = TRUE)} with |effect_r| >= 0.10, ",
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
#' applies the same \code{min_theme_entries} filter
#' that \code{prepare_correlation_data} and \code{compare_theme_groups}
#' use, so the three statistical layers report counts over a consistent
#' theme cohort. An earlier version admitted every theme
#' regardless of frequency, which produced thousands of degenerate
#' Fisher tests on rare themes (a large run found 99.1% of
#' Fisher pairs had \code{observed_both = 0}).
#'
#' @param data Tibble with theme_membership_* columns
#' @param theme_set ThemeSet object
#' @param min_expected Minimum expected cell count for chi-square (default 5)
#' @param min_theme_entries Integer; themes with fewer than this many
#'   positive entries are excluded. Default 5L, matching the
#'   correlation matrix + theme-group test default.
#' @param min_observed_both Integer; polish. Pairs whose observed
#'   co-occurrence is below this count are skipped (Fisher tests on
#'   zero-co-occurrence pairs are uninterpretable). Default 1L.
#' @return Tibble with co-occurrence test results
#' @export
test_theme_cooccurrence <- function(data, theme_set, min_expected = 5,
                                      min_theme_entries = 5L,
                                      min_observed_both = 1L) {

  membership_cols <- grep("^theme_membership_", names(data), value = TRUE)
  if (length(membership_cols) < 2) {
    log_warn("Need at least 2 themes for co-occurrence analysis")
    return(tibble::tibble())
  }

  # pre-filter membership columns by per-theme frequency so the
  # cohort matches prepare_correlation_data + compare_theme_groups.
  min_theme_entries <- as.integer(min_theme_entries %||% 5L)
  if (is.na(min_theme_entries) || min_theme_entries < 1L) min_theme_entries <- 5L
  n_input_themes <- length(membership_cols)
  membership_cols <- membership_cols[vapply(
    membership_cols,
    function(col) sum(data[[col]] == 1L, na.rm = TRUE) >= min_theme_entries,
    logical(1)
  )]
  n_excluded <- n_input_themes - length(membership_cols)
  if (n_excluded > 0L) {
    log_info(paste0(
      "test_theme_cooccurrence: excluded {n_excluded} themes with ",
      "< {min_theme_entries} members ({length(membership_cols)} themes remain)."
    ))
  }
  if (length(membership_cols) < 2L) {
    log_warn(
      "After min_theme_entries filter, fewer than 2 themes remain for co-occurrence"
    )
    return(tibble::tibble())
  }

  n_total <- nrow(data)
  results <- list()
  min_observed_both <- as.integer(min_observed_both %||% 1L)
  if (is.na(min_observed_both) || min_observed_both < 0L) min_observed_both <- 1L

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
    # as.double() on the first factor promotes the product to double so it
    # cannot overflow R's 32-bit integer range: sum(logical) returns an
    # integer, and integer*integer silently overflows to NA past ~2.1e9
    # (reachable when two common themes co-occur across a large corpus).
    expected_both <- round(as.double(sum(a == 1)) * sum(b == 1) / n, 1)

    # skip pairs with too-low observed co-occurrence.
    # On a large saturation run, 93.2% of Fisher pairs had
    # observed_both = 0 -- the tests are vacuous and clog the output
    # tibble with thousands of uninterpretable rows.
    if (observed_both < min_observed_both) next

    # Check expected cell counts
    expected_mat <- outer(rowSums(ct), colSums(ct)) / n
    use_fisher <- any(expected_mat < min_expected)

    test_result <- tryCatch({
      if (use_fisher) {
        ft <- fisher.test(ct)
        # compute Cramer's V (= phi coefficient
        # for a 2x2 table) directly from the contingency table when
        # Fisher dispatches, so the Fisher path doesn't emit NA effect
        # size. Earlier, 99.1% of Fisher tests had NA Cramer's V,
        # giving the audit no way to rank them by magnitude.
        # phi = (ad - bc) / sqrt((a+b)(c+d)(a+c)(b+d))
        a11 <- as.numeric(ct[1, 1]); a12 <- as.numeric(ct[1, 2])
        a21 <- as.numeric(ct[2, 1]); a22 <- as.numeric(ct[2, 2])
        denom <- sqrt(
          (a11 + a12) * (a21 + a22) * (a11 + a21) * (a12 + a22)
        )
        phi_2x2 <- if (is.finite(denom) && denom > 0) {
          (a11 * a22 - a12 * a21) / denom
        } else NA_real_
        # chi_equiv = phi^2 * n, so sqrt(chi_equiv / n) = |phi|. Store
        # chi_equiv as the test statistic for downstream consumers that
        # expect a chi-square-shaped statistic; cramers_v derives from
        # the absolute phi directly to skip the chi-square round-trip.
        # Dropped phi_signed (was created in
        # the internal list but never carried into the output tibble,
        # so consumers couldn't use it). Future sign-aware reporting
        # can derive sign from a^ad-bc^>0 directly when needed.
        chi_equiv <- if (!is.na(phi_2x2)) phi_2x2^2 * n else NA_real_
        list(stat = if (!is.na(chi_equiv)) round(chi_equiv, 3) else NA_real_,
              p_value = ft$p.value, method = "Fisher")
      } else {
        chi <- chisq.test(ct, correct = FALSE)
        list(stat = round(chi$statistic, 3), p_value = chi$p.value,
              method = "Chi-square")
      }
    }, error = function(e) NULL)

    if (is.null(test_result)) next

    # Cramer's V (now populated for both Fisher and Chi-square paths via
    # fix above).
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
  # explicit effect-size label aligned with the
  # correlation + theme-group tibbles. NA when Cramer's V is NA (still
  # possible for some Fisher edge cases; closes the common path).
  df$effect_size <- vapply(df$cramers_v, function(v) {
    if (is.na(v))            NA_character_
    else if (abs(v) >= 0.5)  "large"
    else if (abs(v) >= 0.3)  "medium"
    else if (abs(v) >= 0.10) "small"
    else                     "negligible"
  }, character(1))
  df <- df[order(-abs(df$cramers_v)), ]              # sort by effect size

  log_info("Theme co-occurrence: {nrow(df)} pairs; ",
           "{sum(df$meaningful_effect, na.rm = TRUE)} with |Cramer's V| >= 0.10, ",
           "{sum(df$significant)} significant after Bonferroni (p < 0.05). ",
           "All three p-adjustments (raw, BH, Bonferroni) reported per pair.")
  df
}
