# ==============================================================================
# Validation Utility Functions
# ==============================================================================

#' Validate that a data frame has required columns
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param caller Name of the calling function (for error messages)
#' @keywords internal
validate_data_columns <- function(data, required_cols, caller = "unknown") {
  if (!is.data.frame(data)) {
    stop(sprintf("[%s] Expected a data frame, got %s", caller, class(data)[1]))
  }
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop(sprintf("[%s] Missing required columns: %s. Available: %s",
                 caller, paste(missing, collapse = ", "),
                 paste(names(data), collapse = ", ")))
  }
  invisible(TRUE)
}

#' Validate that an AI provider is properly constructed
#'
#' @param provider Object to validate
#' @param allow_null If TRUE, NULL is accepted (for optional provider)
#' @param caller Name of the calling function
#' @keywords internal
validate_provider <- function(provider, allow_null = FALSE, caller = "unknown") {
  if (is.null(provider)) {
    if (allow_null) return(invisible(TRUE))
    stop(sprintf("[%s] AI provider is required but was NULL", caller))
  }
  if (!inherits(provider, "AIProvider")) {
    stop(sprintf("[%s] Expected AIProvider object, got %s", caller, class(provider)[1]))
  }
  invisible(TRUE)
}

#' Validate that an object inherits from a given class
#'
#' @param x Object to check
#' @param cls Expected class name
#' @param caller Name of the calling function (for error messages)
#' @keywords internal
validate_class <- function(x, cls, caller = NULL) {
  if (!inherits(x, cls)) {
    fn_name <- caller %||% deparse(sys.call(-1)[[1]])
    stop(sprintf("[%s] Expected a %s object, got %s", fn_name, cls, class(x)[1]),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate a methodology mode declaration
#'
#' Multi-mode architecture: every run must declare its
#' methodological posture (reflexive_scaffold / codebook_collaborative /
#' framework_applied). The declaration determines which AI behaviors are
#' permitted, which artifacts are mandatory, and which report sections are
#' generated. There is intentionally no default; missing declarations
#' produce an actionable error pointing to the decision aid.
#'
#' @param mode Character scalar; the methodology mode name
#' @param allow_null If TRUE, NULL is accepted (used by .config_defaults()
#'   which is a bare schema; user-facing validate_config() always passes
#'   allow_null = FALSE)
#' @param caller Name of the calling function (for error messages)
#' @keywords internal
validate_methodology_mode <- function(mode, allow_null = FALSE,
                                      caller = "validate_methodology_mode") {
  if (is.null(mode)) {
    if (allow_null) return(invisible(TRUE))
    stop(sprintf(
      "[%s] methodology.mode is required and was NULL.\n  Valid values: %s.\n  See methodology_decision_aid() for help choosing.",
      caller, paste(.VALID_METHODOLOGY_MODES, collapse = ", ")
    ), call. = FALSE)
  }
  if (!is.character(mode) || length(mode) != 1L || is.na(mode)) {
    stop(sprintf(
      "[%s] methodology.mode must be a single non-NA character string, got %s of length %d",
      caller, class(mode)[1], length(mode)
    ), call. = FALSE)
  }
  if (!mode %in% .VALID_METHODOLOGY_MODES) {
    stop(sprintf(
      "[%s] Invalid methodology mode '%s'. Valid values: %s.\n  See methodology_decision_aid() for help choosing.",
      caller, mode, paste(.VALID_METHODOLOGY_MODES, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' Read \code{config$methodology$mode} defensively
#'
#' Several pipeline + report sites need to read the methodology mode out
#' of a config that may be a bare list (where \code{$methodology} is
#' absent) or a partially-built ThematicConfig. This helper centralizes
#' the tryCatch-on-NULL pattern so future drift in one site can't
#' diverge from the others.
#'
#' Returns \code{NULL} when the field is missing, NA, or empty -- the
#' "unknown methodology" path the various stamping helpers handle.
#' @param config A list or ThematicConfig.
#' @return Character scalar mode, or \code{NULL}.
#' @keywords internal
.config_methodology_mode <- function(config) {
  mode <- tryCatch(config$methodology$mode, error = function(e) NULL)
  if (is.null(mode) || length(mode) != 1L || is.na(mode) || !nzchar(mode)) {
    return(NULL)
  }
  as.character(mode)
}

