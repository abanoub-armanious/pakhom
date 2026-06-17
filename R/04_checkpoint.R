# ==============================================================================
# Checkpoint System: Save/Load/Resume for Long-Running Pipelines
# ==============================================================================
# Saves .rds files after each expensive pipeline step so work isn't lost.
# Supports partial checkpoints within steps (every N entries for coding/sentiment).
#
# Architecture: FLAT checkpoint directory
#   {output_dir}/checkpoints/   <-- all .rds files go here directly
#   No subdirectories. One run = one output directory = one checkpoint directory.
# ==============================================================================

#' Initialize checkpoint system for a pipeline run
#'
#' @param output_dir Base output directory for this run
#' @param config_hash Hash of config for detecting changes between runs
#' @param ... Reserved for future arguments; currently ignored. Accepted
#'   so callers passing extra named arguments don't error out.
#' @return CheckpointManager S3 object
#' @export
init_checkpoints <- function(output_dir, config_hash = NULL, ...) {
  # Flat checkpoint directory -- no run_id subdirectory

  checkpoint_dir <- file.path(output_dir, "checkpoints")
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

  manager <- list(
    checkpoint_dir = checkpoint_dir,
    output_dir = output_dir,
    run_id = basename(output_dir),
    config_hash = config_hash,
    step_order = c(
      "data_loaded", "methodology_setup", "progressive_coding",
      "sentiment_done", "themes_generated", "research_coverage", "correlations"
    ),
    manifest_file = file.path(checkpoint_dir, "_manifest.rds")
  )
  class(manager) <- "CheckpointManager"

  # Initialize or load manifest
  if (file.exists(manager$manifest_file)) {
    manifest <- tryCatch(
      readRDS(manager$manifest_file),
      error = function(e) {
        log_warn("Existing manifest corrupted, starting fresh: {e$message}")
        NULL
      }
    )
    if (!is.null(manifest)) {
      manager$manifest <- manifest
    } else {
      manager$manifest <- list(config_hash = config_hash,
                               created = Sys.time(), steps = list())
      saveRDS(manager$manifest, manager$manifest_file)
    }
  } else {
    manager$manifest <- list(
      config_hash = config_hash,
      created = Sys.time(),
      steps = list()
    )
    saveRDS(manager$manifest, manager$manifest_file)
  }

  log_info("Checkpoint system initialized: {checkpoint_dir}")
  manager
}

#' Read checkpoint manifest from disk
#' @keywords internal
.read_manifest <- function(manager) {
  manifest_path <- file.path(manager$checkpoint_dir, "_manifest.rds")
  if (!file.exists(manifest_path)) return(list(steps = list()))
  tryCatch(
    readRDS(manifest_path),
    error = function(e) {
      log_warn("Checkpoint manifest corrupted, starting fresh: {e$message}")
      list(steps = list())
    }
  )
}

#' Save checkpoint at a given pipeline step
#'
#' @param manager CheckpointManager object
#' @param step_name Step identifier (e.g., "sentiment_done")
#' @param data The data/results to save
save_checkpoint <- function(manager, step_name, data) {
  validate_class(manager, "CheckpointManager")

  file_path <- file.path(manager$checkpoint_dir,
                          paste0(step_name, ".rds"))
  saveRDS(data, file_path, compress = "gzip")

  # Re-read manifest from disk to get all previous steps (R is pass-by-value)
  manifest <- .read_manifest(manager)

  manifest$steps[[step_name]] <- list(
    timestamp = Sys.time(),
    file = basename(file_path),
    size_bytes = file.size(file_path)
  )
  saveRDS(manifest, manager$manifest_file)

  size_mb <- round(file.size(file_path) / 1024 / 1024, 2)
  log_info("Checkpoint saved: {step_name} ({size_mb} MB)")

  # The full save supersedes any in-step partial: delete it so a stale
  # partial cannot be adopted by a later resume (e.g. a user manually
  # removing the full checkpoint to force a re-run of the step).
  partial_path <- file.path(manager$checkpoint_dir,
                            paste0(step_name, "_partial.rds"))
  if (file.exists(partial_path)) unlink(partial_path)

  invisible(manager)
}

#' Save partial checkpoint within a step (for long-running batch operations)
#'
#' Partials are consumed on resume: the coding step reads
#' \code{progressive_coding_partial.rds} (via the pipeline) and the
#' sentiment step reads \code{sentiment_done_partial.rds} (inside
#' \code{analyze_sentiment}), so a crash mid-step does not re-pay the
#' LLM cost of completed entries. A successful \code{save_checkpoint}
#' for the step deletes its partial.
#'
#' @param manager CheckpointManager object
#' @param step_name Step identifier
#' @param data Partial results so far
#' @param progress_idx Index of last completed item
save_partial_checkpoint <- function(manager, step_name, data, progress_idx) {
  validate_class(manager, "CheckpointManager")

  file_path <- file.path(manager$checkpoint_dir,
                          paste0(step_name, "_partial.rds"))

  partial <- list(
    data = data,
    progress_idx = progress_idx,
    timestamp = Sys.time()
  )
  saveRDS(partial, file_path, compress = "gzip")

  log_debug("Partial checkpoint: {step_name} (progress: {progress_idx})")
  invisible(manager)
}

#' Load checkpoint for a step
#'
#' @param manager CheckpointManager object
#' @param step_name Step to load
#' @return Checkpointed data, or NULL if not found
load_checkpoint <- function(manager, step_name) {
  validate_class(manager, "CheckpointManager")

  file_path <- file.path(manager$checkpoint_dir,
                          paste0(step_name, ".rds"))

  # readRDS on a corrupt/truncated payload throws -- exactly the partial-failure
  # case resume=TRUE is meant to recover from (process killed mid-write). Treat
  # an unreadable checkpoint as "step not done" (return NULL) so the pipeline
  # recomputes it, rather than crashing on every resume attempt.
  .read_checkpoint_safely <- function(path, label) {
    tryCatch(readRDS(path), error = function(e) {
      log_warn(paste0(label, " checkpoint '", step_name,
                      "' is unreadable (", conditionMessage(e),
                      "); recomputing this step."))
      NULL
    })
  }
  if (file.exists(file_path)) {
    log_info("Loading checkpoint: {step_name}")
    return(.read_checkpoint_safely(file_path, "Full"))
  }

  # Check for partial
  partial_path <- file.path(manager$checkpoint_dir,
                             paste0(step_name, "_partial.rds"))
  if (file.exists(partial_path)) {
    log_info("Loading partial checkpoint: {step_name}")
    return(.read_checkpoint_safely(partial_path, "Partial"))
  }

  NULL
}

#' List available checkpoints with metadata
#'
#' @param manager CheckpointManager object
#' @return List with completed (character vector) and details (tibble)
list_checkpoints <- function(manager) {
  validate_class(manager, "CheckpointManager")

  # Re-read manifest from disk (save_checkpoint writes to disk but R is pass-by-value)
  manifest <- .read_manifest(manager)

  steps <- manifest$steps
  completed <- names(steps)

  details <- if (length(steps) == 0) {
    tibble(step_name = character(0), timestamp = character(0), size_mb = numeric(0))
  } else {
    tibble(
      step_name = completed,
      timestamp = vapply(steps, function(s) as.character(s$timestamp), character(1)),
      size_mb = vapply(steps, function(s) round(s$size_bytes / 1024 / 1024, 2), numeric(1))
    )
  }

  list(completed = completed, details = details)
}

#' Determine the last completed step for resume
#'
#' @param manager CheckpointManager object
#' @return Character: last completed step name, or NULL if no checkpoints
find_resume_point <- function(manager) {
  validate_class(manager, "CheckpointManager")

  # Re-read manifest from disk
  manifest <- .read_manifest(manager)

  completed_steps <- names(manifest$steps)
  if (length(completed_steps) == 0) return(NULL)

  # Check config hash. isTRUE() also covers an NA hash (hash_config
  # returns NA_character_ for a missing config file): an unknowable hash
  # is not a known mismatch, and a bare `if (NA)` would crash the resume.
  if (!is.null(manager$config_hash) &&
      !is.null(manifest$config_hash) &&
      isTRUE(manager$config_hash != manifest$config_hash)) {
    log_warn("Config has changed since last checkpoint. Results may be inconsistent.")
    log_warn("Consider starting fresh (resume = FALSE)")
  }

  # Find the last completed step in order
  last_step <- NULL
  for (step in manager$step_order) {
    if (step %in% completed_steps) {
      last_step <- step
    }
  }

  if (!is.null(last_step)) {
    log_info("Resume point found: {last_step}")
  }

  last_step
}

#' Invalidate a checkpoint and all downstream checkpoints
#'
#' Used when the researcher requests a loop-back (e.g., revise_codebook
#' disposition after theme review). Deletes the named step and everything
#' after it in step_order so the pipeline re-runs those steps.
#'
#' @param manager CheckpointManager
#' @param from_step Character: the step to invalidate (inclusive).
#' @return Updated manager (manifest rewritten)
#' @keywords internal
invalidate_checkpoints_from <- function(manager, from_step) {
  validate_class(manager, "CheckpointManager")

  idx <- match(from_step, manager$step_order)
  if (is.na(idx)) {
    log_warn("Step '{from_step}' not in step_order -- no invalidation")
    return(manager)
  }

  steps_to_remove <- manager$step_order[idx:length(manager$step_order)]
  manifest <- .read_manifest(manager)

  for (step in steps_to_remove) {
    rds_path <- file.path(manager$checkpoint_dir, paste0(step, ".rds"))
    partial_path <- file.path(manager$checkpoint_dir, paste0(step, "_partial.rds"))
    if (file.exists(rds_path)) {
      file.remove(rds_path)
      log_info("Invalidated checkpoint: {step}")
    }
    if (file.exists(partial_path)) file.remove(partial_path)
    manifest$steps[[step]] <- NULL
  }

  saveRDS(manifest, manager$manifest_file)
  manager
}

#' Find the most recent run folder in the results directory
#'
#' Searches for timestamped run folders (run_YYYY-MM-DD_HHMMSS) at the
#' top level of the results directory. Skips ghost directories (those with
#' no completed checkpoints or output files beyond the manifest).
#'
#' @param results_base Base results directory containing run folders
#' @return Character: folder name of most recent run, or NULL
find_latest_run <- function(results_base) {
  if (!dir.exists(results_base)) return(NULL)

  all_dirs <- list.dirs(results_base, full.names = FALSE, recursive = FALSE)
  # Only match timestamped run folders (exclude "latest" symlink). The
  # optional _M[123] tail accommodates the T1.7 mode-suffixed run dirs
  # introduced with run_id_with_mode; without it, resume
  # silently fails to find the latest run for ANY mode that emits a
  # mode-suffixed dir name.
  run_dirs <- grep("^run_\\d{4}-\\d{2}-\\d{2}_\\d{6}(_M[123])?$",
                    all_dirs, value = TRUE)
  if (length(run_dirs) == 0) return(NULL)

  # Sort by name (timestamp format ensures chronological order), most recent first
  run_dirs <- sort(run_dirs, decreasing = TRUE)

  # Return the most recent directory that has meaningful content
  for (d in run_dirs) {
    full_path <- file.path(results_base, d)
    cp_dir <- file.path(full_path, "checkpoints")
    if (dir.exists(cp_dir)) {
      rds_files <- list.files(cp_dir, pattern = "\\.rds$", recursive = FALSE)
      completed <- rds_files[!grepl("_partial|_manifest", rds_files)]
      if (length(completed) > 0) return(d)
    }
    # Also check if there are output files (CSVs, JSONs, etc.)
    output_files <- list.files(full_path, pattern = "\\.(csv|json|html|png)$", recursive = FALSE)
    if (length(output_files) > 0) return(d)
  }

  # If no directory has meaningful content, return the most recent one
  run_dirs[1]
}

#' Compute a hash of a config file for change detection
#' @param config_path Path to YAML config file
#' @return Character hash string
hash_config <- function(config_path) {
  if (!file.exists(config_path)) return(NA_character_)
  yaml_text <- paste(readLines(config_path, warn = FALSE), collapse = "\n")
  rlang::hash(yaml_text)
}

#' Print method for CheckpointManager
#' @param x CheckpointManager object
#' @param ... Additional arguments (ignored)
#' @export
print.CheckpointManager <- function(x, ...) {
  cat(sprintf("CheckpointManager [%s]\n", x$run_id))
  cat(sprintf("  Directory: %s\n", x$checkpoint_dir))
  completed <- names(x$manifest$steps)
  cat(sprintf("  Completed steps: %d/%d\n", length(completed), length(x$step_order)))
  if (length(completed) > 0) {
    cat(sprintf("  Last step: %s\n", completed[length(completed)]))
  }
  invisible(x)
}
