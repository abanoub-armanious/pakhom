# Tests for the soft-lock + parent_run_id mechanism (T1.5,
# R/run_state.R). The REDCap-style state model is the empirical answer
# to AC5 (soft-lock with audit trail) and AC6 (symmetric obligations
# across modes -- methodology cannot be silently re-declared).

# ---- read_run_metadata ------------------------------------------------------

test_that("read_run_metadata returns NULL for a directory without metadata", {
  d <- withr::local_tempdir()
  expect_null(read_run_metadata(d))
})

test_that("read_run_metadata returns the parsed list when present", {
  d <- withr::local_tempdir()
  jsonlite::write_json(list(run_id = "x", methodology_mode = "reflexive_scaffold",
                             is_finalized = FALSE),
                        file.path(d, "run_metadata.json"),
                        pretty = TRUE, auto_unbox = TRUE)
  m <- read_run_metadata(d)
  expect_equal(m$run_id, "x")
  expect_equal(m$methodology_mode, "reflexive_scaffold")
  expect_false(m$is_finalized)
})

test_that("read_run_metadata returns NULL on parse error (with warning)", {
  d <- withr::local_tempdir()
  writeLines("not valid json {", file.path(d, "run_metadata.json"))
  expect_message(
    expect_null(read_run_metadata(d)),
    NA  # log_warn goes to logger, not message; we just check it returns NULL
  )
})

# ---- init_run_state ---------------------------------------------------------

test_that("init_run_state creates run_metadata.json with required fields", {
  d <- withr::local_tempdir()
  meta <- init_run_state(
    run_dir = d, run_id = "run_test",
    methodology_mode = "reflexive_scaffold"
  )
  expect_equal(meta$run_id, "run_test")
  expect_equal(meta$methodology_mode, "reflexive_scaffold")
  expect_false(meta$is_finalized)
  expect_null(meta$finalized_at)
  expect_null(meta$parent_run_id)
  expect_null(meta$mode_changed_from)
  expect_true(nzchar(meta$mode_locked_at))
  expect_true(nzchar(meta$created_at))
  expect_equal(meta$schema_version, "1.0.0")
  expect_true(file.exists(file.path(d, "run_metadata.json")))
})

test_that("init_run_state validates methodology_mode (rejects unknown modes)", {
  d <- withr::local_tempdir()
  expect_error(
    init_run_state(d, "x", methodology_mode = "free_for_all"),
    "Invalid methodology"
  )
})

test_that("init_run_state is idempotent on resume (does not overwrite finalized state)", {
  d <- withr::local_tempdir()
  init_run_state(d, "r1", "reflexive_scaffold")
  finalize_run(d)

  # Re-init: should preserve the finalized state, not reset it
  meta <- init_run_state(d, "r1", "reflexive_scaffold")
  expect_true(meta$is_finalized)
  expect_true(nzchar(meta$finalized_at))
})

test_that("init_run_state stores parent_run_id and mode_changed_from when supplied", {
  d <- withr::local_tempdir()
  meta <- init_run_state(
    run_dir = d, run_id = "r2",
    methodology_mode = "framework_applied",
    parent_run_id    = "r1",
    mode_changed_from = "reflexive_scaffold"
  )
  expect_equal(meta$parent_run_id, "r1")
  expect_equal(meta$mode_changed_from, "reflexive_scaffold")
})

test_that("init_run_state accepts and writes extra fields verbatim", {
  d <- withr::local_tempdir()
  meta <- init_run_state(
    run_dir = d, run_id = "r1",
    methodology_mode = "reflexive_scaffold",
    provider = "anthropic", study_name = "Sleep & Meds"
  )
  expect_equal(meta$provider, "anthropic")
  expect_equal(meta$study_name, "Sleep & Meds")
})

# ---- is_run_finalized -------------------------------------------------------

test_that("is_run_finalized: FALSE for missing metadata, FALSE for active run, TRUE after finalize", {
  d <- withr::local_tempdir()
  expect_false(is_run_finalized(d))  # no metadata at all

  init_run_state(d, "r1", "reflexive_scaffold")
  expect_false(is_run_finalized(d))  # active run

  finalize_run(d)
  expect_true(is_run_finalized(d))
})

# ---- finalize_run -----------------------------------------------------------

test_that("finalize_run sets is_finalized + finalized_at on an active run", {
  d <- withr::local_tempdir()
  init_run_state(d, "r1", "reflexive_scaffold")
  meta <- finalize_run(d)
  expect_true(meta$is_finalized)
  expect_true(nzchar(meta$finalized_at))
})

test_that("finalize_run is idempotent on already-finalized runs", {
  d <- withr::local_tempdir()
  init_run_state(d, "r1", "reflexive_scaffold")
  m1 <- finalize_run(d)
  m2 <- finalize_run(d)
  # finalized_at should not advance on re-finalize
  expect_equal(m1$finalized_at, m2$finalized_at)
})

test_that("finalize_run on a directory without metadata returns NULL with warning", {
  d <- withr::local_tempdir()
  expect_null(finalize_run(d))
})

# ---- methodology_mismatch_status -------------------------------------------

test_that("methodology_mismatch_status: no_metadata when run_dir has no metadata", {
  d <- withr::local_tempdir()
  cfg <- list(methodology = list(mode = "reflexive_scaffold"))
  expect_equal(methodology_mismatch_status(d, cfg), "no_metadata")
})

test_that("methodology_mismatch_status: match when modes agree", {
  d <- withr::local_tempdir()
  init_run_state(d, "r1", "reflexive_scaffold")
  cfg <- list(methodology = list(mode = "reflexive_scaffold"))
  expect_equal(methodology_mismatch_status(d, cfg), "match")
})

test_that("methodology_mismatch_status: mismatch_active on active run with different mode", {
  d <- withr::local_tempdir()
  init_run_state(d, "r1", "reflexive_scaffold")
  cfg <- list(methodology = list(mode = "framework_applied"))
  expect_equal(methodology_mismatch_status(d, cfg), "mismatch_active")
})

test_that("methodology_mismatch_status: mismatch_finalized on finalized run with different mode", {
  d <- withr::local_tempdir()
  init_run_state(d, "r1", "reflexive_scaffold")
  finalize_run(d)
  cfg <- list(methodology = list(mode = "framework_applied"))
  expect_equal(methodology_mismatch_status(d, cfg), "mismatch_finalized")
})

# ---- clone_run_with_new_mode -----------------------------------------------

test_that("clone_run_with_new_mode creates a new run dir with parent linkage", {
  parent_d <- withr::local_tempdir()
  init_run_state(parent_d, "parent_r1", "reflexive_scaffold")
  finalize_run(parent_d)

  child_d <- file.path(dirname(parent_d), "child_run")
  on.exit(unlink(child_d, recursive = TRUE), add = TRUE)
  meta <- clone_run_with_new_mode(parent_d, "framework_applied", child_d,
                                    new_run_id = "child_r1")

  expect_equal(meta$run_id, "child_r1")
  expect_equal(meta$methodology_mode, "framework_applied")
  expect_equal(meta$parent_run_id, "parent_r1")
  expect_equal(meta$mode_changed_from, "reflexive_scaffold")
  expect_false(meta$is_finalized)
  expect_true(file.exists(file.path(child_d, "run_metadata.json")))
})

test_that("clone_run_with_new_mode refuses when parent has no metadata", {
  parent_d <- withr::local_tempdir()
  child_d  <- file.path(dirname(parent_d), "child_run")
  on.exit(unlink(child_d, recursive = TRUE), add = TRUE)
  expect_error(
    clone_run_with_new_mode(parent_d, "reflexive_scaffold", child_d),
    "no run_metadata.json"
  )
})

test_that("clone_run_with_new_mode refuses when modes match (no fork is needed)", {
  parent_d <- withr::local_tempdir()
  init_run_state(parent_d, "p", "reflexive_scaffold")
  child_d <- file.path(dirname(parent_d), "child_run")
  on.exit(unlink(child_d, recursive = TRUE), add = TRUE)
  expect_error(
    clone_run_with_new_mode(parent_d, "reflexive_scaffold", child_d),
    "matches parent's mode"
  )
})

test_that("clone_run_with_new_mode validates new_mode (rejects unknown)", {
  parent_d <- withr::local_tempdir()
  init_run_state(parent_d, "p", "reflexive_scaffold")
  child_d <- file.path(dirname(parent_d), "child_run")
  on.exit(unlink(child_d, recursive = TRUE), add = TRUE)
  expect_error(
    clone_run_with_new_mode(parent_d, "free_for_all", child_d),
    "Invalid methodology"
  )
})

test_that("clone_run_with_new_mode refuses to overwrite an existing run dir", {
  parent_d <- withr::local_tempdir()
  init_run_state(parent_d, "p", "reflexive_scaffold")
  child_d  <- withr::local_tempdir()  # already exists
  expect_error(
    clone_run_with_new_mode(parent_d, "framework_applied", child_d),
    "already exists"
  )
})

# ---- Schema regression check -----------------------------------------------

test_that("init_run_state's metadata round-trips through JSON without losing fields", {
  d <- withr::local_tempdir()
  init_run_state(
    run_dir = d, run_id = "rt",
    methodology_mode  = "framework_applied",
    parent_run_id     = "rt-parent",
    mode_changed_from = "reflexive_scaffold",
    provider          = "anthropic",
    study_name        = "Round Trip"
  )
  read_back <- read_run_metadata(d)
  expect_equal(read_back$run_id, "rt")
  expect_equal(read_back$methodology_mode, "framework_applied")
  expect_equal(read_back$parent_run_id, "rt-parent")
  expect_equal(read_back$mode_changed_from, "reflexive_scaffold")
  expect_equal(read_back$provider, "anthropic")
  expect_equal(read_back$study_name, "Round Trip")
  expect_false(read_back$is_finalized)
})
