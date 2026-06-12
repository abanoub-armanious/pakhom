# Tests for checkpoint system (04_checkpoint.R)

test_that("init_checkpoints creates CheckpointManager", {
  tmp <- tempdir()
  test_dir <- file.path(tmp, "test_checkpoints")
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  mgr <- init_checkpoints(test_dir, config_hash = "abc123")
  expect_s3_class(mgr, "CheckpointManager")
  expect_true(dir.exists(mgr$checkpoint_dir))
  expect_equal(mgr$config_hash, "abc123")
})

test_that("save_checkpoint and load_checkpoint round-trip", {
  tmp <- tempdir()
  test_dir <- file.path(tmp, "test_cp_roundtrip")
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  mgr <- init_checkpoints(test_dir, config_hash = "test")

  # Save some data
  test_data <- tibble::tibble(x = 1:5, y = letters[1:5])
  save_checkpoint(mgr, "test_step", test_data)

  # Load it back
  loaded <- load_checkpoint(mgr, "test_step")
  expect_equal(loaded, test_data)
})

test_that("list_checkpoints tracks saved steps", {
  tmp <- tempdir()
  test_dir <- file.path(tmp, "test_cp_list")
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  mgr <- init_checkpoints(test_dir, config_hash = "test")
  save_checkpoint(mgr, "step_a", list(a = 1))
  save_checkpoint(mgr, "step_b", list(b = 2))

  info <- list_checkpoints(mgr)
  expect_true("step_a" %in% info$completed)
  expect_true("step_b" %in% info$completed)
})

test_that("find_resume_point returns last completed step", {
  tmp <- tempdir()
  test_dir <- file.path(tmp, "test_cp_resume")
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  mgr <- init_checkpoints(test_dir, config_hash = "test")
  save_checkpoint(mgr, "data_loaded", list(x = 1))
  save_checkpoint(mgr, "sentiment_done", list(x = 2))

  resume_point <- find_resume_point(mgr)
  expect_equal(resume_point, "sentiment_done")
})

test_that("load_checkpoint returns NULL for missing step", {
  tmp <- tempdir()
  test_dir <- file.path(tmp, "test_cp_missing")
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  mgr <- init_checkpoints(test_dir, config_hash = "test")
  result <- load_checkpoint(mgr, "nonexistent")
  expect_null(result)
})

test_that("generate_run_id produces valid string", {
  rid <- generate_run_id()
  expect_type(rid, "character")
  expect_true(nchar(rid) > 0)
  expect_true(grepl("^run_", rid))
})

test_that("find_latest_run finds timestamped run folders", {
  tmp <- file.path(tempdir(), "test_latest_run")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # No runs yet
  expect_null(find_latest_run(tmp))

  # Create some run folders
  dir.create(file.path(tmp, "run_2026-01-01_120000"), recursive = TRUE)
  dir.create(file.path(tmp, "run_2026-03-14_091500"), recursive = TRUE)
  dir.create(file.path(tmp, "run_2026-02-15_080000"), recursive = TRUE)
  dir.create(file.path(tmp, "not_a_run"), recursive = TRUE)  # should be ignored

  latest <- find_latest_run(tmp)
  expect_equal(latest, "run_2026-03-14_091500")
})

test_that("symlink detection works via base R", {
  tmp <- file.path(tempdir(), "test_symlink")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp, recursive = TRUE)

  # Regular file
  f <- file.path(tmp, "regular.txt")
  writeLines("hello", f)
  expect_false(nzchar(Sys.readlink(f)))

  # Symlink detection is OS-specific. On Windows, file.symlink() may even
  # report success yet produce a link Sys.readlink() cannot resolve (real
  # symlinks need privileges / developer-mode), so this base-R behaviour is
  # only assertable on Unix-likes. The regular-file check above runs
  # everywhere; the package degrades gracefully where symlinks are absent.
  skip_on_os("windows")
  link <- file.path(tmp, "link.txt")
  file.symlink(f, link)
  expect_true(nzchar(Sys.readlink(link)))
})

test_that("load_checkpoint returns NULL (not a crash) on a corrupt payload", {
  # A truncated/corrupt .rds is the partial-failure case resume=TRUE is meant
  # to recover from; it must be treated as 'step not done' (recompute), not
  # crash on every resume attempt.
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mgr <- init_checkpoints(tmp, config_hash = "x")
  writeLines("this is not a valid rds payload",
             file.path(mgr$checkpoint_dir, "badstep.rds"))
  expect_null(suppressWarnings(load_checkpoint(mgr, "badstep")))
})

test_that("find_resume_point surfaces a config-hash mismatch but still resumes", {
  # The 1.0.0 policy is warn-and-continue: the user opted in via resume=TRUE,
  # and the raw-YAML hash cannot distinguish substantive from cosmetic edits,
  # so a hard stop would block legitimate resumes. This test locks the policy
  # in: the mismatch IS surfaced (via the logger) AND the resume proceeds.
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mgr_a <- init_checkpoints(tmp, config_hash = "hash_A")
  save_checkpoint(mgr_a, "data_loaded", list(x = 1))

  mgr_b <- init_checkpoints(tmp, config_hash = "hash_B")
  # logger::log_warn does NOT signal an R warning condition; capture the
  # appender output instead. NOTE: logger::log_appender() (the getter)
  # returns a SYMBOL in logger >= 0.4, not a function -- restore the
  # console appender (the suite baseline) explicitly, or the failed
  # restore leaks the collector into every downstream test.
  logs <- character(0)
  logger::log_appender(function(lines) logs <<- c(logs, lines))
  # Restore the SUITE BASELINE (the silent appender), not the console appender,
  # so this test does not re-enable logger console output -- and the resulting
  # GitHub Actions annotations -- for every test that runs afterward.
  on.exit(logger::log_appender(.pakhom_test_silent_appender),
          add = TRUE, after = FALSE)

  resume <- find_resume_point(mgr_b)
  expect_true(any(grepl("Config has changed", logs)))
  expect_equal(resume, "data_loaded")
})

test_that("find_resume_point stays silent when the config hash matches", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mgr_a <- init_checkpoints(tmp, config_hash = "hash_A")
  save_checkpoint(mgr_a, "data_loaded", list(x = 1))

  mgr_a2 <- init_checkpoints(tmp, config_hash = "hash_A")
  logs <- character(0)
  logger::log_appender(function(lines) logs <<- c(logs, lines))
  # Restore the SUITE BASELINE (the silent appender), not the console appender,
  # so this test does not re-enable logger console output -- and the resulting
  # GitHub Actions annotations -- for every test that runs afterward.
  on.exit(logger::log_appender(.pakhom_test_silent_appender),
          add = TRUE, after = FALSE)

  resume <- find_resume_point(mgr_a2)
  expect_false(any(grepl("Config has changed", logs)))
  expect_equal(resume, "data_loaded")
})

test_that("find_resume_point tolerates an NA config hash (missing config file)", {
  # hash_config() returns NA_character_ when the config file is absent; an
  # unknowable hash is not a known mismatch, and `if (NA)` must not crash.
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mgr_a <- init_checkpoints(tmp, config_hash = "hash_A")
  save_checkpoint(mgr_a, "data_loaded", list(x = 1))

  mgr_na <- init_checkpoints(tmp, config_hash = NA_character_)
  expect_no_error(resume <- find_resume_point(mgr_na))
  expect_equal(resume, "data_loaded")
})
