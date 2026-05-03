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

  # Symlink
  link <- file.path(tmp, "link.txt")
  file.symlink(f, link)
  expect_true(nzchar(Sys.readlink(link)))
})
