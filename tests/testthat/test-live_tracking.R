# Tests for the Phase 53 LiveTracker (R/live_tracking.R) — three streamed/
# snapshot artifacts written during analysis so a researcher can `tail -F`
# or `cat` files in the run directory and watch the codebook + theme
# clustering grow in real time.

test_that("init_live_tracker creates 3 files with valid placeholder content", {
  tmp_dir <- withr::local_tempdir()
  tracker <- init_live_tracker(tmp_dir)

  expect_s3_class(tracker, "LiveTracker")
  expect_true(file.exists(tracker$paths$assignments))
  expect_true(file.exists(tracker$paths$codebook_snapshot))
  expect_true(file.exists(tracker$paths$cluster_snapshot))

  # JSONL events file starts empty
  expect_equal(file.size(tracker$paths$assignments), 0)

  # JSON snapshots are valid JSON with placeholder content
  cb <- jsonlite::fromJSON(tracker$paths$codebook_snapshot)
  expect_equal(cb$n_codes, 0L)
  expect_equal(cb$schema_version, "1.0.0")

  cl <- jsonlite::fromJSON(tracker$paths$cluster_snapshot)
  expect_equal(cl$walk_status, "not_started")
  expect_equal(cl$n_decisions, 0L)
})

test_that("init_live_tracker rejects empty output_dir", {
  expect_error(init_live_tracker(""),     "non-empty path")
  expect_error(init_live_tracker(NULL),   "non-empty path")
})

test_that("live_record_assignment appends JSONL line with expected fields", {
  tmp_dir <- withr::local_tempdir()
  tracker <- init_live_tracker(tmp_dir)

  seg <- list(
    text = "I take my medication every morning.",
    start_char = 0L, end_char = 35L,
    provenance = list(verification_status = "verified")
  )
  tracker <- live_record_assignment(
    tracker, entry_id = "e1", code_key = "med_routine",
    code_name = "Medication routine",
    segment = seg, is_new_code = TRUE, entry_index = 1L
  )

  expect_equal(tracker$counters$n_assignments, 1L)
  lines <- readLines(tracker$paths$assignments)
  expect_length(lines, 1L)
  rec <- jsonlite::fromJSON(lines[[1]])
  expect_equal(rec$event_type, "code_assignment")
  expect_equal(rec$entry_id, "e1")
  expect_equal(rec$code_key, "med_routine")
  expect_equal(rec$code_name, "Medication routine")
  expect_true(rec$is_new_code)
  expect_equal(rec$segment$text, seg$text)
  expect_equal(rec$segment$start_char, 0L)
  expect_equal(rec$segment$verification_status, "verified")
})

test_that("live_snapshot_codebook atomically rewrites JSON", {
  tmp_dir <- withr::local_tempdir()
  tracker <- init_live_tracker(tmp_dir)

  codebook <- list(
    med_helps = list(
      code_name = "Medication helps", description = "general efficacy",
      type = "descriptive", frequency = 5L, entry_ids = c("e1", "e2"),
      coded_segments = list(list(text = "x"), list(text = "y"))
    ),
    side_effects = list(
      code_name = "Side effects", description = "",
      type = "descriptive", frequency = 2L, entry_ids = c("e3"),
      coded_segments = list(list(text = "z"))
    )
  )
  tracker <- live_snapshot_codebook(tracker, codebook, entry_index = 5L,
                                       force = TRUE)

  expect_equal(tracker$counters$n_codebook_snapshots, 1L)
  cb <- jsonlite::fromJSON(tracker$paths$codebook_snapshot)
  expect_equal(cb$n_codes, 2L)
  expect_equal(cb$last_entry_index, 5L)
  expect_setequal(cb$codes$key, c("med_helps", "side_effects"))
  expect_setequal(cb$codes$frequency[cb$codes$key == "med_helps"], 5L)
  expect_equal(cb$codes$n_segments[cb$codes$key == "med_helps"], 2L)
})

test_that("live_snapshot_clusters records walk state + decisions", {
  tmp_dir <- withr::local_tempdir()
  tracker <- init_live_tracker(tmp_dir)

  walk_state <- new.env(parent = emptyenv())
  walk_state$n_calls <- 3L
  walk_state$n_failed_calls <- 0L
  walk_state$decisions <- list(
    list(call_idx = 1L, level = "THEME", parent = NA_character_,
         n_codes = 50L, decision = "split_required",
         articulation = "no single principle works",
         name = NA_character_, rationale = "..."),
    list(call_idx = 2L, level = "THEME", parent = NA_character_,
         n_codes = 25L, decision = "coherent_theme",
         articulation = "Strategies for managing the daily medication routine",
         name = "Daily medication routines", rationale = "...")
  )

  themes <- list(
    list(name = "Daily medication routines", description = "...",
         decision_origin = "coherent_theme",
         code_indices = c(1L, 2L, 3L),
         code_keys = c("a", "b", "c"))
  )

  tracker <- live_snapshot_clusters(tracker, walk_status = "in_progress",
                                       walk_state = walk_state,
                                       themes_so_far = themes)

  expect_equal(tracker$counters$n_cluster_snapshots, 1L)
  cl <- jsonlite::fromJSON(tracker$paths$cluster_snapshot)
  expect_equal(cl$walk_status, "in_progress")
  expect_equal(cl$n_decisions, 2L)
  expect_equal(cl$n_themes, 1L)
  expect_equal(cl$n_calls, 3L)
  expect_setequal(cl$themes$name, "Daily medication routines")
})

test_that("NULL tracker is a no-op for all writers", {
  # Back-compat / opt-out: passing tracker = NULL must not error.
  expect_null(live_record_assignment(NULL, "e1", "k", "name", list(), FALSE, 1L))
  expect_null(live_snapshot_codebook(NULL, list()))
  expect_null(live_snapshot_clusters(NULL, walk_status = "in_progress"))
})

test_that("LiveTracker print method shows current counts", {
  tmp_dir <- withr::local_tempdir()
  tracker <- init_live_tracker(tmp_dir)
  out <- capture.output(print(tracker))
  expect_true(any(grepl("LiveTracker", out)))
  expect_true(any(grepl("0 assignments", out)))
})

test_that("LiveTracker counters persist across calls without caller reassignment", {
  # Phase 53 audit CRITICAL-1: counters live in tracker$counters (env)
  # so writers can mutate them in place. Without this fix, each
  # live_*() call would reset the counter to its initial value (the
  # on-disk file would still be written -- atomic-rewrite is a side-
  # effect -- but the print method + downstream debug logs would lie).
  tmp_dir <- withr::local_tempdir()
  tracker <- init_live_tracker(tmp_dir)

  seg <- list(text = "x", start_char = 0L, end_char = 1L,
              provenance = list(verification_status = "verified"))

  # Three calls without reassignment
  live_record_assignment(tracker, "e1", "a", "Code A", seg)
  live_record_assignment(tracker, "e2", "a", "Code A", seg)
  live_record_assignment(tracker, "e3", "b", "Code B", seg)

  # Counter persisted across all three calls
  expect_equal(tracker$counters$n_assignments, 3L)
  expect_length(readLines(tracker$paths$assignments), 3L)
})

test_that("LiveTracker codebook snapshot cadence gates on call count", {
  # cadence = 3 means write on calls 3, 6, 9, ...
  tmp_dir <- withr::local_tempdir()
  tracker <- init_live_tracker(tmp_dir, codebook_snapshot_every = 3L)

  cb <- list(a = list(code_name = "A", description = "", type = "descriptive",
                       frequency = 1L, entry_ids = "e1", coded_segments = list()))

  # First two calls should not advance snapshot_index
  live_snapshot_codebook(tracker, cb, entry_index = 1L)
  live_snapshot_codebook(tracker, cb, entry_index = 2L)
  # The on-disk file is the placeholder (snapshot_index from init)
  payload2 <- jsonlite::fromJSON(tracker$paths$codebook_snapshot)
  expect_equal(payload2$snapshot_index, 0L)  # placeholder

  # Third call hits the cadence -> writes
  live_snapshot_codebook(tracker, cb, entry_index = 3L)
  payload3 <- jsonlite::fromJSON(tracker$paths$codebook_snapshot)
  expect_equal(payload3$snapshot_index, 3L)
  expect_equal(payload3$last_entry_index, 3L)

  # Counter tracks calls (not writes)
  expect_equal(tracker$counters$n_codebook_snapshots, 3L)
})
