# ==============================================================================
# Tests for researcher review mechanisms (19_researcher_review.R)
# ==============================================================================

# -- Helper: build a minimal ProgressiveCodingState ----------------------------

make_coding_state <- function() {
  coding_state <- list(
    codebook = list(
      code_a = list(
        code_name = "Code A", description = "Desc A", type = "descriptive",
        frequency = 3L, entry_ids = c("e1", "e2", "e3"),
        coded_segments = list(
          list(text = "segment 1", code_key = "code_a", code_name = "Code A",
               start_char = 0L, end_char = 10L)
        )
      ),
      code_b = list(
        code_name = "Code B", description = "Desc B", type = "process",
        frequency = 2L, entry_ids = c("e1", "e4"),
        coded_segments = list(
          list(text = "segment 2", code_key = "code_b", code_name = "Code B",
               start_char = 0L, end_char = 10L)
        )
      )
    ),
    entry_results = list(
      e1 = list(
        codes_assigned = c("code_a", "code_b"), skipped = FALSE,
        coded_segments = list(
          list(text = "segment 1", code_key = "code_a", code_name = "Code A",
               start_char = 0L, end_char = 10L),
          list(text = "segment 2", code_key = "code_b", code_name = "Code B",
               start_char = 11L, end_char = 20L)
        )
      ),
      e2 = list(
        codes_assigned = c("code_a"), skipped = FALSE,
        coded_segments = list(
          list(text = "segment 3", code_key = "code_a", code_name = "Code A",
               start_char = 0L, end_char = 10L)
        )
      ),
      e3 = list(
        codes_assigned = c("code_a"), skipped = FALSE,
        coded_segments = list(
          list(text = "segment 4", code_key = "code_a", code_name = "Code A",
               start_char = 0L, end_char = 10L)
        )
      ),
      e4 = list(
        codes_assigned = c("code_b"), skipped = FALSE,
        coded_segments = list(
          list(text = "segment 5", code_key = "code_b", code_name = "Code B",
               start_char = 0L, end_char = 10L)
        )
      )
    ),
    entries_processed = 1:4
  )
  class(coding_state) <- "ProgressiveCodingState"
  coding_state
}

# -- Helper: write a reviewed codebook CSV with specific actions ---------------

write_reviewed_codebook <- function(dir, rows) {
  # rows is a data.frame / tibble with columns matching the export schema
  review_dir <- file.path(dir, "researcher_review")
  dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(rows, file.path(review_dir, "codebook_reviewed.csv"))
}

# -- Helper: write a reviewed themes CSV with specific actions -----------------

write_reviewed_themes <- function(dir, rows) {
  review_dir <- file.path(dir, "researcher_review")
  dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(rows, file.path(review_dir, "themes_reviewed.csv"))
}

# ==============================================================================
# 1. Codebook review -- basic actions
# ==============================================================================

test_that("review_progressive_codebook exports CSV with correct columns", {
  withr::with_tempdir({
    cs <- make_coding_state()
    result <- review_progressive_codebook(cs, getwd())

    expect_equal(result$status, "exported")
    export_path <- file.path("researcher_review", "codebook_review.csv")
    expect_true(file.exists(export_path))

    df <- readr::read_csv(export_path, show_col_types = FALSE)
    expected_cols <- c(
      "code_key", "code_name", "description", "frequency", "n_entries",
      "example_segment", "action", "new_name", "merge_into",
      "new_description", "split_name", "researcher_memo",
      "irr_agreement", "irr_flag"
    )
    expect_true(all(expected_cols %in% names(df)))
    expect_equal(nrow(df), 2L)
  })
})

test_that("codebook review applies rename", {
  withr::with_tempdir({
    cs <- make_coding_state()

    reviewed <- tibble::tibble(
      code_key       = c("code_a", "code_b"),
      code_name      = c("Code A", "Code B"),
      action         = c("rename", "keep"),
      new_name       = c("Renamed A", NA_character_),
      merge_into     = NA_character_,
      new_description = NA_character_,
      split_name     = NA_character_,
      researcher_memo = NA_character_
    )
    write_reviewed_codebook(getwd(), reviewed)

    result <- review_progressive_codebook(cs, getwd())
    expect_equal(result$status, "applied")
    expect_equal(result$coding_state$codebook$code_a$code_name, "Renamed A")
    # code_b unchanged
    expect_equal(result$coding_state$codebook$code_b$code_name, "Code B")
  })
})

test_that("codebook review applies delete", {
  withr::with_tempdir({
    cs <- make_coding_state()

    reviewed <- tibble::tibble(
      code_key       = c("code_a", "code_b"),
      code_name      = c("Code A", "Code B"),
      action         = c("delete", "keep"),
      new_name       = NA_character_,
      merge_into     = NA_character_,
      new_description = NA_character_,
      split_name     = NA_character_,
      researcher_memo = NA_character_
    )
    write_reviewed_codebook(getwd(), reviewed)

    result <- review_progressive_codebook(cs, getwd())
    expect_equal(result$status, "applied")

    # code_a removed from codebook
    expect_null(result$coding_state$codebook[["code_a"]])
    expect_false(is.null(result$coding_state$codebook[["code_b"]]))

    # code_a removed from entry_results
    for (eid in names(result$coding_state$entry_results)) {
      er <- result$coding_state$entry_results[[eid]]
      expect_false("code_a" %in% er$codes_assigned)
      seg_keys <- vapply(er$coded_segments, function(s) s$code_key, character(1))
      expect_false("code_a" %in% seg_keys)
    }
  })
})

test_that("codebook review applies merge", {
  withr::with_tempdir({
    cs <- make_coding_state()

    reviewed <- tibble::tibble(
      code_key       = c("code_a", "code_b"),
      code_name      = c("Code A", "Code B"),
      action         = c("merge", "keep"),
      new_name       = NA_character_,
      merge_into     = c("code_b", NA_character_),
      new_description = NA_character_,
      split_name     = NA_character_,
      researcher_memo = NA_character_
    )
    write_reviewed_codebook(getwd(), reviewed)

    result <- review_progressive_codebook(cs, getwd())
    expect_equal(result$status, "applied")

    # code_a removed, code_b absorbs its frequency
    expect_null(result$coding_state$codebook[["code_a"]])
    merged <- result$coding_state$codebook[["code_b"]]
    expect_equal(merged$frequency, 5L)  # 3 + 2
    expect_true(all(c("e1", "e2", "e3", "e4") %in% merged$entry_ids))

    # entry_results for e1 should now have code_b (not code_a)
    e1 <- result$coding_state$entry_results[["e1"]]
    expect_true("code_b" %in% e1$codes_assigned)
    expect_false("code_a" %in% e1$codes_assigned)
  })
})

test_that("codebook review applies description update", {
  withr::with_tempdir({
    cs <- make_coding_state()

    reviewed <- tibble::tibble(
      code_key       = c("code_a", "code_b"),
      code_name      = c("Code A", "Code B"),
      action         = c("keep", "keep"),
      new_name       = NA_character_,
      merge_into     = NA_character_,
      new_description = c("Updated description for A", NA_character_),
      split_name     = NA_character_,
      researcher_memo = NA_character_
    )
    write_reviewed_codebook(getwd(), reviewed)

    result <- review_progressive_codebook(cs, getwd())
    expect_equal(result$status, "applied")
    expect_equal(result$coding_state$codebook$code_a$description,
                 "Updated description for A")
    # code_b description unchanged
    expect_equal(result$coding_state$codebook$code_b$description, "Desc B")
  })
})

# ==============================================================================
# 2. Codebook review -- new capabilities
# ==============================================================================

test_that("codebook review applies split", {
  withr::with_tempdir({
    cs <- make_coding_state()

    reviewed <- tibble::tibble(
      code_key       = c("code_a", "code_b"),
      code_name      = c("Code A", "Code B"),
      action         = c("split", "keep"),
      new_name       = c("Code A Part 1", NA_character_),
      merge_into     = NA_character_,
      new_description = NA_character_,
      split_name     = c("Code A Part 2", NA_character_),
      researcher_memo = NA_character_
    )
    write_reviewed_codebook(getwd(), reviewed)

    result <- review_progressive_codebook(cs, getwd())
    expect_equal(result$status, "applied")

    # Original key renamed
    expect_equal(result$coding_state$codebook$code_a$code_name, "Code A Part 1")

    # New split code created
    split_key <- "code_a_part_2"
    expect_false(is.null(result$coding_state$codebook[[split_key]]))
    expect_equal(result$coding_state$codebook[[split_key]]$code_name, "Code A Part 2")

    # Both codes should exist in entry_results for entries that had code_a
    e1 <- result$coding_state$entry_results[["e1"]]
    expect_true("code_a" %in% e1$codes_assigned)
    expect_true(split_key %in% e1$codes_assigned)

    # Split code segments duplicated
    split_segs <- Filter(function(s) s$code_key == split_key, e1$coded_segments)
    expect_true(length(split_segs) > 0)
  })
})

test_that("codebook review stores researcher memo", {
  withr::with_tempdir({
    cs <- make_coding_state()

    reviewed <- tibble::tibble(
      code_key       = c("code_a", "code_b"),
      code_name      = c("Code A", "Code B"),
      action         = c("keep", "keep"),
      new_name       = NA_character_,
      merge_into     = NA_character_,
      new_description = NA_character_,
      split_name     = NA_character_,
      researcher_memo = c("This code needs further refinement", NA_character_)
    )
    write_reviewed_codebook(getwd(), reviewed)

    result <- review_progressive_codebook(cs, getwd())
    expect_equal(result$status, "applied")
    expect_equal(result$coding_state$codebook$code_a$researcher_memo,
                 "This code needs further refinement")
    expect_null(result$coding_state$codebook$code_b$researcher_memo)
  })
})

test_that("codebook review includes IRR agreement when provided", {
  withr::with_tempdir({
    cs <- make_coding_state()

    irr_result <- list(
      per_code_agreement = list(
        "Code A" = 0.85,
        "Code B" = 0.45
      )
    )

    result <- review_progressive_codebook(cs, getwd(), irr_result = irr_result)
    expect_equal(result$status, "exported")

    export_path <- file.path("researcher_review", "codebook_review.csv")
    df <- readr::read_csv(export_path, show_col_types = FALSE)

    # Row for Code A (high agreement)
    row_a <- df[df$code_name == "Code A", ]
    expect_equal(row_a$irr_agreement, 0.85)
    expect_true(is.na(row_a$irr_flag) || row_a$irr_flag == "")

    # Row for Code B (low agreement)
    row_b <- df[df$code_name == "Code B", ]
    expect_equal(row_b$irr_agreement, 0.45)
    expect_equal(row_b$irr_flag, "LOW_AGREEMENT")
  })
})

# ==============================================================================
# 3. Theme review -- basic actions
# ==============================================================================

test_that("review_themes exports CSV with correct columns", {
  withr::with_tempdir({
    themes <- list(
      list(id = 1L, name = "Theme A", description = "Desc A",
           codes_included = c("Code A")),
      list(id = 2L, name = "Theme B", description = "Desc B",
           codes_included = c("Code B"))
    )
    ts <- create_theme_set(themes)

    result <- review_themes(ts, getwd())
    expect_equal(result$status, "exported")

    export_path <- file.path("researcher_review", "themes_review.csv")
    expect_true(file.exists(export_path))

    df <- readr::read_csv(export_path, show_col_types = FALSE)
    expected_cols <- c(
      "theme_name", "description", "codes_included", "action",
      "new_name", "new_description", "merge_into",
      "codes_to_add", "codes_to_remove", "split_into", "researcher_memo"
    )
    expect_true(all(expected_cols %in% names(df)))
    expect_equal(nrow(df), 2L)
  })
})

test_that("theme review applies rename", {
  withr::with_tempdir({
    themes <- list(
      list(id = 1L, name = "Theme A", description = "Desc A",
           codes_included = c("Code A")),
      list(id = 2L, name = "Theme B", description = "Desc B",
           codes_included = c("Code B"))
    )
    ts <- create_theme_set(themes)

    reviewed <- tibble::tibble(
      theme_name      = c("Theme A", "Theme B"),
      description     = c("Desc A", "Desc B"),
      codes_included  = c("Code A", "Code B"),
      action          = c("keep", "keep"),
      new_name        = c("Renamed Theme A", NA_character_),
      new_description = NA_character_,
      merge_into      = NA_character_,
      codes_to_add    = NA_character_,
      codes_to_remove = NA_character_,
      split_into      = NA_character_,
      researcher_memo = NA_character_
    )
    write_reviewed_themes(getwd(), reviewed)

    result <- review_themes(ts, getwd())
    expect_equal(result$status, "applied")
    names_out <- theme_names(result$theme_set)
    expect_true("Renamed Theme A" %in% names_out)
    expect_false("Theme A" %in% names_out)
    expect_true("Theme B" %in% names_out)
  })
})

test_that("theme review applies delete", {
  withr::with_tempdir({
    themes <- list(
      list(id = 1L, name = "Theme A", description = "Desc A",
           codes_included = c("Code A")),
      list(id = 2L, name = "Theme B", description = "Desc B",
           codes_included = c("Code B"))
    )
    ts <- create_theme_set(themes)

    reviewed <- tibble::tibble(
      theme_name      = c("Theme A", "Theme B"),
      description     = c("Desc A", "Desc B"),
      codes_included  = c("Code A", "Code B"),
      action          = c("delete", "keep"),
      new_name        = NA_character_,
      new_description = NA_character_,
      merge_into      = NA_character_,
      codes_to_add    = NA_character_,
      codes_to_remove = NA_character_,
      split_into      = NA_character_,
      researcher_memo = NA_character_
    )
    write_reviewed_themes(getwd(), reviewed)

    result <- review_themes(ts, getwd())
    expect_equal(result$status, "applied")
    expect_equal(n_themes(result$theme_set), 1L)
    expect_equal(theme_names(result$theme_set), "Theme B")
  })
})

test_that("theme review applies merge", {
  withr::with_tempdir({
    themes <- list(
      list(id = 1L, name = "Theme A", description = "Desc A",
           codes_included = c("Code A")),
      list(id = 2L, name = "Theme B", description = "Desc B",
           codes_included = c("Code B"))
    )
    ts <- create_theme_set(themes)

    reviewed <- tibble::tibble(
      theme_name      = c("Theme A", "Theme B"),
      description     = c("Desc A", "Desc B"),
      codes_included  = c("Code A", "Code B"),
      action          = c("merge", "keep"),
      new_name        = NA_character_,
      new_description = NA_character_,
      merge_into      = c("Theme B", NA_character_),
      codes_to_add    = NA_character_,
      codes_to_remove = NA_character_,
      split_into      = NA_character_,
      researcher_memo = NA_character_
    )
    write_reviewed_themes(getwd(), reviewed)

    result <- review_themes(ts, getwd())
    expect_equal(result$status, "applied")
    expect_equal(n_themes(result$theme_set), 1L)

    # Theme B should now contain codes from both themes
    tb <- result$theme_set$themes[[1]]
    expect_true("Code A" %in% tb$codes_included)
    expect_true("Code B" %in% tb$codes_included)
  })
})

# ==============================================================================
# 4. Theme review -- new capabilities
# ==============================================================================

test_that("theme review applies code reassignment", {
  withr::with_tempdir({
    themes <- list(
      list(id = 1L, name = "Theme A", description = "Desc A",
           codes_included = c("Code A", "Code C")),
      list(id = 2L, name = "Theme B", description = "Desc B",
           codes_included = c("Code B"))
    )
    ts <- create_theme_set(themes)

    reviewed <- tibble::tibble(
      theme_name      = c("Theme A", "Theme B"),
      description     = c("Desc A", "Desc B"),
      codes_included  = c("Code A; Code C", "Code B"),
      action          = c("keep", "keep"),
      new_name        = NA_character_,
      new_description = NA_character_,
      merge_into      = NA_character_,
      codes_to_add    = c(NA_character_, "Code D"),
      codes_to_remove = c("Code C", NA_character_),
      split_into      = NA_character_,
      researcher_memo = NA_character_
    )
    write_reviewed_themes(getwd(), reviewed)

    result <- review_themes(ts, getwd())
    expect_equal(result$status, "applied")

    ta <- result$theme_set$themes[[which(theme_names(result$theme_set) == "Theme A")]]
    tb <- result$theme_set$themes[[which(theme_names(result$theme_set) == "Theme B")]]

    # Code C removed from Theme A
    expect_false("Code C" %in% ta$codes_included)
    expect_true("Code A" %in% ta$codes_included)

    # Code D added to Theme B
    expect_true("Code D" %in% tb$codes_included)
    expect_true("Code B" %in% tb$codes_included)
  })
})

test_that("theme review creates new theme", {
  withr::with_tempdir({
    themes <- list(
      list(id = 1L, name = "Theme A", description = "Desc A",
           codes_included = c("Code A"))
    )
    ts <- create_theme_set(themes)

    reviewed <- tibble::tibble(
      theme_name      = c("Theme A", "New Theme"),
      description     = c("Desc A", ""),
      codes_included  = c("Code A", "Code X; Code Y"),
      action          = c("keep", "create"),
      new_name        = NA_character_,
      new_description = c(NA_character_, "A brand new theme"),
      merge_into      = NA_character_,
      codes_to_add    = NA_character_,
      codes_to_remove = NA_character_,
      split_into      = NA_character_,
      researcher_memo = NA_character_
    )
    write_reviewed_themes(getwd(), reviewed)

    result <- review_themes(ts, getwd())
    expect_equal(result$status, "applied")
    expect_equal(n_themes(result$theme_set), 2L)

    names_out <- theme_names(result$theme_set)
    expect_true("New Theme" %in% names_out)

    new_t <- result$theme_set$themes[[which(names_out == "New Theme")]]
    expect_equal(new_t$description, "A brand new theme")
    expect_true(all(c("Code X", "Code Y") %in% new_t$codes_included))
  })
})

test_that("theme review splits theme", {
  withr::with_tempdir({
    themes <- list(
      list(id = 1L, name = "Theme A", description = "Desc A",
           codes_included = c("Code 1", "Code 2", "Code 3")),
      list(id = 2L, name = "Theme B", description = "Desc B",
           codes_included = c("Code B"))
    )
    ts <- create_theme_set(themes)

    reviewed <- tibble::tibble(
      theme_name      = c("Theme A", "Theme B"),
      description     = c("Desc A", "Desc B"),
      codes_included  = c("Code 1; Code 2; Code 3", "Code B"),
      action          = c("split", "keep"),
      new_name        = c("Theme A1", NA_character_),
      new_description = c("First half", NA_character_),
      merge_into      = NA_character_,
      codes_to_add    = NA_character_,
      codes_to_remove = c("Code 3", NA_character_),
      split_into      = c("Theme A2", NA_character_),
      researcher_memo = NA_character_
    )
    write_reviewed_themes(getwd(), reviewed)

    result <- review_themes(ts, getwd())
    expect_equal(result$status, "applied")

    names_out <- theme_names(result$theme_set)
    expect_true("Theme A1" %in% names_out)
    expect_true("Theme A2" %in% names_out)
    expect_true("Theme B" %in% names_out)

    t_a1 <- result$theme_set$themes[[which(names_out == "Theme A1")]]
    t_a2 <- result$theme_set$themes[[which(names_out == "Theme A2")]]

    # Theme A1 keeps codes 1 and 2, loses code 3
    expect_true(all(c("Code 1", "Code 2") %in% t_a1$codes_included))
    expect_false("Code 3" %in% t_a1$codes_included)
    expect_equal(t_a1$description, "First half")

    # Theme A2 gets Code 3
    expect_true("Code 3" %in% t_a2$codes_included)
  })
})

test_that("theme review stores researcher memo", {
  withr::with_tempdir({
    themes <- list(
      list(id = 1L, name = "Theme A", description = "Desc A",
           codes_included = c("Code A")),
      list(id = 2L, name = "Theme B", description = "Desc B",
           codes_included = c("Code B"))
    )
    ts <- create_theme_set(themes)

    reviewed <- tibble::tibble(
      theme_name      = c("Theme A", "Theme B"),
      description     = c("Desc A", "Desc B"),
      codes_included  = c("Code A", "Code B"),
      action          = c("keep", "keep"),
      new_name        = NA_character_,
      new_description = NA_character_,
      merge_into      = NA_character_,
      codes_to_add    = NA_character_,
      codes_to_remove = NA_character_,
      split_into      = NA_character_,
      researcher_memo = c("Important theme to watch", NA_character_)
    )
    write_reviewed_themes(getwd(), reviewed)

    result <- review_themes(ts, getwd())
    expect_equal(result$status, "applied")

    ta <- result$theme_set$themes[[which(theme_names(result$theme_set) == "Theme A")]]
    expect_equal(ta$researcher_memo, "Important theme to watch")
  })
})

# ==============================================================================
# 5. Disposition
# ==============================================================================

test_that("read_review_disposition returns continue by default", {
  withr::with_tempdir({
    # No disposition file exists
    result <- read_review_disposition(getwd())
    expect_equal(result, "continue")
  })
})

test_that("read_review_disposition reads revise_codebook", {
  withr::with_tempdir({
    review_dir <- file.path(getwd(), "researcher_review")
    dir.create(review_dir, recursive = TRUE)
    disp_df <- tibble::tibble(disposition = "revise_codebook")
    readr::write_csv(disp_df, file.path(review_dir, "review_disposition.csv"))

    result <- read_review_disposition(getwd())
    expect_equal(result, "revise_codebook")
  })
})

test_that("review_themes exports disposition CSV", {
  withr::with_tempdir({
    themes <- list(
      list(id = 1L, name = "Theme A", description = "Desc A",
           codes_included = c("Code A"))
    )
    ts <- create_theme_set(themes)

    review_themes(ts, getwd())

    disp_path <- file.path("researcher_review", "review_disposition.csv")
    expect_true(file.exists(disp_path))

    disp <- readr::read_csv(disp_path, show_col_types = FALSE)
    expect_equal(disp$disposition[1], "continue")
  })
})

# ==============================================================================
# 6. Checkpoint rollback
# ==============================================================================

test_that("invalidate_checkpoints_from removes downstream checkpoints", {
  withr::with_tempdir({
    manager <- init_checkpoints(getwd())

    # Create fake checkpoint files and manifest entries
    cp_dir <- file.path(getwd(), "checkpoints")
    steps <- c("data_loaded", "progressive_coding", "sentiment_done",
               "themes_generated", "correlations")
    manifest <- list(steps = list())
    for (step in steps) {
      saveRDS(list(data = step), file.path(cp_dir, paste0(step, ".rds")))
      manifest$steps[[step]] <- list(completed_at = Sys.time())
    }
    saveRDS(manifest, manager$manifest_file)

    # Invalidate from themes_generated onward
    manager <- invalidate_checkpoints_from(manager, "themes_generated")

    # themes_generated and correlations should be removed
    expect_false(file.exists(file.path(cp_dir, "themes_generated.rds")))
    expect_false(file.exists(file.path(cp_dir, "correlations.rds")))

    # Earlier steps should remain
    expect_true(file.exists(file.path(cp_dir, "data_loaded.rds")))
    expect_true(file.exists(file.path(cp_dir, "progressive_coding.rds")))
    expect_true(file.exists(file.path(cp_dir, "sentiment_done.rds")))
  })
})

# ==============================================================================
# 7. rebuild_code_to_theme_map
# ==============================================================================

test_that("rebuild_code_to_theme_map correctly maps codes to themes", {
  cs <- make_coding_state()

  themes <- list(
    list(id = 1L, name = "Theme A", description = "Desc A",
         codes_included = c("Code A")),
    list(id = 2L, name = "Theme B", description = "Desc B",
         codes_included = c("Code B"))
  )
  ts <- create_theme_set(themes)

  result <- rebuild_code_to_theme_map(ts, cs)
  c2t <- result$merge_history$code_to_theme_map

  expect_equal(c2t[["code_a"]], "Theme A")
  expect_equal(c2t[["code_b"]], "Theme B")
})

# ==============================================================================
# 8. Audit logging
# ==============================================================================

test_that("codebook review logs decisions to audit log", {
  withr::with_tempdir({
    cs <- make_coding_state()

    audit <- init_audit_log(getwd())

    reviewed <- tibble::tibble(
      code_key       = c("code_a", "code_b"),
      code_name      = c("Code A", "Code B"),
      action         = c("delete", "rename"),
      new_name       = c(NA_character_, "Better Name B"),
      merge_into     = NA_character_,
      new_description = NA_character_,
      split_name     = NA_character_,
      researcher_memo = NA_character_
    )
    write_reviewed_codebook(getwd(), reviewed)

    result <- review_progressive_codebook(cs, getwd(), audit_log = audit)
    expect_equal(result$status, "applied")

    # Close the audit log connection before reading
    close(audit$con)

    log_path <- file.path(getwd(), "ai_decisions.jsonl")
    expect_true(file.exists(log_path))

    lines <- readLines(log_path)
    expect_true(length(lines) >= 2L)

    # Parse JSONL entries
    entries <- lapply(lines, jsonlite::fromJSON)
    decision_types <- vapply(entries, function(e) e$decision_type, character(1))

    expect_true("code_deleted" %in% decision_types)
    expect_true("code_renamed" %in% decision_types)
  })
})
