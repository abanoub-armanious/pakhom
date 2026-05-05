# ==============================================================================
# Tests for data loading + column detection (R/07_data_loading.R)
# ==============================================================================
# Phase 39 finding: the default reddit column-mapping had `id` candidates
# ordered as `c("post_id", "comment_id", "id")`, so a comments table with
# BOTH columns matched id->post_id (the parent's id, not the row's own id).
# Result: every comment under the same parent post collapsed onto one
# std_id, silently corrupting coding_state$entry_results, the fabrication
# log, the per-theme exports, and quote provenance lookups.
#
# These tests pin the fixed ordering AND the duplicate-std_id guard in
# load_and_combine_tables that catches the same class of bug regardless
# of why std_id duplicates arise.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Column detection: id picks comment_id when both are present
# ------------------------------------------------------------------------------

test_that("detect_columns picks comment_id over post_id when both are present (reddit)", {
  comments_df <- tibble::tibble(
    comment_id = c("c1", "c2", "c3"),
    post_id    = c("p1", "p1", "p2"),  # parent id; would collapse if picked
    author     = c("u1", "u2", "u3"),
    comment_body = c("a", "b", "c")
  )
  mapping <- detect_columns(comments_df, source_type = "reddit")
  expect_equal(mapping$id, "comment_id")
  expect_equal(mapping$text, "comment_body")
})

test_that("detect_columns picks post_id when comment_id is absent (posts table)", {
  posts_df <- tibble::tibble(
    post_id = c("p1", "p2"),
    author  = c("u1", "u2"),
    text    = c("hello", "world")
  )
  mapping <- detect_columns(posts_df, source_type = "reddit")
  expect_equal(mapping$id, "post_id")
  expect_equal(mapping$text, "text")
})

test_that("explicit_columns override beats the auto-detection", {
  df <- tibble::tibble(
    comment_id = c("c1", "c2"),
    post_id    = c("p1", "p2"),
    text       = c("a", "b")
  )
  mapping <- detect_columns(df, source_type = "reddit",
                             config = list(data = list(explicit_columns = list(
                               id_column = "post_id",
                               text_column = "text"
                             ))))
  # The user explicitly asked for post_id -- honor it
  expect_equal(mapping$id, "post_id")
})

# ------------------------------------------------------------------------------
# 2. load_and_combine_tables: duplicate std_id guard
# ------------------------------------------------------------------------------

# Build a fixture SQLite db with deliberately-colliding ids across
# posts and comments to exercise the auto-prefix recovery path.
.build_fixture_db_with_collisions <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  posts <- data.frame(
    post_id = c("a1", "a2", "a3"),
    text    = c("post a1 body", "post a2 body", "post a3 body"),
    author  = c("u1", "u2", "u3"),
    stringsAsFactors = FALSE
  )
  # Comments table has the canonical Reddit shape: both comment_id and
  # post_id columns. With the FIXED ordering, comment_id wins.
  comments <- data.frame(
    comment_id   = c("c1", "c2", "c3"),
    post_id      = c("a1", "a1", "a2"),
    comment_body = c("comment one", "comment two", "comment three"),
    author       = c("u4", "u5", "u6"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "posts", posts)
  DBI::dbWriteTable(con, "comments", comments)
  invisible(NULL)
}

test_that("load_and_combine_tables produces unique std_ids (post-fix happy path)", {
  td <- withr::local_tempdir()
  db_path <- file.path(td, "test.db")
  .build_fixture_db_with_collisions(db_path)
  combined <- load_and_combine_tables(db_path, c("posts", "comments"),
                                       source_type = "reddit")
  # 3 posts + 3 comments = 6 rows; all std_ids must be unique now that
  # comments map to comment_id, not post_id.
  expect_equal(nrow(combined), 6L)
  expect_equal(length(unique(combined$std_id)), 6L)
  expect_equal(sort(combined$std_id),
               sort(c("a1", "a2", "a3", "c1", "c2", "c3")))
})

test_that("load_and_combine_tables auto-prefixes with source_table when explicit_columns force a collision", {
  td <- withr::local_tempdir()
  db_path <- file.path(td, "test.db")
  # Build a db where both tables have a literal `parent_id` column with
  # values that DELIBERATELY collide. With explicit_columns forcing
  # id_column = "parent_id", standardize_data uses it -> duplicates flow
  # to combined$std_id -> the guard must auto-prefix with source_table.
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(con, "posts", data.frame(
    parent_id = c("x1", "x2"), text = c("p1", "p2"),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "comments", data.frame(
    parent_id = c("x1", "x2"),  # collision with posts
    text      = c("c1", "c2"),
    stringsAsFactors = FALSE
  ))
  DBI::dbDisconnect(con)

  config <- list(data = list(
    explicit_columns = list(id_column = "parent_id", text_column = "text")
  ))
  combined <- load_and_combine_tables(db_path, c("posts", "comments"),
                                       source_type = "reddit",
                                       config = config)
  # Auto-recovery: std_ids prefixed with source_table -> all unique
  expect_equal(nrow(combined), 4L)
  expect_equal(length(unique(combined$std_id)), 4L)
  expect_true(all(grepl("^(posts|comments):", combined$std_id)))
})

test_that("load_and_combine_tables refuses when intra-table std_id duplicates persist after prefixing", {
  td <- withr::local_tempdir()
  db_path <- file.path(td, "test.db")
  # Posts table has TWO rows with identical post_id -> after
  # source_table prefixing (posts:DUP), they're STILL duplicates ->
  # the guard's second check refuses.
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(con, "posts", data.frame(
    post_id = c("DUP", "DUP", "OK"),
    text    = c("a", "b", "c"),
    stringsAsFactors = FALSE
  ))
  DBI::dbDisconnect(con)
  expect_error(
    load_and_combine_tables(db_path, "posts", source_type = "reddit"),
    "duplicate"
  )
})
