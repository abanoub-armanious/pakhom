# ==============================================================================
# Tests for data loading + column detection (R/07_data_loading.R)
# ==============================================================================
# Finding: the default reddit column-mapping had `id` candidates
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

test_that("standardize_data errors on a non-unique single-table id column (std_id integrity)", {
  # std_id is the primary key for coding / quote provenance / IRR / cross-run
  # joins. A single table whose id column has duplicates would silently corrupt
  # coding; standardize_data must fail loudly (the multi-table path can
  # auto-prefix with source_table, but a single table has no such fallback).
  cm <- list(id = "uid", text = "txt", author = NA, timestamp = NA,
             metrics = character(0))
  dup <- data.frame(uid = c("a", "a", "b"), txt = c("x", "y", "z"),
                    stringsAsFactors = FALSE)
  expect_error(standardize_data(dup, cm), "duplicate value")
  uniq <- data.frame(uid = c("a", "b", "c"), txt = c("x", "y", "z"),
                     stringsAsFactors = FALSE)
  expect_equal(nrow(standardize_data(uniq, cm)), 3L)
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

# ============================================================================
# regression: multi-table merge union (was intersect)
#
# Background: an early full-corpus run dropped num_comments and
# upvote_ratio from the analytic data because the comments table doesn't
# carry those columns. load_and_combine_tables was filtering to the
# intersect of column names across tables -> the post-only metric
# columns were silently dropped. The paper-style subtheme tables
# and correlations could therefore only ever score on `score`.
#
# Fix: bind_rows() the standardized tables directly and let dplyr NA-fill
# missing columns. Log a single info line listing the columns that get
# NA-filled so users have explicit signal about partial coverage.
# ============================================================================

test_that("load_and_combine_tables preserves columns present in only some tables", {
  td <- withr::local_tempdir()
  db_path <- file.path(td, "test.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  # Canonical Reddit shape: posts carry score, num_comments, upvote_ratio;
  # comments carry score only. The earlier intersect path would drop
  # num_comments + upvote_ratio from the combined data, leaving downstream
  # paper-style tables + correlation layers with `score` as the lone metric.
  DBI::dbWriteTable(con, "posts", data.frame(
    post_id      = c("p1", "p2", "p3"),
    text         = c("post 1", "post 2", "post 3"),
    author       = c("a1", "a2", "a3"),
    score        = c(10L, 20L, 30L),
    num_comments = c(5L, 6L, 7L),
    upvote_ratio = c(0.9, 0.8, 0.7),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "comments", data.frame(
    comment_id   = c("c1", "c2"),
    post_id      = c("p1", "p2"),
    comment_body = c("comment 1", "comment 2"),
    author       = c("u1", "u2"),
    score        = c(3L, 4L),
    stringsAsFactors = FALSE
  ))
  DBI::dbDisconnect(con)
  on.exit(NULL, add = FALSE)  # disarm cleanup; already disconnected

  combined <- load_and_combine_tables(db_path, c("posts", "comments"),
                                       source_type = "reddit")

  # All 5 rows survive (3 posts + 2 comments).
  expect_equal(nrow(combined), 5L)

  # num_comments and upvote_ratio must EXIST in the combined data.
  expect_true("num_comments" %in% names(combined),
              info = "num_comments dropped at merge")
  expect_true("upvote_ratio" %in% names(combined),
              info = "upvote_ratio dropped at merge")

  # Post rows carry the metric values; comment rows are NA.
  post_rows    <- combined[combined$source_table == "posts", ]
  comment_rows <- combined[combined$source_table == "comments", ]
  expect_equal(sort(post_rows$num_comments), c(5L, 6L, 7L))
  expect_equal(sort(post_rows$upvote_ratio), c(0.7, 0.8, 0.9))
  expect_true(all(is.na(comment_rows$num_comments)))
  expect_true(all(is.na(comment_rows$upvote_ratio)))

  # score is shared across both tables, so no NA-fill there.
  expect_false(any(is.na(combined$score)))
})

test_that("load_and_combine_tables works when all columns are shared (no NA-fill needed)", {
  td <- withr::local_tempdir()
  db_path <- file.path(td, "test.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  # Both tables carry identical column sets -> partial_cols should be
  # empty and no info log fires.
  shared_shape <- data.frame(
    id     = c("x", "y"),
    text   = c("a", "b"),
    author = c("u1", "u2"),
    score  = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "tbl_a", shared_shape)
  DBI::dbWriteTable(con, "tbl_b", data.frame(
    id     = c("p", "q"),
    text   = c("c", "d"),
    author = c("u3", "u4"),
    score  = c(3L, 4L),
    stringsAsFactors = FALSE
  ))
  DBI::dbDisconnect(con)
  on.exit(NULL, add = FALSE)

  combined <- load_and_combine_tables(db_path, c("tbl_a", "tbl_b"),
                                       source_type = "reddit")
  expect_equal(nrow(combined), 4L)
  # No NA in any input-supplied column. (standardize_data may add
  # internal columns like std_timestamp that are NA when no source
  # timestamp column was detected. Those are unrelated to the
  # intersect-vs-union behavior we're pinning here.)
  for (col in c("std_id", "std_text", "score", "source_table")) {
    expect_false(any(is.na(combined[[col]])),
                 info = sprintf("unexpected NA in column %s", col))
  }
})

test_that("load_and_combine_tables: 3-table union with disjoint metric columns", {
  # audit : the original tests only covered 2-table fixtures.
  # The union semantics need to behave correctly across N tables where each
  # contributes a DIFFERENT subset of the standardized metrics list.
  # standardize_data() drops non-metric columns at line 364-368, so the test
  # must use the canonical metric names (score / num_comments / upvote_ratio)
  # distributed across the 3 tables rather than inventing new metric names.
  td <- withr::local_tempdir()
  db_path <- file.path(td, "test.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  # Posts: all 3 standard reddit metrics.
  DBI::dbWriteTable(con, "posts", data.frame(
    post_id      = c("p1", "p2"),
    text         = c("post 1", "post 2"),
    author       = c("a1", "a2"),
    score        = c(10L, 20L),
    num_comments = c(5L, 6L),
    upvote_ratio = c(0.9, 0.8),
    stringsAsFactors = FALSE
  ))
  # Comments: score + num_comments only (no upvote_ratio).
  DBI::dbWriteTable(con, "comments", data.frame(
    comment_id   = c("c1", "c2"),
    post_id      = c("p1", "p2"),
    comment_body = c("comment 1", "comment 2"),
    author       = c("u1", "u2"),
    score        = c(3L, 4L),
    num_comments = c(0L, 0L),
    stringsAsFactors = FALSE
  ))
  # Submissions: score + upvote_ratio only (no num_comments). 'submissions'
  # is in the auto-content-tables list so detect_columns recognizes it.
  DBI::dbWriteTable(con, "submissions", data.frame(
    submission_id = c("s1", "s2"),
    text          = c("sub 1", "sub 2"),
    author        = c("a3", "a4"),
    score         = c(15L, 25L),
    upvote_ratio  = c(0.7, 0.6),
    stringsAsFactors = FALSE
  ))
  DBI::dbDisconnect(con)
  on.exit(NULL, add = FALSE)

  combined <- load_and_combine_tables(db_path,
                                       c("posts", "comments", "submissions"),
                                       source_type = "reddit")

  # 2 + 2 + 2 = 6 rows; all three reddit metric columns must survive the
  # 3-way union (the earlier intersect would have kept only `score`).
  expect_equal(nrow(combined), 6L)
  expect_true("score"        %in% names(combined),
              info = "shared metric dropped in 3-table union")
  expect_true("num_comments" %in% names(combined),
              info = "posts+comments metric dropped in 3-table union")
  expect_true("upvote_ratio" %in% names(combined),
              info = "posts+submissions metric dropped in 3-table union")

  # Per-row partial-fill correctness:
  #   posts rows        -> all 3 metrics present
  #   comments rows     -> score + num_comments present, upvote_ratio NA
  #   submissions rows  -> score + upvote_ratio present, num_comments NA
  posts_rows       <- combined[combined$source_table == "posts", ]
  comments_rows    <- combined[combined$source_table == "comments", ]
  submissions_rows <- combined[combined$source_table == "submissions", ]

  expect_equal(sort(posts_rows$num_comments), c(5L, 6L))
  expect_equal(sort(posts_rows$upvote_ratio), c(0.8, 0.9))
  expect_equal(sort(comments_rows$num_comments), c(0L, 0L))
  expect_true(all(is.na(comments_rows$upvote_ratio)),
              info = "comments rows must have NA upvote_ratio (only posts+submissions carry it)")
  expect_equal(sort(submissions_rows$upvote_ratio), c(0.6, 0.7))
  expect_true(all(is.na(submissions_rows$num_comments)),
              info = "submissions rows must have NA num_comments")

  # `score` is shared across all 3 tables -> no NA anywhere.
  expect_false(any(is.na(combined$score)))
})
