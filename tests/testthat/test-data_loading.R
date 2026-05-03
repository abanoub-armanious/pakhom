# Tests for data loading (07_data_loading.R)

test_that("load_data works with SQLite database", {
  skip_if_not_installed("RSQLite")
  db_path <- tempfile(fileext = ".db")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(con, "posts", data.frame(
    id = 1:3,
    body = c("First post text", "Second post text", "Third post text"),
    author = c("user1", "user2", "user3"),
    created_utc = c(1000, 2000, 3000),
    score = c(10, 20, 30)
  ))
  DBI::dbDisconnect(con)

  result <- load_data(db_path)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true("body" %in% names(result))
})

test_that("load_data auto-selects content table", {
  skip_if_not_installed("RSQLite")
  db_path <- tempfile(fileext = ".db")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(con, "metadata", data.frame(key = "val"))
  DBI::dbWriteTable(con, "posts", data.frame(
    id = 1:5,
    body = paste("Post", 1:5)
  ))
  DBI::dbDisconnect(con)

  result <- load_data(db_path)
  expect_equal(nrow(result), 5)
})

test_that("load_data errors on missing database", {
  expect_error(load_data("/nonexistent/path.db"), "not found")
})

test_that("explore_database returns table info", {
  skip_if_not_installed("RSQLite")
  db_path <- tempfile(fileext = ".db")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(con, "test_table", data.frame(x = 1:5, y = letters[1:5]))
  DBI::dbDisconnect(con)

  info <- explore_database(db_path)
  expect_type(info, "list")
  expect_true("test_table" %in% info$table_names)
  expect_equal(info$row_counts[["test_table"]], 5)
  expect_true("x" %in% info$table_info$test_table$columns)
})

test_that("explore_database errors on missing file", {
  expect_error(explore_database("/nonexistent.db"), "not found")
})
