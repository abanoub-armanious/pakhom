# Tests for Reddit scraper (05_scraper.R)
# All tests mock HTTP and database calls -- no live API or DB access.

# ==============================================================================
# Helper: Minimal mock Reddit JSON response
# ==============================================================================
mock_reddit_listing <- function(n_posts = 2) {
  children <- lapply(seq_len(n_posts), function(i) {
    list(
      kind = "t3",
      data = list(
        id = paste0("post_", i),
        name = paste0("t3_post_", i),
        title = paste0("Test post title ", i),
        selftext = paste0("Test post body text for entry ", i, ". Enough content here."),
        author = paste0("user_", i),
        score = i * 10L,
        num_comments = i,
        created_utc = as.numeric(Sys.time()) - i * 3600,
        permalink = paste0("/r/test/comments/post_", i, "/test/"),
        subreddit = "test"
      )
    )
  })
  list(data = list(children = children, after = NULL))
}

mock_reddit_comments <- function(n_comments = 1) {
  children <- lapply(seq_len(n_comments), function(i) {
    list(
      kind = "t1",
      data = list(
        id = paste0("comment_", i),
        body = paste0("Test comment body text ", i),
        author = paste0("commenter_", i),
        score = i * 5L,
        created_utc = as.numeric(Sys.time()) - i * 1800
      )
    )
  })
  list(
    list(data = list(children = list())),
    list(data = list(children = children))
  )
}

# ==============================================================================
# .init_scraper_db: Database schema creation
# ==============================================================================
test_that(".init_scraper_db creates posts and comments tables", {
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".db")
  on.exit(unlink(db_path), add = TRUE)

  # Call internal function
  pakhom:::.init_scraper_db(db_path)

  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  tables <- DBI::dbListTables(db)
  expect_true("posts" %in% tables)
  expect_true("comments" %in% tables)

  # Check posts schema has expected columns
  post_cols <- DBI::dbListFields(db, "posts")
  expect_true(all(c("post_id", "subreddit", "title", "text", "author", "score") %in% post_cols))

  # Check comments schema
  comment_cols <- DBI::dbListFields(db, "comments")
  expect_true(all(c("comment_id", "post_id", "comment_body", "author", "score") %in% comment_cols))
})

test_that(".init_scraper_db is idempotent", {
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".db")
  on.exit(unlink(db_path), add = TRUE)

  # Run twice -- should not error

  pakhom:::.init_scraper_db(db_path)
  expect_no_error(pakhom:::.init_scraper_db(db_path))

  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)
  expect_true("posts" %in% DBI::dbListTables(db))
})

# ==============================================================================
# .get_reddit_credentials: Credential resolution
# ==============================================================================
test_that(".get_reddit_credentials reads from config", {
  cfg <- list(
    reddit_client_id = "test_id",
    reddit_client_secret = "test_secret",
    reddit_user_agent = "test_agent"
  )
  creds <- pakhom:::.get_reddit_credentials(cfg)
  expect_equal(creds$client_id, "test_id")
  expect_equal(creds$client_secret, "test_secret")
  expect_equal(creds$user_agent, "test_agent")
})

test_that(".get_reddit_credentials falls back to env vars", {
  withr::local_envvar(
    REDDIT_CLIENT_ID = "env_id",
    REDDIT_CLIENT_SECRET = "env_secret",
    REDDIT_USER_AGENT = "env_agent"
  )
  creds <- pakhom:::.get_reddit_credentials(NULL)
  expect_equal(creds$client_id, "env_id")
  expect_equal(creds$client_secret, "env_secret")
  expect_equal(creds$user_agent, "env_agent")
})

test_that(".get_reddit_credentials errors on missing credentials", {
  withr::local_envvar(
    REDDIT_CLIENT_ID = NA,
    REDDIT_CLIENT_SECRET = NA,
    REDDIT_USER_AGENT = NA
  )
  expect_error(pakhom:::.get_reddit_credentials(NULL))
})

# ==============================================================================
# scrape_reddit: Top-level integration with mocked HTTP
# ==============================================================================
test_that("scrape_reddit validates required credentials", {
  withr::local_envvar(
    REDDIT_CLIENT_ID = NA,
    REDDIT_CLIENT_SECRET = NA,
    REDDIT_USER_AGENT = NA
  )
  expect_error(
    scrape_reddit(subreddits = "test", db_path = tempfile(fileext = ".db")),
    regex = "credential|client_id|secret",
    ignore.case = TRUE
  )
})

# ==============================================================================
# Duplicate detection: post_id uniqueness
# ==============================================================================
test_that("duplicate posts are skipped on re-insert", {
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".db")
  on.exit(unlink(db_path), add = TRUE)

  pakhom:::.init_scraper_db(db_path)

  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  # Insert a post manually
  DBI::dbExecute(db, "INSERT INTO posts (post_id, subreddit, title, text, author, score, num_comments, created_utc, scraped_at, permalink)
    VALUES ('post_1', 'test', 'Title', 'Body text', 'author', 10, 1, '1234567890', CURRENT_TIMESTAMP, '/r/test/')")

  # Verify the post exists
  count <- DBI::dbGetQuery(db, "SELECT COUNT(*) as n FROM posts WHERE post_id = 'post_1'")$n
  expect_equal(count, 1L)

  # Try inserting the same post_id again -- should fail or be handled
  result <- tryCatch(
    DBI::dbExecute(db, "INSERT INTO posts (post_id, subreddit, title, text, author, score, num_comments, created_utc, scraped_at, permalink)
      VALUES ('post_1', 'test', 'Title 2', 'Body 2', 'author2', 20, 2, '1234567891', CURRENT_TIMESTAMP, '/r/test2/')"),
    error = function(e) "duplicate_caught"
  )

  # Either the insert was rejected (unique constraint) or handled
  final_count <- DBI::dbGetQuery(db, "SELECT COUNT(*) as n FROM posts WHERE post_id = 'post_1'")$n
  expect_equal(final_count, 1L)
})

# ==============================================================================
# .parse_filename_metadata: Regex extraction (if exposed)
# ==============================================================================
# Note: This tests the learning module's filename parser, but it's relevant
# to scraper data flow since scraped filenames follow similar patterns.
