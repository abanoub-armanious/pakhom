# Tests for Reddit scraper (05_scraper.R)
# All tests mock HTTP and database calls -- no live API or DB access.

# ==============================================================================
# Helpers: minimal mock Reddit JSON payloads (new structured api_get shape)
# ==============================================================================
ok_body <- function(body) list(ok = TRUE, status = 200L, body = body)

mk_post <- function(i, num_comments = 0L, created = TRUE) {
  d <- list(
    id = paste0("post_", i),
    title = paste0("Test post title ", i),
    selftext = paste0("Test post body text for entry ", i, ". Enough content here."),
    author = paste0("user_", i),
    score = i * 10L,
    num_comments = num_comments,
    upvote_ratio = 0.9,
    permalink = paste0("/r/test/comments/post_", i, "/test/"),
    subreddit = "test"
  )
  if (created) d$created_utc <- 1700000000 + i * 3600
  list(kind = "t3", data = d)
}

mk_listing <- function(posts, after = NULL) {
  ok_body(list(data = list(after = after, children = posts)))
}

mk_t1 <- function(id, body = NULL, replies = "") {
  list(kind = "t1", data = list(
    id = id,
    body = body %||% paste0("Comment body ", id),
    author = paste0("commenter_", id),
    score = 5L,
    created_utc = 1700000200,
    permalink = paste0("/r/test/comments/post_1/test/", id, "/"),
    replies = replies
  ))
}

mk_more <- function(ids) {
  list(kind = "more", data = list(id = "_more", count = length(ids),
                                  children = as.list(ids)))
}

# A post-comments listing payload: data[[2]]$data$children holds the tree.
mk_comments_listing <- function(children) {
  ok_body(list(
    list(data = list(children = list())),
    list(data = list(children = children))
  ))
}

# A morechildren payload: body$json$data$things holds a flat node list.
mk_morechildren <- function(things) {
  ok_body(list(json = list(data = list(things = things))))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ==============================================================================
# .init_scraper_db: Database schema creation
# ==============================================================================
test_that(".init_scraper_db creates posts and comments tables", {
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".db")
  on.exit(unlink(db_path), add = TRUE)

  pakhom:::.init_scraper_db(db_path)

  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  tables <- DBI::dbListTables(db)
  expect_true("posts" %in% tables)
  expect_true("comments" %in% tables)

  post_cols <- DBI::dbListFields(db, "posts")
  expect_true(all(c("post_id", "subreddit", "title", "text", "author", "score") %in% post_cols))

  comment_cols <- DBI::dbListFields(db, "comments")
  expect_true(all(c("comment_id", "post_id", "comment_body", "author", "score") %in% comment_cols))
})

test_that(".init_scraper_db is idempotent", {
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".db")
  on.exit(unlink(db_path), add = TRUE)

  pakhom:::.init_scraper_db(db_path)
  expect_no_error(pakhom:::.init_scraper_db(db_path))

  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)
  expect_true("posts" %in% DBI::dbListTables(db))
})

# ==============================================================================
# .get_reddit_credentials: Credential resolution (environment-first)
# ==============================================================================
test_that(".get_reddit_credentials reads from config when env is unset", {
  withr::local_envvar(
    REDDIT_CLIENT_ID = NA, REDDIT_CLIENT_SECRET = NA, REDDIT_USER_AGENT = NA
  )
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

test_that(".get_reddit_credentials prefers env vars over config", {
  withr::local_envvar(
    REDDIT_CLIENT_ID = "env_id",
    REDDIT_CLIENT_SECRET = "env_secret",
    REDDIT_USER_AGENT = "env_agent"
  )
  # Even when config also sets values, env wins (secrets live in .Renviron).
  creds <- pakhom:::.get_reddit_credentials(list(reddit_client_id = "cfg_id"))
  expect_equal(creds$client_id, "env_id")
  expect_equal(creds$client_secret, "env_secret")
  expect_equal(creds$user_agent, "env_agent")
})

test_that(".get_reddit_credentials errors on missing credentials", {
  withr::local_envvar(
    REDDIT_CLIENT_ID = NA, REDDIT_CLIENT_SECRET = NA, REDDIT_USER_AGENT = NA
  )
  expect_error(pakhom:::.get_reddit_credentials(NULL))
})

# ==============================================================================
# scrape_reddit: validation
# ==============================================================================
test_that("scrape_reddit validates required credentials", {
  withr::local_envvar(
    REDDIT_CLIENT_ID = NA, REDDIT_CLIENT_SECRET = NA, REDDIT_USER_AGENT = NA
  )
  expect_error(
    scrape_reddit(subreddits = "test", db_path = tempfile(fileext = ".db")),
    regex = "credential|client_id|secret",
    ignore.case = TRUE
  )
})

test_that("scrape_reddit rejects an invalid sort_by", {
  expect_error(
    scrape_reddit(subreddits = "test", db_path = tempfile(fileext = ".db"),
                  sort_by = "newest"),
    regex = "should be one of"
  )
})

# ==============================================================================
# scrape_reddit: happy path with mocked HTTP (posts + comment tree)
# ==============================================================================
test_that("scrape_reddit stores posts and the full comment tree", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  # Comment tree: c1 (with nested reply c2) + a "more" node pointing to m1/m2,
  # which morechildren expands into two further comments.
  comments <- mk_comments_listing(list(
    mk_t1("c1", replies = list(kind = "Listing",
                               data = list(children = list(mk_t1("c2"))))),
    mk_more(c("m1", "m2"))
  ))
  more_payload <- mk_morechildren(list(mk_t1("m1"), mk_t1("m2")))
  listing <- mk_listing(list(mk_post(1, num_comments = 3L)), after = NULL)

  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("morechildren", url)) return(more_payload)
      if (grepl("/comments/", url)) return(comments)
      listing
    },
    .package = "pakhom"
  )

  res <- scrape_reddit(
    subreddits = "test", db_path = db,
    config = list(reddit_client_id = "i", reddit_client_secret = "s",
                  reddit_user_agent = "ua"),
    posts_per_subreddit = 10
  )

  expect_equal(res$posts_added, 1L)
  expect_equal(res$comments_added, 4L)          # c1, c2, m1, m2
  expect_length(res$truncated_subreddits, 0L)

  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM posts")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM comments")$n, 4L)
  expect_setequal(DBI::dbGetQuery(con, "SELECT comment_id FROM comments")$comment_id,
                  c("c1", "c2", "m1", "m2"))
})

test_that("scrape_reddit dedups posts and comments on a second run", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  listing <- mk_listing(list(mk_post(1, num_comments = 1L)), after = NULL)
  comments <- mk_comments_listing(list(mk_t1("c1")))

  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("/comments/", url)) return(comments)
      listing
    },
    .package = "pakhom"
  )

  args <- list(subreddits = "test", db_path = db,
               config = list(reddit_client_id = "i", reddit_client_secret = "s",
                             reddit_user_agent = "ua"),
               posts_per_subreddit = 10)
  r1 <- do.call(scrape_reddit, args)
  r2 <- do.call(scrape_reddit, args)

  expect_equal(r1$posts_added, 1L)
  expect_equal(r2$posts_added, 0L)
  expect_equal(r2$posts_skipped, 1L)

  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM posts")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM comments")$n, 1L)
})

# ==============================================================================
# scrape_reddit: a post missing created_utc must not crash the run
# ==============================================================================
test_that("scrape_reddit tolerates a post with no created_utc", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  listing <- mk_listing(list(mk_post(1, created = FALSE)), after = NULL)

  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) listing,
    .package = "pakhom"
  )

  res <- expect_no_error(scrape_reddit(
    subreddits = "test", db_path = db,
    config = list(reddit_client_id = "i", reddit_client_secret = "s",
                  reddit_user_agent = "ua"),
    posts_per_subreddit = 10, include_comments = FALSE
  ))
  expect_equal(res$posts_added, 1L)

  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  cu <- DBI::dbGetQuery(con, "SELECT created_utc FROM posts WHERE post_id='post_1'")$created_utc
  expect_false(is.na(cu))
  expect_match(cu, "^1970-01-01")  # epoch fallback, not a crash
})

# ==============================================================================
# scrape_reddit: an API failure flags the subreddit as truncated, not success
# ==============================================================================
test_that("scrape_reddit flags a failed subreddit as truncated without erroring", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      list(ok = FALSE, status = 404L, body = NULL)  # e.g. misspelled subreddit
    },
    .package = "pakhom"
  )

  res <- scrape_reddit(
    subreddits = "doesnotexist", db_path = db,
    config = list(reddit_client_id = "i", reddit_client_secret = "s",
                  reddit_user_agent = "ua"),
    posts_per_subreddit = 10
  )
  expect_equal(res$posts_added, 0L)
  expect_equal(res$truncated_subreddits, "doesnotexist")
})

# ==============================================================================
# scrape_reddit: budget counts only NEW posts, across pages
# ==============================================================================
test_that("scrape_reddit budget counts only new posts and paginates", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  # Pre-seed post_1 so it is a duplicate on page 1; page 1 also has post_2
  # (new), page 2 has post_3 (new). With a budget of 2 NEW posts, the run must
  # reach page 2 rather than stopping once the skip+add count hits 2.
  pakhom:::.init_scraper_db(db)
  con0 <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con0, "INSERT INTO posts (post_id, subreddit, created_utc) VALUES ('post_1','test','0')")
  DBI::dbDisconnect(con0)

  page1 <- mk_listing(list(mk_post(1), mk_post(2)), after = "cursor")
  page2 <- mk_listing(list(mk_post(3)), after = NULL)
  calls <- new.env(); calls$n <- 0L

  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("after=", url)) return(page2)
      calls$n <- calls$n + 1L
      page1
    },
    .package = "pakhom"
  )

  res <- scrape_reddit(
    subreddits = "test", db_path = db,
    config = list(reddit_client_id = "i", reddit_client_secret = "s",
                  reddit_user_agent = "ua"),
    posts_per_subreddit = 2, include_comments = FALSE
  )
  expect_equal(res$posts_added, 2L)   # post_2 + post_3
  expect_equal(res$posts_skipped, 1L) # post_1 (pre-seeded) did not consume budget

  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_setequal(DBI::dbGetQuery(con, "SELECT post_id FROM posts")$post_id,
                  c("post_1", "post_2", "post_3"))
})

# ==============================================================================
# Progress bar must not over-tick past its total (would abort the scrape)
# ==============================================================================
test_that("scrape_reddit does not over-tick the progress bar when a page exceeds the budget", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("progress")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  # One page of 5 posts, budget of 3 (page larger than budget). The real
  # progress bar throws if ticked past its total, so this guards the fix.
  page <- mk_listing(lapply(1:5, function(i) mk_post(i)), after = NULL)
  testthat::local_mocked_bindings(
    safe_progress_bar = function(format, total) {
      progress::progress_bar$new(format = format, total = total, clear = FALSE)
    },
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) page,
    .package = "pakhom"
  )

  res <- expect_no_error(scrape_reddit(
    subreddits = "test", db_path = db,
    config = list(reddit_client_id = "i", reddit_client_secret = "s",
                  reddit_user_agent = "ua"),
    posts_per_subreddit = 3, include_comments = FALSE
  ))
  expect_equal(res$posts_added, 3L)
})

# ==============================================================================
# Token manager: refreshes on expiry and on force
# ==============================================================================
test_that(".reddit_token_manager refreshes when the token expires", {
  n <- new.env(); n$count <- 0L
  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) {
      n$count <- n$count + 1L
      list(access_token = paste0("TOK", n$count), expires_in = 3600)
    },
    .package = "pakhom"
  )
  tm <- pakhom:::.reddit_token_manager(list())
  expect_equal(tm$get(), "TOK1")
  expect_equal(tm$get(), "TOK1")    # cached, no re-auth
  expect_equal(n$count, 1L)
  tm$refresh()                       # force
  expect_equal(tm$get(), "TOK2")
  expect_equal(n$count, 2L)
})

# ==============================================================================
# .reddit_api_get: status handling against constructed httr2 responses
# ==============================================================================
test_that(".reddit_api_get re-authenticates on a 401 then succeeds", {
  skip_if_not_installed("httr2")
  refreshes <- new.env(); refreshes$n <- 0L
  fake_tokmgr <- list(
    get = function() "tok",
    refresh = function() { refreshes$n <- refreshes$n + 1L; invisible("tok2") }
  )
  seq_resp <- list(
    httr2::response(status_code = 401, body = charToRaw("{}")),
    httr2::response(status_code = 200,
                    headers = list("content-type" = "application/json"),
                    body = charToRaw('{"hello":"world"}'))
  )
  i <- new.env(); i$k <- 0L
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) { i$k <- i$k + 1L; seq_resp[[i$k]] },
    .package = "httr2"
  )

  out <- pakhom:::.reddit_api_get("https://oauth.reddit.com/x", fake_tokmgr,
                                  list(user_agent = "ua"))
  expect_true(out$ok)
  expect_equal(out$body$hello, "world")
  expect_equal(refreshes$n, 1L)
})

test_that(".reddit_api_get returns ok=FALSE on a 404", {
  skip_if_not_installed("httr2")
  fake_tokmgr <- list(get = function() "tok", refresh = function() invisible(NULL))
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) httr2::response(status_code = 404, body = charToRaw("{}")),
    .package = "httr2"
  )
  out <- pakhom:::.reddit_api_get("https://oauth.reddit.com/x", fake_tokmgr,
                                  list(user_agent = "ua"))
  expect_false(out$ok)
  expect_equal(out$status, 404L)
})

test_that(".retry_after_seconds reads the Retry-After header", {
  skip_if_not_installed("httr2")
  resp <- httr2::response(status_code = 429, headers = list("retry-after" = "12"),
                          body = charToRaw("{}"))
  expect_equal(pakhom:::.retry_after_seconds(resp, default = 60), 12)
  resp2 <- httr2::response(status_code = 429, body = charToRaw("{}"))
  expect_equal(pakhom:::.retry_after_seconds(resp2, default = 60), 60)
})

# ==============================================================================
# Plain-list config: db_path + subreddits resolve from a nested $scraping
# ==============================================================================
test_that("scrape_reddit resolves db_path and subreddits from a plain list config", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  listing <- mk_listing(list(mk_post(1)), after = NULL)
  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) listing,
    .package = "pakhom"
  )

  cfg <- list(
    database = db,
    scraping = list(subreddits = "test", reddit_client_id = "i",
                    reddit_client_secret = "s", reddit_user_agent = "ua")
  )
  res <- scrape_reddit(config = cfg, include_comments = FALSE)
  expect_equal(res$posts_added, 1L)
})

# ==============================================================================
# Duplicate detection: post_id uniqueness (raw SQL)
# ==============================================================================
test_that("duplicate posts are skipped on re-insert", {
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".db")
  on.exit(unlink(db_path), add = TRUE)

  pakhom:::.init_scraper_db(db_path)

  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  DBI::dbExecute(db, "INSERT INTO posts (post_id, subreddit, title, text, author, score, num_comments, created_utc, scraped_at, permalink)
    VALUES ('post_1', 'test', 'Title', 'Body text', 'author', 10, 1, '1234567890', CURRENT_TIMESTAMP, '/r/test/')")

  count <- DBI::dbGetQuery(db, "SELECT COUNT(*) as n FROM posts WHERE post_id = 'post_1'")$n
  expect_equal(count, 1L)

  tryCatch(
    DBI::dbExecute(db, "INSERT INTO posts (post_id, subreddit, title, text, author, score, num_comments, created_utc, scraped_at, permalink)
      VALUES ('post_1', 'test', 'Title 2', 'Body 2', 'author2', 20, 2, '1234567891', CURRENT_TIMESTAMP, '/r/test2/')"),
    error = function(e) "duplicate_caught"
  )

  final_count <- DBI::dbGetQuery(db, "SELECT COUNT(*) as n FROM posts WHERE post_id = 'post_1'")$n
  expect_equal(final_count, 1L)
})

# ==============================================================================
# Precedence: an explicit argument wins over a config value (consistent)
# ==============================================================================
test_that("explicit scrape_reddit arguments override config values", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  seen <- new.env(); seen$sort <- NA_character_
  listing <- mk_listing(list(mk_post(1)), after = NULL)
  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      m <- regmatches(url, regexpr("/(new|hot|top|rising)\\.json", url))
      if (length(m)) seen$sort <- sub("\\.json", "", sub("/", "", m))
      listing
    },
    .package = "pakhom"
  )

  cfg <- list(database = db, scraping = list(
    subreddits = "test", sort_by = "new", reddit_client_id = "i",
    reddit_client_secret = "s", reddit_user_agent = "ua"))
  # config says sort_by=new, but the explicit argument hot must win.
  scrape_reddit(config = cfg, sort_by = "hot", include_comments = FALSE)
  expect_equal(seen$sort, "hot")
})
