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

test_that(".reddit_api_get returns ok=FALSE on a 200 with a non-JSON body", {
  skip_if_not_installed("httr2")
  fake_tokmgr <- list(get = function() "tok", refresh = function() invisible(NULL))
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) httr2::response(
      status_code = 200,
      headers = list("content-type" = "text/html"),
      body = charToRaw("<html>interstitial</html>")),
    .package = "httr2"
  )
  # max_retries = 1 so the single attempt returns immediately (no retry sleep).
  out <- pakhom:::.reddit_api_get("https://oauth.reddit.com/x", fake_tokmgr,
                                  list(user_agent = "ua"), max_retries = 1)
  expect_false(out$ok)        # surfaced as failure, not thrown
  expect_equal(out$status, 200L)
})

# ==============================================================================
# Corpus integrity: failed comment fetch is surfaced and backfilled on re-scrape
# ==============================================================================
test_that("a failed comment fetch is counted and backfilled on the next run", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  listing <- mk_listing(list(mk_post(1, num_comments = 1L)), after = NULL)
  good_comments <- mk_comments_listing(list(mk_t1("c1")))
  state <- new.env(); state$comments_ok <- FALSE   # run 1: comment fetch fails

  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("/comments/", url)) {
        if (state$comments_ok) return(good_comments)
        return(list(ok = FALSE, status = 500L, body = NULL))
      }
      listing
    },
    .package = "pakhom"
  )

  args <- list(subreddits = "test", db_path = db,
               config = list(reddit_client_id = "i", reddit_client_secret = "s",
                             reddit_user_agent = "ua"),
               posts_per_subreddit = 10)

  # Run 1: post stored, comment fetch fails -> counted, post left comment-less.
  r1 <- do.call(scrape_reddit, args)
  expect_equal(r1$posts_added, 1L)
  expect_equal(r1$comments_added, 0L)
  expect_equal(r1$comment_fetch_failures, 1L)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM comments")$n, 0L)
  # The post is marked un-fetched (NULL) so a later run knows to backfill.
  csa <- DBI::dbGetQuery(con, "SELECT comments_scraped_at FROM posts WHERE post_id='post_1'")$comments_scraped_at
  expect_true(is.na(csa))
  DBI::dbDisconnect(con)

  # Run 2: comment endpoint now succeeds -> the existing post is backfilled.
  state$comments_ok <- TRUE
  r2 <- do.call(scrape_reddit, args)
  expect_equal(r2$posts_added, 0L)       # no new post
  expect_equal(r2$posts_skipped, 1L)
  expect_equal(r2$comments_added, 1L)    # comment backfilled
  expect_equal(r2$comment_fetch_failures, 0L)

  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM comments")$n, 1L)
  csa2 <- DBI::dbGetQuery(con, "SELECT comments_scraped_at FROM posts WHERE post_id='post_1'")$comments_scraped_at
  expect_false(is.na(csa2))               # now marked fetched

  # Run 3: already fetched -> not re-fetched.
  r3 <- do.call(scrape_reddit, args)
  expect_equal(r3$comments_added, 0L)
})

test_that(".epoch_to_iso parses a numeric-string epoch instead of zeroing it", {
  expect_match(pakhom:::.epoch_to_iso("1700000000"), "^2023-11-14")
  expect_match(pakhom:::.epoch_to_iso(1700000000), "^2023-11-14")
  expect_equal(pakhom:::.epoch_to_iso(NULL), "1970-01-01")
  expect_equal(pakhom:::.epoch_to_iso("not-a-number"), "1970-01-01")
})

# ==============================================================================
# A failed "load more" (morechildren) expansion is recoverable, not silent
# ==============================================================================
test_that("a morechildren failure leaves the post un-stamped, counted, and backfilled", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  listing <- mk_listing(list(mk_post(1, num_comments = 3L)), after = NULL)
  # Embedded tree: c1 plus a "more" placeholder for m1.
  comments <- mk_comments_listing(list(mk_t1("c1"), mk_more("m1")))
  more_ok <- mk_morechildren(list(mk_t1("m1")))
  state <- new.env(); state$more_ok <- FALSE  # run 1: expansion fails

  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("morechildren", url)) {
        if (state$more_ok) return(more_ok)
        return(list(ok = FALSE, status = 500L, body = NULL))
      }
      if (grepl("/comments/", url)) return(comments)
      listing
    },
    .package = "pakhom"
  )
  args <- list(subreddits = "test", db_path = db,
               config = list(reddit_client_id = "i", reddit_client_secret = "s",
                             reddit_user_agent = "ua"), posts_per_subreddit = 10)

  # Run 1: c1 stored, but the "more" expansion failed -> failure surfaced, not stamped.
  r1 <- do.call(scrape_reddit, args)
  expect_equal(r1$comment_fetch_failures, 1L)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM comments")$n, 1L)  # only c1
  expect_true(is.na(DBI::dbGetQuery(con, "SELECT comments_scraped_at FROM posts WHERE post_id='post_1'")$comments_scraped_at))
  DBI::dbDisconnect(con)

  # Run 2: expansion now succeeds -> the deep comment is backfilled.
  state$more_ok <- TRUE
  r2 <- do.call(scrape_reddit, args)
  expect_equal(r2$comment_fetch_failures, 0L)
  expect_equal(r2$comments_added, 1L)  # m1 backfilled
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_setequal(DBI::dbGetQuery(con, "SELECT comment_id FROM comments")$comment_id, c("c1", "m1"))
  expect_false(is.na(DBI::dbGetQuery(con, "SELECT comments_scraped_at FROM posts WHERE post_id='post_1'")$comments_scraped_at))
})

test_that("a post_id duplicated within one page is comment-fetched only once", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  # Same post_1 appears twice in one listing page.
  listing <- mk_listing(list(mk_post(1, num_comments = 1L), mk_post(1, num_comments = 1L)),
                        after = NULL)
  comments <- mk_comments_listing(list(mk_t1("c1")))
  fetches <- new.env(); fetches$n <- 0L
  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("/comments/", url)) { fetches$n <- fetches$n + 1L; return(comments) }
      listing
    },
    .package = "pakhom"
  )
  do.call(scrape_reddit, list(subreddits = "test", db_path = db,
    config = list(reddit_client_id = "i", reddit_client_secret = "s", reddit_user_agent = "ua"),
    posts_per_subreddit = 10))
  expect_equal(fetches$n, 1L)  # not 2
})

# ==============================================================================
# Deep-thread "continue this thread" continuations are followed (rooted re-fetch)
# ==============================================================================
test_that("a depth-limit 'continue this thread' marker is followed via a rooted re-fetch", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  listing <- mk_listing(list(mk_post(1, num_comments = 2L)), after = NULL)
  # c1's replies hold a depth-limit "more" (empty children + parent_id) -- the
  # "continue this thread" marker morechildren cannot resolve.
  cont_marker <- list(kind = "more",
                      data = list(id = "_cont", count = 1, children = list(),
                                  parent_id = "t1_c1"))
  c1 <- mk_t1("c1", replies = list(kind = "Listing", data = list(children = list(cont_marker))))
  comments <- mk_comments_listing(list(c1))
  # The ?comment=c1 re-fetch returns c1 (dup) plus the deeper reply c2.
  refetch <- mk_comments_listing(list(
    mk_t1("c1", replies = list(kind = "Listing", data = list(children = list(mk_t1("c2")))))
  ))

  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("comment=", url)) return(refetch)        # rooted continuation fetch
      if (grepl("/comments/", url)) return(comments)     # top-level comment listing
      listing
    },
    .package = "pakhom"
  )

  res <- scrape_reddit(
    subreddits = "test", db_path = db,
    config = list(reddit_client_id = "i", reddit_client_secret = "s", reddit_user_agent = "ua"),
    posts_per_subreddit = 10
  )
  expect_equal(res$comments_added, 2L)            # c1 + the deeper c2
  expect_equal(res$comment_fetch_failures, 0L)
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_setequal(DBI::dbGetQuery(con, "SELECT comment_id FROM comments")$comment_id, c("c1", "c2"))
})

test_that(".is_permanent_failure classifies statuses correctly", {
  expect_true(pakhom:::.is_permanent_failure(404L))
  expect_true(pakhom:::.is_permanent_failure(403L))
  expect_false(pakhom:::.is_permanent_failure(429L))  # rate limit is transient
  expect_false(pakhom:::.is_permanent_failure(503L))
  expect_false(pakhom:::.is_permanent_failure(NA_integer_))  # retry-exhausted/network
})

test_that("a permanent (4xx) expansion failure is accepted, not retried forever", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  listing <- mk_listing(list(mk_post(1, num_comments = 3L)), after = NULL)
  comments <- mk_comments_listing(list(mk_t1("c1"), mk_more("m1")))
  fetches <- new.env(); fetches$more <- 0L
  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("morechildren", url)) { fetches$more <- fetches$more + 1L
                                        return(list(ok = FALSE, status = 404L, body = NULL)) }
      if (grepl("/comments/", url)) return(comments)
      listing
    },
    .package = "pakhom"
  )
  args <- list(subreddits = "test", db_path = db,
               config = list(reddit_client_id = "i", reddit_client_secret = "s", reddit_user_agent = "ua"),
               posts_per_subreddit = 10)
  r1 <- do.call(scrape_reddit, args)
  expect_equal(r1$comment_fetch_failures, 0L)  # permanent failure accepted, not counted
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false(is.na(DBI::dbGetQuery(con, "SELECT comments_scraped_at FROM posts WHERE post_id='post_1'")$comments_scraped_at))
  # Second run must NOT re-fetch (post stamped) -> still 1 morechildren call total.
  do.call(scrape_reddit, args)
  expect_equal(fetches$more, 1L)
})

test_that("a transient (5xx) expansion failure is counted and retried", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)
  listing <- mk_listing(list(mk_post(1, num_comments = 3L)), after = NULL)
  comments <- mk_comments_listing(list(mk_t1("c1"), mk_more("m1")))
  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("morechildren", url)) return(list(ok = FALSE, status = NA_integer_, body = NULL))
      if (grepl("/comments/", url)) return(comments)
      listing
    },
    .package = "pakhom"
  )
  r1 <- do.call(scrape_reddit, list(subreddits = "test", db_path = db,
    config = list(reddit_client_id = "i", reddit_client_secret = "s", reddit_user_agent = "ua"),
    posts_per_subreddit = 10))
  expect_equal(r1$comment_fetch_failures, 1L)  # transient -> counted
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(is.na(DBI::dbGetQuery(con, "SELECT comments_scraped_at FROM posts WHERE post_id='post_1'")$comments_scraped_at))
})

# ==============================================================================
# Schema migration: an old DB gains comments_scraped_at without losing rows
# ==============================================================================
test_that(".init_scraper_db migrates an old posts schema and preserves rows", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  # Pre-change schema: no upvote_ratio / scraped_at / comments_scraped_at.
  DBI::dbExecute(con, "CREATE TABLE posts (post_id TEXT PRIMARY KEY, subreddit TEXT,
    title TEXT, created_utc TEXT, num_comments INTEGER, score INTEGER, author TEXT,
    text TEXT, permalink TEXT)")
  DBI::dbExecute(con, "CREATE TABLE comments (comment_id TEXT PRIMARY KEY, post_id TEXT,
    subreddit TEXT, created_utc TEXT, score INTEGER, author TEXT, comment_body TEXT, permalink TEXT)")
  DBI::dbExecute(con, "INSERT INTO posts (post_id, subreddit, title) VALUES ('old1','x','T')")
  DBI::dbDisconnect(con)

  pakhom:::.init_scraper_db(db)

  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  cols <- DBI::dbListFields(con, "posts")
  expect_true(all(c("upvote_ratio", "scraped_at", "comments_scraped_at") %in% cols))
  row <- DBI::dbGetQuery(con, "SELECT * FROM posts WHERE post_id='old1'")
  expect_equal(nrow(row), 1L)                 # row survived the migration
  expect_true(is.na(row$comments_scraped_at)) # eligible for comment backfill
})

# ==============================================================================
# Credential fallback: a NULL user-agent yields the version-stamped default
# ==============================================================================
test_that(".get_reddit_credentials builds a version-stamped user-agent when unset", {
  withr::local_envvar(
    REDDIT_CLIENT_ID = "id", REDDIT_CLIENT_SECRET = "sec", REDDIT_USER_AGENT = NA
  )
  creds <- pakhom:::.get_reddit_credentials(list(reddit_user_agent = NULL))
  expect_match(creds$user_agent, "^pakhom/.+ \\(by u/YourRedditUsername\\)$")
})

# ==============================================================================
# Recursive morechildren: a "more" inside a morechildren response is expanded
# ==============================================================================
test_that("scrape_reddit recurses into a nested morechildren placeholder", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  listing <- mk_listing(list(mk_post(1, num_comments = 5L)), after = NULL)
  comments <- mk_comments_listing(list(mk_t1("c1"), mk_more("m1")))
  calls <- new.env(); calls$more <- 0L
  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("morechildren", url)) {
        calls$more <- calls$more + 1L
        # First expansion returns a comment AND a deeper "more" (m2); second
        # expansion resolves m2.
        if (grepl("children=m1", url)) return(mk_morechildren(list(mk_t1("d1"), mk_more("m2"))))
        return(mk_morechildren(list(mk_t1("d2"))))
      }
      if (grepl("/comments/", url)) return(comments)
      listing
    },
    .package = "pakhom"
  )
  res <- scrape_reddit(subreddits = "test", db_path = db,
    config = list(reddit_client_id = "i", reddit_client_secret = "s", reddit_user_agent = "ua"),
    posts_per_subreddit = 10)
  expect_equal(calls$more, 2L)             # recursion fired (m1 then m2)
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_setequal(DBI::dbGetQuery(con, "SELECT comment_id FROM comments")$comment_id,
                  c("c1", "d1", "d2"))
})

# ==============================================================================
# Backfill is deferred (not lost) when a same-page new post fills the budget
# ==============================================================================
test_that("a backfill-eligible post after the budget cutoff is deferred, then recovered", {
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)

  pakhom:::.init_scraper_db(db)
  con0 <- DBI::dbConnect(RSQLite::SQLite(), db)
  # Pre-seed an existing comment-less post (needs backfill), comments_scraped_at NULL.
  DBI::dbExecute(con0, "INSERT INTO posts (post_id, subreddit, num_comments, created_utc) VALUES ('post_9','test',1,'0')")
  DBI::dbDisconnect(con0)

  # Page order: two NEW posts first, then the backfill-eligible existing one.
  page <- mk_listing(list(mk_post(1, num_comments = 1L), mk_post(2, num_comments = 1L),
                          mk_post(9, num_comments = 1L)), after = NULL)
  comments <- mk_comments_listing(list(mk_t1("c1")))
  testthat::local_mocked_bindings(
    .reddit_authenticate = function(creds) list(access_token = "TOK", expires_in = 3600),
    .reddit_api_get = function(url, tokmgr, creds, max_retries = 4) {
      if (grepl("/comments/", url)) return(comments)
      page
    },
    .package = "pakhom"
  )
  args <- list(subreddits = "test", db_path = db,
               config = list(reddit_client_id = "i", reddit_client_secret = "s", reddit_user_agent = "ua"),
               posts_per_subreddit = 2)  # budget filled by post_1 + post_2

  # Run 1: budget hit before reaching post_9 -> its comments are NOT backfilled yet.
  do.call(scrape_reddit, args)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  expect_true(is.na(DBI::dbGetQuery(con, "SELECT comments_scraped_at FROM posts WHERE post_id='post_9'")$comments_scraped_at))
  DBI::dbDisconnect(con)

  # Run 2: post_1/post_2 now existing (add 0, no budget break), so post_9 is reached + backfilled.
  do.call(scrape_reddit, args)
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false(is.na(DBI::dbGetQuery(con, "SELECT comments_scraped_at FROM posts WHERE post_id='post_9'")$comments_scraped_at))
})
