# ==============================================================================
# Reddit Scraper: Broad Subreddit Collection for Thematic Analysis
# ==============================================================================
# Scrapes entire subreddits (posts + full comment trees) into SQLite. The
# progressive sequential coder downstream decides which entries match the
# research question, so no keyword pre-filtering is applied here.
# ==============================================================================

#' Scrape Reddit subreddits into a SQLite database
#'
#' Authenticates with the Reddit API and collects posts and their full comment
#' trees from specified subreddits. Unlike keyword-based scrapers, this collects
#' ALL content and lets the progressive sequential coder downstream decide which
#' entries match the research question (no keyword pre-filter).
#'
#' Comments are captured well beyond the first page: nested reply threads are
#' walked recursively and truncated "load more comments" placeholders are
#' expanded via the Reddit API, up to the API's returned depth. Very deeply
#' nested "continue this thread" branches past that depth are not followed.
#'
#' The access token is refreshed automatically (proactively before expiry and
#' on a 401), so long multi-subreddit runs do not truncate when the initial
#' token ages out.
#'
#' Comment fetching is recoverable: if a post's comments cannot be fetched on
#' one run, the post is kept and its comments are backfilled on a later run
#' (tracked per post), and the count of failed fetches is returned.
#'
#' @param config ThematicConfig object or list with a \code{$scraping} section
#'   (a bare list is also accepted and treated as the scraping section).
#' @param db_path Path to SQLite database (created if missing)
#' @param subreddits Character vector of subreddit names (without "r/")
#' @param posts_per_subreddit Max NEW posts to add per subreddit (default 500).
#'   Already-stored posts do not count against this budget, so re-running reaches
#'   genuinely new content rather than re-counting duplicates.
#' @param include_comments Logical; also scrape full comment trees (default TRUE)
#' @param sort_by Sort method: "new", "hot", "top", "rising" (default "new")
#' @param time_filter Time filter for "top" sort: "hour", "day", "week",
#'   "month", "year", "all" (default "all")
#' @return List with counts: \code{posts_added}, \code{comments_added},
#'   \code{posts_skipped}, \code{comment_fetch_failures} (posts whose comments
#'   could not be fetched this run; they are retried next run), and
#'   \code{truncated_subreddits} (a character vector of subreddits whose
#'   collection stopped early because of an API error, so the caller can tell a
#'   genuinely empty result apart from an incomplete one).
#' @export
scrape_reddit <- function(config = NULL, db_path = NULL, subreddits = NULL,
                           posts_per_subreddit = NULL, include_comments = NULL,
                           sort_by = NULL, time_filter = NULL) {

  # Resolve config. Accept a ThematicConfig or a plain list. Prefer an explicit
  # $scraping sub-list; otherwise treat the supplied list as the scraping
  # section. db_path is resolved from $data$database (ThematicConfig) or a
  # top-level $database, so a hand-built list configures the scraper the same
  # way a loaded config does.
  # Use exact [[ ]] indexing rather than $ so a top-level `database` key cannot
  # be partial-matched by `data` (which would mis-resolve db_path).
  scrape_cfg <- NULL
  if (!is.null(config)) {
    scrape_cfg <- config[["scraping"]] %||% (if (is.list(config)) config else NULL)
    db_path <- db_path %||% config[["data"]][["database"]] %||% config[["database"]]
    subreddits <- subreddits %||% scrape_cfg$subreddits
  }

  # Resolution order, consistent for every parameter: an explicitly-passed
  # argument wins, then the config value, then the built-in default.
  posts_per_subreddit <- posts_per_subreddit %||% scrape_cfg$posts_per_subreddit %||% 500L
  include_comments <- include_comments %||% scrape_cfg$include_comments %||% TRUE
  sort_by <- sort_by %||% scrape_cfg$sort_by %||% "new"
  time_filter <- time_filter %||% scrape_cfg$time_filter %||% "all"

  # Validate the listing parameters up front so a typo fails with an actionable
  # message rather than a malformed request that silently returns nothing.
  sort_by <- match.arg(sort_by, c("new", "hot", "top", "rising"))
  time_filter <- match.arg(time_filter,
                            c("hour", "day", "week", "month", "year", "all"))

  if (is.null(db_path)) stop("db_path is required")
  if (is.null(subreddits) || length(subreddits) == 0) stop("subreddits is required")

  # Authenticate. The token manager refreshes proactively and on 401.
  creds <- .get_reddit_credentials(scrape_cfg)
  tokmgr <- .reddit_token_manager(creds)
  tokmgr$get()  # fetch the first token now so bad credentials fail fast

  # Initialize database
  .init_scraper_db(db_path)

  log_info("Starting Reddit scrape: {length(subreddits)} subreddit(s), {posts_per_subreddit} posts each")

  totals <- list(posts_added = 0L, comments_added = 0L, posts_skipped = 0L,
                 comment_fetch_failures = 0L, truncated_subreddits = character(0))

  # Randomize order to reduce pattern detection
  shuffled_subs <- sample(subreddits)
  for (sub in shuffled_subs) {
    result <- .scrape_subreddit(
      subreddit = sub,
      db_path = db_path,
      tokmgr = tokmgr,
      creds = creds,
      max_posts = posts_per_subreddit,
      include_comments = include_comments,
      sort_by = sort_by,
      time_filter = time_filter
    )

    totals$posts_added <- totals$posts_added + result$posts_added
    totals$comments_added <- totals$comments_added + result$comments_added
    totals$posts_skipped <- totals$posts_skipped + result$posts_skipped
    totals$comment_fetch_failures <- totals$comment_fetch_failures + result$comment_failures
    if (isTRUE(result$truncated)) {
      totals$truncated_subreddits <- c(totals$truncated_subreddits, sub)
    }

    # Sleep between subreddits to respect rate limits
    if (sub != shuffled_subs[length(shuffled_subs)]) {
      delay <- runif(1, 2, 5)
      Sys.sleep(delay)
    }
  }

  log_info("Scraping complete:")
  log_info("  Posts added: {totals$posts_added}")
  log_info("  Comments added: {totals$comments_added}")
  log_info("  Posts skipped (already in DB): {totals$posts_skipped}")
  if (totals$comment_fetch_failures > 0) {
    log_warn("Comment fetch failed for {totals$comment_fetch_failures} post(s); they will be retried on the next scrape.")
  }
  if (length(totals$truncated_subreddits) > 0) {
    incomplete <- paste(totals$truncated_subreddits, collapse = ", ")
    log_warn("Incomplete scrape (stopped on API error) for: {incomplete}")
  }

  totals
}

# ==============================================================================
# Internal: small helpers
# ==============================================================================

# Convert a Unix epoch (seconds) to an ISO-8601 UTC string, tolerating a
# missing/NA/non-numeric value (Reddit occasionally omits created_utc on
# special entries). Coerce first so a numeric string ("1700000000") parses
# rather than being zeroed; anything that does not yield a single finite number
# falls back to the epoch. Without this guard as.POSIXct(NULL) yields a
# zero-length value that aborts the parameterized insert.
.epoch_to_iso <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  if (length(n) != 1 || !is.finite(n)) n <- 0
  as.character(as.POSIXct(n, origin = "1970-01-01", tz = "UTC"))
}

# Run fn() inside a single database transaction. Commit on success, roll back
# on error. The on.exit handler is scoped to this one call, so handlers never
# accumulate across iterations and the rollback cannot fire against an already
# committed or disconnected connection.
.with_tx <- function(db, fn) {
  committed <- FALSE
  DBI::dbBegin(db)
  on.exit(if (!committed) DBI::dbRollback(db), add = TRUE)
  out <- fn()
  DBI::dbCommit(db)
  committed <- TRUE
  out
}

# ==============================================================================
# Internal: Reddit API authentication + token management
# ==============================================================================

.get_reddit_credentials <- function(scrape_cfg = NULL) {
  # Resolution order: environment first (the documented, secure path -- keep
  # secrets in .Renviron rather than a committed config), then the config
  # fields as a fallback.
  client_id <- Sys.getenv("REDDIT_CLIENT_ID")
  if (nchar(client_id) == 0) client_id <- scrape_cfg$reddit_client_id %||% ""

  client_secret <- Sys.getenv("REDDIT_CLIENT_SECRET")
  if (nchar(client_secret) == 0) client_secret <- scrape_cfg$reddit_client_secret %||% ""

  user_agent <- Sys.getenv("REDDIT_USER_AGENT")
  if (nchar(user_agent) == 0) user_agent <- scrape_cfg$reddit_user_agent %||% ""
  if (nchar(user_agent) == 0) {
    # Fallback only fires when both env and config are empty. Constructed
    # dynamically from packageVersion so it stays in sync with DESCRIPTION
    # without needing a coordinated edit on every release. The literal
    # 'YourRedditUsername' is intended to make it obvious in Reddit's logs
    # that the integrator forgot to set REDDIT_USER_AGENT properly.
    pkg_version <- tryCatch(
      as.character(utils::packageVersion("pakhom")),
      error = function(e) "dev"
    )
    user_agent <- sprintf("pakhom/%s (by u/YourRedditUsername)", pkg_version)
  }

  if (nchar(client_id) == 0 || nchar(client_secret) == 0) {
    stop(paste0(
      "Reddit API credentials not found.\n",
      "Set environment variables (recommended, in .Renviron):\n",
      "  REDDIT_CLIENT_ID=your_id\n",
      "  REDDIT_CLIENT_SECRET=your_secret\n",
      "Or set them in config.yaml under scraping:\n",
      "  scraping:\n",
      "    reddit_client_id: \"your_id\"\n",
      "    reddit_client_secret: \"your_secret\""
    ))
  }

  list(client_id = client_id, client_secret = client_secret, user_agent = user_agent)
}

.reddit_authenticate <- function(creds) {
  log_info("Authenticating with Reddit API...")

  resp <- tryCatch({
    httr2::request("https://www.reddit.com/api/v1/access_token") |>
      httr2::req_auth_basic(creds$client_id, creds$client_secret) |>
      httr2::req_headers(`User-Agent` = creds$user_agent) |>
      httr2::req_body_form(grant_type = "client_credentials") |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
  }, error = function(e) {
    stop("Reddit authentication failed: ", e$message)
  })

  body <- httr2::resp_body_json(resp)

  if (is.null(body$access_token)) {
    stop("Reddit authentication failed: no access token returned")
  }

  log_info("Reddit authentication successful")
  list(access_token = body$access_token, expires_in = body$expires_in %||% 3600)
}

# Environment-backed token holder. Reddit client-credentials tokens expire
# (commonly ~1 hour), so a long scrape must re-authenticate. get() returns a
# valid token, refreshing proactively shortly before expiry; refresh() forces
# a new token (used when a 401 says the current one was rejected).
.reddit_token_manager <- function(creds) {
  state <- new.env(parent = emptyenv())
  state$token <- NULL
  state$expires_at <- 0

  refresh <- function() {
    auth <- .reddit_authenticate(creds)
    state$token <- auth$access_token
    ttl <- suppressWarnings(as.numeric(auth$expires_in))
    if (is.na(ttl) || ttl <= 0) ttl <- 3600
    # Refresh a minute early so a token never expires mid-request.
    state$expires_at <- as.numeric(Sys.time()) + max(30, ttl - 60)
    invisible(state$token)
  }

  get <- function() {
    if (is.null(state$token) || as.numeric(Sys.time()) >= state$expires_at) {
      refresh()
    }
    state$token
  }

  list(get = get, refresh = refresh)
}

# Number of seconds to wait after a 429, read from the server's Retry-After or
# x-ratelimit-reset header when present, capped so a bad header cannot stall a
# run indefinitely.
.retry_after_seconds <- function(resp, default = 60) {
  for (hdr in c("retry-after", "x-ratelimit-reset")) {
    val <- tryCatch(httr2::resp_header(resp, hdr), error = function(e) NULL)
    if (!is.null(val)) {
      secs <- suppressWarnings(as.numeric(val))
      if (!is.na(secs) && secs >= 0) return(min(secs, 300))
    }
  }
  default
}

# Perform an authenticated GET. Returns a list: $ok (logical), $status (HTTP
# status, NA if the request never completed), and $body (parsed JSON when ok).
# A failed request is reported as ok = FALSE rather than an ambiguous NULL, so
# callers can distinguish "no more data" from "the request failed".
.reddit_api_get <- function(url, tokmgr, creds, max_retries = 4) {
  refreshed <- FALSE
  for (attempt in seq_len(max_retries)) {
    token <- tokmgr$get()
    resp <- tryCatch({
      httr2::request(url) |>
        httr2::req_headers(
          Authorization = paste("bearer", token),
          `User-Agent` = creds$user_agent
        ) |>
        # Handle HTTP statuses ourselves so 401/403/404/429 are inspectable
        # rather than thrown.
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_timeout(30) |>
        httr2::req_perform()
    }, error = function(e) {
      if (attempt < max_retries) {
        log_warn("API request failed (attempt {attempt}/{max_retries}): {e$message}")
        Sys.sleep(2 * attempt)
      }
      NULL
    })

    if (is.null(resp)) next

    status <- httr2::resp_status(resp)
    if (status == 200) {
      # A 200 can still carry a non-JSON body (a CDN/interstitial HTML page or
      # a truncated response). Parse defensively so that surfaces as ok = FALSE
      # rather than throwing out of the scrape.
      body <- tryCatch(httr2::resp_body_json(resp), error = function(e) {
        log_warn("200 response was not valid JSON: {e$message}")
        NULL
      })
      if (is.null(body)) {
        if (attempt < max_retries) { Sys.sleep(2 * attempt); next }
        return(list(ok = FALSE, status = 200L, body = NULL))
      }
      return(list(ok = TRUE, status = 200L, body = body))
    }
    if (status == 401 && !refreshed) {
      log_warn("Access token rejected (401); re-authenticating")
      tokmgr$refresh()
      refreshed <- TRUE
      next
    }
    if (status == 429) {
      wait <- .retry_after_seconds(resp, default = 60)
      log_warn("Rate limited (429). Waiting {wait}s...")
      Sys.sleep(wait)
      next
    }
    if (status >= 500) {
      log_warn("Server error {status}. Retrying...")
      Sys.sleep(5 * attempt)
      next
    }
    # Other 4xx (403 private/banned, 404 missing, ...): not retryable.
    log_warn("API returned status {status} for {url}")
    return(list(ok = FALSE, status = as.integer(status), body = NULL))
  }

  log_warn("Max retries reached for {url}")
  list(ok = FALSE, status = NA_integer_, body = NULL)
}

# ==============================================================================
# Internal: Database initialization
# ==============================================================================

.init_scraper_db <- function(db_path) {
  db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  DBI::dbExecute(db, "CREATE TABLE IF NOT EXISTS posts (
    post_id TEXT PRIMARY KEY,
    subreddit TEXT,
    title TEXT,
    created_utc TEXT,
    num_comments INTEGER,
    score INTEGER,
    upvote_ratio REAL,
    author TEXT,
    text TEXT,
    permalink TEXT,
    scraped_at TEXT DEFAULT (datetime('now')),
    comments_scraped_at TEXT
  )")

  DBI::dbExecute(db, "CREATE TABLE IF NOT EXISTS comments (
    comment_id TEXT PRIMARY KEY,
    post_id TEXT,
    subreddit TEXT,
    created_utc TEXT,
    score INTEGER,
    author TEXT,
    comment_body TEXT,
    permalink TEXT,
    scraped_at TEXT DEFAULT (datetime('now'))
  )")

  # Migrate existing tables: add columns that may be missing from old schemas
  existing_post_cols <- DBI::dbListFields(db, "posts")
  if (!"upvote_ratio" %in% existing_post_cols) {
    DBI::dbExecute(db, "ALTER TABLE posts ADD COLUMN upvote_ratio REAL")
  }
  if (!"scraped_at" %in% existing_post_cols) {
    DBI::dbExecute(db, "ALTER TABLE posts ADD COLUMN scraped_at TEXT")
  }
  # Marks when a post's comments were successfully fetched. NULL means never
  # fetched (a failed fetch, or a row from before comment scraping), which a
  # re-scrape uses to backfill comments rather than silently leaving the post
  # comment-less.
  if (!"comments_scraped_at" %in% existing_post_cols) {
    DBI::dbExecute(db, "ALTER TABLE posts ADD COLUMN comments_scraped_at TEXT")
  }

  existing_comment_cols <- DBI::dbListFields(db, "comments")
  if (!"scraped_at" %in% existing_comment_cols) {
    DBI::dbExecute(db, "ALTER TABLE comments ADD COLUMN scraped_at TEXT")
  }

  DBI::dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_comments_post_id ON comments(post_id)")
  DBI::dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_posts_subreddit ON posts(subreddit)")
}

# ==============================================================================
# Internal: Scrape a single subreddit
# ==============================================================================

.scrape_subreddit <- function(subreddit, db_path, tokmgr, creds, max_posts,
                               include_comments, sort_by, time_filter) {
  log_info("Scraping r/{subreddit} ({sort_by}, limit={max_posts})...")

  db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  posts_added <- 0L
  comments_added <- 0L
  posts_skipped <- 0L
  comment_failures <- 0L
  after <- NULL
  chunk_size <- 100L  # Reddit API max per request
  pages <- 0L
  max_pages <- 50L    # backstop; Reddit listings cap well below this
  truncated <- FALSE

  # The bar tracks NEW posts toward the budget and is only advanced on an
  # insert, so it never ticks past its total (which would abort the run).
  pb <- safe_progress_bar(
    format = "  [:bar] :current/:total posts",
    total = max_posts
  )

  # Budget counts only newly-added posts, so already-seen entries do not
  # consume it; pagination continues until the budget is met, the listing
  # ends, or the page backstop is hit.
  while (posts_added < max_posts && pages < max_pages) {
    pages <- pages + 1L

    # Build URL. URL-encode the externally-derived path/query components
    # (subreddit, after cursor) so a name with reserved characters cannot
    # alter the request path or inject query parameters. sort_by/time_filter
    # are validated by match.arg() in scrape_reddit().
    url <- sprintf("https://oauth.reddit.com/r/%s/%s.json?limit=%d&t=%s",
                    utils::URLencode(as.character(subreddit), reserved = TRUE),
                    sort_by, chunk_size, time_filter)
    if (!is.null(after)) {
      url <- paste0(url, "&after=",
                    utils::URLencode(as.character(after), reserved = TRUE))
    }

    res <- .reddit_api_get(url, tokmgr, creds)
    if (!isTRUE(res$ok)) {
      # A failed request (404/403/persistent 429/5xx/network) stops this
      # subreddit early; flag it so the caller knows the result is partial.
      truncated <- TRUE
      break
    }

    data <- res$body
    posts <- data$data$children
    if (length(posts) == 0) break

    page <- .process_post_page(
      db = db, posts = posts, subreddit = subreddit, tokmgr = tokmgr,
      creds = creds, include_comments = include_comments,
      budget = max_posts - posts_added, pb = pb
    )

    posts_added <- posts_added + page$added
    posts_skipped <- posts_skipped + page$skipped
    comments_added <- comments_added + page$comments
    comment_failures <- comment_failures + page$comment_failures

    # Pagination
    after <- data$data$after
    if (is.null(after)) break

    # Rate limit pause
    Sys.sleep(runif(1, 1, 2))
  }

  status_note <- if (truncated) " (INCOMPLETE: stopped on API error)" else ""
  log_info("  r/{subreddit}: +{posts_added} posts, +{comments_added} comments, {posts_skipped} skipped{status_note}")

  list(posts_added = posts_added, comments_added = comments_added,
       posts_skipped = posts_skipped, truncated = truncated,
       comment_failures = comment_failures)
}

# Insert a page of posts in one transaction, then fetch comments for the newly
# added posts AFTER the commit so a long comment crawl never holds a write
# lock and a comment failure cannot discard the page's posts. Already-stored
# posts whose comments were never successfully fetched are backfilled, so a
# transient comment failure does not leave a permanently comment-less post.
.process_post_page <- function(db, posts, subreddit, tokmgr, creds,
                                include_comments, budget, pb) {
  res <- .with_tx(db, function() {
    added <- 0L
    skipped <- 0L
    new_ids <- character(0)
    backfill_ids <- character(0)

    for (post in posts) {
      if (added >= budget) break  # honor the per-subreddit budget exactly
      pd <- post$data
      if (is.null(pd$id)) next

      wants_comments <- include_comments && (pd$num_comments %||% 0) > 0
      existing <- DBI::dbGetQuery(db,
        "SELECT comments_scraped_at FROM posts WHERE post_id = ? LIMIT 1",
        params = list(pd$id)
      )
      if (nrow(existing) > 0) {
        skipped <- skipped + 1L
        # Backfill a post stored without its comments (a prior fetch failed, or
        # the row predates comment scraping).
        if (wants_comments && is.na(existing$comments_scraped_at[1])) {
          backfill_ids <- c(backfill_ids, pd$id)
        }
        next
      }

      DBI::dbExecute(db,
        "INSERT OR IGNORE INTO posts (post_id, subreddit, title, created_utc, num_comments, score, upvote_ratio, author, text, permalink) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          pd$id,
          subreddit,
          pd$title %||% "",
          .epoch_to_iso(pd$created_utc),
          pd$num_comments %||% 0L,
          pd$score %||% 0L,
          pd$upvote_ratio %||% NA_real_,
          pd$author %||% "[deleted]",
          pd$selftext %||% "",
          paste0("https://www.reddit.com", pd$permalink %||% "")
        )
      )
      added <- added + 1L
      pb$tick()

      if (wants_comments) new_ids <- c(new_ids, pd$id)
    }

    list(added = added, skipped = skipped, new_ids = new_ids,
         backfill_ids = backfill_ids)
  })

  # Fetch comments after the commit. On a successful fetch, stamp
  # comments_scraped_at so the post is not re-fetched next run; a failed fetch
  # leaves it NULL (to retry) and is counted so the caller can see it.
  comments <- 0L
  comment_failures <- 0L
  # unique() guards the rare case of a post_id appearing twice on one page,
  # which would otherwise hit the comment endpoint twice in a single run.
  for (pid in unique(c(res$new_ids, res$backfill_ids))) {
    r <- .scrape_post_comments(db, subreddit, pid, tokmgr, creds)
    comments <- comments + r$added
    if (isTRUE(r$ok)) {
      DBI::dbExecute(db,
        "UPDATE posts SET comments_scraped_at = datetime('now') WHERE post_id = ?",
        params = list(pid))
    } else {
      comment_failures <- comment_failures + 1L
    }
  }

  list(added = res$added, skipped = res$skipped, comments = comments,
       comment_failures = comment_failures)
}

# ==============================================================================
# Internal: Scrape the full comment tree for a single post
# ==============================================================================

# Returns list(ok, added): ok is FALSE only when the comment listing request
# itself failed (so the caller can retry that post later), TRUE when a response
# was obtained even if it held no storable comments. added is the count newly
# inserted.
.scrape_post_comments <- function(db, subreddit, post_id, tokmgr, creds) {
  url <- sprintf("https://oauth.reddit.com/r/%s/comments/%s.json?limit=500&depth=10",
                  utils::URLencode(as.character(subreddit), reserved = TRUE),
                  utils::URLencode(as.character(post_id), reserved = TRUE))

  res <- .reddit_api_get(url, tokmgr, creds)
  if (!isTRUE(res$ok)) return(list(ok = FALSE, added = 0L))

  data <- res$body
  if (length(data) < 2) return(list(ok = TRUE, added = 0L))

  children <- data[[2]]$data$children
  if (is.null(children) || length(children) == 0) return(list(ok = TRUE, added = 0L))

  # Walk the embedded tree, collecting any "load more" placeholders, then
  # expand those via the API so the whole discussion is captured.
  more_acc <- new.env(parent = emptyenv())
  more_acc$ids <- character(0)

  added <- .with_tx(db, function() {
    .insert_comment_tree(db, subreddit, post_id, children, more_acc)
  })

  # ok stays TRUE only if every "load more" expansion also succeeded, so a
  # failed expansion leaves the post un-stamped (retried + counted) rather than
  # silently recording the deep-thread tail as complete.
  ok <- TRUE
  if (length(more_acc$ids) > 0) {
    exp <- .expand_more_comments(
      db, subreddit, post_id, unique(more_acc$ids), tokmgr, creds
    )
    added <- added + exp$added
    ok <- isTRUE(exp$ok)
  }

  list(ok = ok, added = added)
}

# Recursively insert a list of comment nodes. t1 nodes are stored (and their
# nested $replies walked); "more" nodes (truncated threads) have their child
# ids collected into more_acc for later API expansion. Returns the number of
# comments newly inserted. depth/max_depth bound the recursion: Reddit returns
# at most depth=10 of embedded replies, so the generous cap is only a backstop
# against a pathological payload, never a limit on real data.
.insert_comment_tree <- function(db, subreddit, post_id, children, more_acc,
                                  depth = 0L, max_depth = 50L) {
  added <- 0L
  if (depth >= max_depth) return(added)

  for (child in children) {
    kind <- child$kind
    cd <- child$data
    if (is.null(cd)) next

    if (identical(kind, "more")) {
      ids <- unlist(cd$children, use.names = FALSE)
      if (length(ids) > 0) more_acc$ids <- c(more_acc$ids, as.character(ids))
      next
    }

    if (!identical(kind, "t1")) next

    if (!is.null(cd$id) && !is.null(cd$body) &&
        !(cd$body %in% c("[deleted]", "[removed]"))) {
      exists <- DBI::dbGetQuery(db,
        "SELECT 1 FROM comments WHERE comment_id = ? LIMIT 1",
        params = list(cd$id)
      )
      if (nrow(exists) == 0) {
        DBI::dbExecute(db,
          "INSERT OR IGNORE INTO comments (comment_id, post_id, subreddit, created_utc, score, author, comment_body, permalink) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
          params = list(
            cd$id,
            post_id,
            subreddit,
            .epoch_to_iso(cd$created_utc),
            cd$score %||% 0L,
            cd$author %||% "[deleted]",
            cd$body,
            paste0("https://www.reddit.com", cd$permalink %||% "")
          )
        )
        added <- added + 1L
      }
    }

    # Recurse into nested replies. Reddit stores these under
    # cd$replies$data$children; it is "" (empty string) when there are none.
    replies <- cd$replies
    if (is.list(replies) && !is.null(replies$data) &&
        !is.null(replies$data$children)) {
      added <- added + .insert_comment_tree(
        db, subreddit, post_id, replies$data$children, more_acc,
        depth + 1L, max_depth
      )
    }
  }

  added
}

# Expand "load more comments" placeholders via the morechildren endpoint, in
# batches of 100 ids, recursing into any further placeholders the expansion
# returns (bounded by max_depth so a pathological thread cannot loop forever).
# Returns list(ok, added): ok is FALSE if any morechildren request failed, so
# the caller can leave the post for a later backfill rather than recording an
# incomplete deep thread as done. Hitting max_depth is an accepted truncation,
# not a failure.
.expand_more_comments <- function(db, subreddit, post_id, more_ids, tokmgr, creds,
                                   depth = 0L, max_depth = 8L) {
  if (length(more_ids) == 0 || depth >= max_depth) return(list(ok = TRUE, added = 0L))

  link_id <- paste0("t3_", post_id)
  added <- 0L
  all_ok <- TRUE
  remaining <- more_ids

  while (length(remaining) > 0) {
    batch <- utils::head(remaining, 100L)
    remaining <- if (length(remaining) > 100L) remaining[-(1:100)] else character(0)

    url <- sprintf(
      "https://oauth.reddit.com/api/morechildren.json?api_type=json&link_id=%s&children=%s&limit_children=false",
      link_id,
      paste(utils::URLencode(batch, reserved = TRUE), collapse = ",")
    )

    res <- .reddit_api_get(url, tokmgr, creds)
    if (!isTRUE(res$ok)) { all_ok <- FALSE; next }

    things <- res$body$json$data$things
    if (is.null(things) || length(things) == 0) next

    next_acc <- new.env(parent = emptyenv())
    next_acc$ids <- character(0)

    added <- added + .with_tx(db, function() {
      .insert_comment_tree(db, subreddit, post_id, things, next_acc)
    })

    if (length(next_acc$ids) > 0) {
      sub <- .expand_more_comments(
        db, subreddit, post_id, unique(next_acc$ids), tokmgr, creds,
        depth + 1L, max_depth
      )
      added <- added + sub$added
      all_ok <- all_ok && isTRUE(sub$ok)
    }
  }

  list(ok = all_ok, added = added)
}
