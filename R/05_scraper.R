# ==============================================================================
# Reddit Scraper — Broad Subreddit Collection for Thematic Analysis
# ==============================================================================
# Scrapes entire subreddits (posts + comments) into SQLite. The progressive
# sequential coder downstream decides which entries match the research
# question, so no keyword pre-filtering is applied here.
# ==============================================================================

#' Scrape Reddit subreddits into a SQLite database
#'
#' Authenticates with the Reddit API and collects posts and comments from
#' specified subreddits. Unlike keyword-based scrapers, this collects ALL
#' content and lets the progressive sequential coder downstream decide
#' which entries match the research question (no keyword pre-filter).
#'
#' @param config ThematicConfig object or list with $scraping section
#' @param db_path Path to SQLite database (created if missing)
#' @param subreddits Character vector of subreddit names (without "r/")
#' @param posts_per_subreddit Max posts to fetch per subreddit (default 500)
#' @param include_comments Logical; also scrape comments (default TRUE)
#' @param sort_by Sort method: "new", "hot", "top", "rising" (default "new")
#' @param time_filter Time filter for "top" sort: "hour", "day", "week",
#'   "month", "year", "all" (default "all")
#' @return List with counts: posts_added, comments_added, posts_skipped
#' @export
scrape_reddit <- function(config = NULL, db_path = NULL, subreddits = NULL,
                           posts_per_subreddit = 500, include_comments = TRUE,
                           sort_by = "new", time_filter = "all") {

  # Resolve config
  scrape_cfg <- NULL
  if (inherits(config, "ThematicConfig")) {
    scrape_cfg <- config$scraping
    db_path <- db_path %||% config$data$database
    subreddits <- subreddits %||% scrape_cfg$subreddits
    posts_per_subreddit <- scrape_cfg$posts_per_subreddit %||% posts_per_subreddit
    include_comments <- scrape_cfg$include_comments %||% include_comments
    sort_by <- scrape_cfg$sort_by %||% sort_by
    time_filter <- scrape_cfg$time_filter %||% time_filter
  } else if (is.list(config)) {
    scrape_cfg <- config
    subreddits <- subreddits %||% scrape_cfg$subreddits
  }

  if (is.null(db_path)) stop("db_path is required")
  if (is.null(subreddits) || length(subreddits) == 0) stop("subreddits is required")

  # Authenticate
  creds <- .get_reddit_credentials(scrape_cfg)
  token <- .reddit_authenticate(creds)

  # Initialize database
  .init_scraper_db(db_path)

  log_info("Starting Reddit scrape: {length(subreddits)} subreddit(s), {posts_per_subreddit} posts each")

  totals <- list(posts_added = 0L, comments_added = 0L, posts_skipped = 0L)

  # Randomize order to reduce pattern detection
  shuffled_subs <- sample(subreddits)
  for (sub in shuffled_subs) {
    result <- .scrape_subreddit(
      subreddit = sub,
      db_path = db_path,
      token = token,
      creds = creds,
      max_posts = posts_per_subreddit,
      include_comments = include_comments,
      sort_by = sort_by,
      time_filter = time_filter
    )

    totals$posts_added <- totals$posts_added + result$posts_added
    totals$comments_added <- totals$comments_added + result$comments_added
    totals$posts_skipped <- totals$posts_skipped + result$posts_skipped

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

  totals
}

# ==============================================================================
# Internal: Reddit API authentication
# ==============================================================================

.get_reddit_credentials <- function(scrape_cfg = NULL) {
  # Try config first, then env vars
  client_id <- scrape_cfg$reddit_client_id %||% ""
  if (nchar(client_id) == 0) {
    client_id <- Sys.getenv("REDDIT_CLIENT_ID")
  }

  client_secret <- scrape_cfg$reddit_client_secret %||% ""
  if (nchar(client_secret) == 0) {
    client_secret <- Sys.getenv("REDDIT_CLIENT_SECRET")
  }

  user_agent <- scrape_cfg$reddit_user_agent %||% ""
  if (nchar(user_agent) == 0) {
    # Fallback only fires when both config and env are empty. Constructed
    # dynamically from packageVersion so it stays in sync with DESCRIPTION
    # without needing a coordinated edit on every release. The literal
    # 'YourRedditUsername' is intended to make it obvious in Reddit's logs
    # that the integrator forgot to set REDDIT_USER_AGENT properly.
    pkg_version <- tryCatch(
      as.character(utils::packageVersion("pakhom")),
      error = function(e) "dev"
    )
    default_ua <- sprintf("pakhom/%s (by u/YourRedditUsername)", pkg_version)
    user_agent <- Sys.getenv("REDDIT_USER_AGENT", default_ua)
  }

  if (nchar(client_id) == 0 || nchar(client_secret) == 0) {
    stop(paste0(
      "Reddit API credentials not found.\n",
      "Set them in config.yaml under scraping:\n",
      "  scraping:\n",
      "    reddit_client_id: \"your_id\"\n",
      "    reddit_client_secret: \"your_secret\"\n",
      "Or set environment variables:\n",
      "  Sys.setenv(REDDIT_CLIENT_ID = 'your_id')\n",
      "  Sys.setenv(REDDIT_CLIENT_SECRET = 'your_secret')"
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
  body$access_token
}

.reddit_api_get <- function(url, token, creds, max_retries = 3) {
  for (attempt in seq_len(max_retries)) {
    resp <- tryCatch({
      httr2::request(url) |>
        httr2::req_headers(
          Authorization = paste("bearer", token),
          `User-Agent` = creds$user_agent
        ) |>
        httr2::req_timeout(30) |>
        httr2::req_perform()
    }, error = function(e) {
      if (attempt < max_retries) {
        log_warn("API request failed (attempt {attempt}/{max_retries}): {e$message}")
        Sys.sleep(2 * attempt)
      }
      NULL
    })

    if (!is.null(resp)) {
      status <- httr2::resp_status(resp)
      if (status == 200) return(httr2::resp_body_json(resp))
      if (status == 429) {
        wait <- 60
        log_warn("Rate limited. Waiting {wait}s...")
        Sys.sleep(wait)
      } else if (status >= 500) {
        log_warn("Server error {status}. Retrying...")
        Sys.sleep(5 * attempt)
      } else {
        log_warn("API returned status {status}")
        return(NULL)
      }
    }
  }

  log_warn("Max retries reached for {url}")
  NULL
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
    scraped_at TEXT DEFAULT (datetime('now'))
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

.scrape_subreddit <- function(subreddit, db_path, token, creds, max_posts,
                               include_comments, sort_by, time_filter) {
  log_info("Scraping r/{subreddit} ({sort_by}, limit={max_posts})...")

  db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  posts_added <- 0L
  comments_added <- 0L
  posts_skipped <- 0L
  after <- NULL
  chunk_size <- 100  # Reddit API max per request

  pb <- safe_progress_bar(
    format = "  r/{subreddit} [:bar] :current posts processed",
    total = max_posts
  )

  while (posts_added + posts_skipped < max_posts) {
    # Build URL. URL-encode the externally-derived path/query components
    # (subreddit, after cursor) so a name with reserved characters cannot
    # alter the request path or inject query parameters.
    url <- sprintf("https://oauth.reddit.com/r/%s/%s.json?limit=%d&t=%s",
                    utils::URLencode(as.character(subreddit), reserved = TRUE),
                    sort_by, chunk_size, time_filter)
    if (!is.null(after)) {
      url <- paste0(url, "&after=",
                    utils::URLencode(as.character(after), reserved = TRUE))
    }

    data <- .reddit_api_get(url, token, creds)
    if (is.null(data)) break

    posts <- data$data$children
    if (length(posts) == 0) break

    committed <- FALSE
    DBI::dbBegin(db)
    on.exit(if (!committed) DBI::dbRollback(db), add = TRUE)

    for (post in posts) {
      pd <- post$data
      if (is.null(pd$id)) next

      # Check if already in DB
      exists <- DBI::dbGetQuery(db,
        "SELECT 1 FROM posts WHERE post_id = ? LIMIT 1",
        params = list(pd$id)
      )

      if (nrow(exists) > 0) {
        posts_skipped <- posts_skipped + 1L
        pb$tick()
        next
      }

      # Insert post
      created <- as.character(as.POSIXct(pd$created_utc, origin = "1970-01-01", tz = "UTC"))
      DBI::dbExecute(db,
        "INSERT OR IGNORE INTO posts (post_id, subreddit, title, created_utc, num_comments, score, upvote_ratio, author, text, permalink) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          pd$id,
          subreddit,
          pd$title %||% "",
          created,
          pd$num_comments %||% 0L,
          pd$score %||% 0L,
          pd$upvote_ratio %||% NA_real_,
          pd$author %||% "[deleted]",
          pd$selftext %||% "",
          paste0("https://www.reddit.com", pd$permalink %||% "")
        )
      )
      posts_added <- posts_added + 1L

      # Fetch comments if requested
      if (include_comments && (pd$num_comments %||% 0) > 0) {
        n_comments <- .scrape_post_comments(db, subreddit, pd$id, token, creds)
        comments_added <- comments_added + n_comments
      }

      pb$tick()
    }

    DBI::dbCommit(db)
    committed <- TRUE

    # Pagination
    after <- data$data$after
    if (is.null(after)) break

    # Rate limit pause
    Sys.sleep(runif(1, 1, 2))
  }

  log_info("  r/{subreddit}: +{posts_added} posts, +{comments_added} comments, {posts_skipped} skipped")

  list(posts_added = posts_added, comments_added = comments_added, posts_skipped = posts_skipped)
}

# ==============================================================================
# Internal: Scrape comments for a single post
# ==============================================================================

.scrape_post_comments <- function(db, subreddit, post_id, token, creds) {
  url <- sprintf("https://oauth.reddit.com/r/%s/comments/%s.json?limit=200",
                  utils::URLencode(as.character(subreddit), reserved = TRUE),
                  utils::URLencode(as.character(post_id), reserved = TRUE))

  data <- .reddit_api_get(url, token, creds)
  if (is.null(data) || length(data) < 2) return(0L)

  comments_data <- data[[2]]$data$children
  if (is.null(comments_data)) return(0L)

  added <- 0L

  for (comment in comments_data) {
    cd <- comment$data
    if (is.null(cd$body) || is.null(cd$id)) next
    if (cd$body == "[deleted]" || cd$body == "[removed]") next

    exists <- DBI::dbGetQuery(db,
      "SELECT 1 FROM comments WHERE comment_id = ? LIMIT 1",
      params = list(cd$id)
    )
    if (nrow(exists) > 0) next

    created <- as.character(as.POSIXct(cd$created_utc %||% 0, origin = "1970-01-01", tz = "UTC"))

    DBI::dbExecute(db,
      "INSERT OR IGNORE INTO comments (comment_id, post_id, subreddit, created_utc, score, author, comment_body, permalink) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        cd$id,
        post_id,
        subreddit,
        created,
        cd$score %||% 0L,
        cd$author %||% "[deleted]",
        cd$body,
        paste0("https://www.reddit.com", cd$permalink %||% "")
      )
    )
    added <- added + 1L
  }

  added
}
