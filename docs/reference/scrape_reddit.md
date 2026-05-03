# Scrape Reddit subreddits into a SQLite database

Authenticates with the Reddit API and collects posts and comments from
specified subreddits. Unlike keyword-based scrapers, this collects ALL
content and lets the analysis pipeline's relevance filter determine
what's useful for the research question.

## Usage

``` r
scrape_reddit(
  config = NULL,
  db_path = NULL,
  subreddits = NULL,
  posts_per_subreddit = 500,
  include_comments = TRUE,
  sort_by = "new",
  time_filter = "all"
)
```

## Arguments

- config:

  ThematicConfig object or list with \$scraping section

- db_path:

  Path to SQLite database (created if missing)

- subreddits:

  Character vector of subreddit names (without "r/")

- posts_per_subreddit:

  Max posts to fetch per subreddit (default 500)

- include_comments:

  Logical; also scrape comments (default TRUE)

- sort_by:

  Sort method: "new", "hot", "top", "rising" (default "new")

- time_filter:

  Time filter for "top" sort: "hour", "day", "week", "month", "year",
  "all" (default "all")

## Value

List with counts: posts_added, comments_added, posts_skipped
