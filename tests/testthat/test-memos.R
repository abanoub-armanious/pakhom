# Tests for the reflexive-memos module (R/memos.R) -- M1.3.
# Closes an earlier audit HIGH #2: ResearcherReflectionLog had a
# memos slot but no CRUD. Per AC6 (symmetric obligations), Mode 1's
# burden parity vs Modes 2/3 is delivered through reflexive memos at
# pause points. These tests pin the constructor, CRUD, Markdown round-
# trip, and persistence contracts.

# ---- make_memo: constructor + validation ---------------------------------

test_that("make_memo constructs a Memo S3 object with default fields", {
  m <- make_memo("Body content")
  expect_s3_class(m, "Memo")
  expect_equal(m$body, "Body content")
  expect_equal(m$type, "theoretical")  # default
  expect_equal(m$author, "researcher")  # default
  expect_equal(m$linked_codes, character(0))
  expect_equal(m$linked_themes, character(0))
  expect_equal(m$linked_entries, character(0))
  expect_true(is.na(m$linked_prior_memo))
  expect_match(m$id, "^memo_\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}")
  expect_equal(m$schema_version, "1.0.0")
})

test_that("make_memo rejects invalid type", {
  expect_error(make_memo("body", type = "FAKE"),
               "invalid type")
})

test_that("make_memo rejects non-character body", {
  expect_error(make_memo(42), "must be a single character string")
  expect_error(make_memo(c("a", "b")), "must be a single character string")
})

test_that("make_memo rejects empty author", {
  expect_error(make_memo("body", author = ""),
               "single non-empty string")
})

test_that("make_memo accepts all four valid memo types", {
  for (t in c("operational", "coding", "theoretical", "positionality")) {
    m <- make_memo("body", type = t)
    expect_equal(m$type, t)
  }
})

test_that("make_memo accepts links + linked_prior_memo", {
  m <- make_memo("body",
                   linked_codes = c("c1", "c2"),
                   linked_themes = c("T1"),
                   linked_entries = c("e1", "e2", "e3"),
                   linked_prior_memo = "memo_prior")
  expect_equal(m$linked_codes, c("c1", "c2"))
  expect_equal(m$linked_themes, c("T1"))
  expect_equal(m$linked_entries, c("e1", "e2", "e3"))
  expect_equal(m$linked_prior_memo, "memo_prior")
})

test_that("make_memo NULL/empty linked_prior_memo becomes NA_character_", {
  m1 <- make_memo("body", linked_prior_memo = NULL)
  m2 <- make_memo("body", linked_prior_memo = "")
  expect_true(is.na(m1$linked_prior_memo))
  expect_true(is.na(m2$linked_prior_memo))
})

test_that("make_memo generates unique ids on rapid construction", {
  ids <- character(50)
  for (i in seq_len(50)) ids[i] <- make_memo("x")$id
  expect_equal(length(unique(ids)), 50L)
})

test_that("print.Memo produces a structured summary", {
  m <- make_memo("body content", type = "coding",
                   linked_codes = c("c1"))
  out <- capture.output(print(m))
  expect_true(any(grepl("Memo \\[coding\\]", out)))
  expect_true(any(grepl("body content", out)))
  expect_true(any(grepl("Linked codes:", out)))
})

# ---- add_memo: CRUD ------------------------------------------------------

test_that("add_memo appends to log$memos and updates last_updated", {
  log <- create_reflection_log()
  Sys.sleep(0.05)
  expect_length(log$memos, 0L)

  log2 <- add_memo(log, body = "first memo")
  expect_length(log2$memos, 1L)
  expect_s3_class(log2$memos[[1L]], "Memo")
  expect_true(log2$last_updated >= log$last_updated)
})

test_that("add_memo accepts a pre-built Memo via memo arg", {
  log <- create_reflection_log()
  m <- make_memo("pre-built", type = "coding")
  log <- add_memo(log, memo = m)
  expect_length(log$memos, 1L)
  expect_equal(log$memos[[1L]]$type, "coding")
  expect_equal(log$memos[[1L]]$body, "pre-built")
})

test_that("add_memo refuses both body + memo (mutually exclusive)", {
  log <- create_reflection_log()
  m <- make_memo("x")
  expect_error(add_memo(log, body = "y", memo = m),
               "mutually exclusive")
})

test_that("add_memo refuses neither body nor memo", {
  log <- create_reflection_log()
  expect_error(add_memo(log), "must supply either body")
})

test_that("add_memo rejects non-ResearcherReflectionLog input", {
  expect_error(add_memo(list(memos = list()), body = "x"),
               "ResearcherReflectionLog")
})

# ---- read_memo + list_memos ---------------------------------------------

test_that("read_memo returns the memo by id; NULL when absent", {
  log <- create_reflection_log()
  log <- add_memo(log, body = "first", id = "memo_AAA")
  log <- add_memo(log, body = "second", id = "memo_BBB")
  expect_equal(read_memo(log, "memo_AAA")$body, "first")
  expect_equal(read_memo(log, "memo_BBB")$body, "second")
  expect_null(read_memo(log, "memo_DOES_NOT_EXIST"))
})

test_that("list_memos returns an empty tibble when no memos", {
  log <- create_reflection_log()
  out <- list_memos(log)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("id", "timestamp", "author", "type",
                         "n_linked_codes", "n_linked_themes",
                         "n_linked_entries", "body_chars", "body_preview"))
})

test_that("list_memos one row per memo with link counts", {
  log <- create_reflection_log()
  log <- add_memo(log, body = "first",
                    linked_codes = c("c1", "c2"),
                    linked_themes = c("T1"))
  log <- add_memo(log, body = "second",
                    linked_entries = c("e1"))
  out <- list_memos(log)
  expect_equal(nrow(out), 2L)
  expect_equal(out$n_linked_codes, c(2L, 0L))
  expect_equal(out$n_linked_themes, c(1L, 0L))
  expect_equal(out$n_linked_entries, c(0L, 1L))
})

test_that("list_memos filters by type / author / linked_theme", {
  log <- create_reflection_log()
  log <- add_memo(log, body = "op", type = "operational")
  log <- add_memo(log, body = "th1", type = "theoretical",
                    linked_themes = c("T1"))
  log <- add_memo(log, body = "th2", type = "theoretical",
                    author = "second_researcher",
                    linked_themes = c("T2"))
  expect_equal(nrow(list_memos(log, type = "theoretical")), 2L)
  expect_equal(nrow(list_memos(log, type = "operational")), 1L)
  expect_equal(nrow(list_memos(log, author = "second_researcher")), 1L)
  expect_equal(nrow(list_memos(log, linked_theme = "T1")), 1L)
})

# ---- Markdown round-trip ------------------------------------------------

test_that("memo_to_markdown produces YAML frontmatter + body", {
  m <- make_memo("Body line 1\n\nBody line 2",
                   type = "coding",
                   linked_codes = c("c1", "c2"))
  md <- memo_to_markdown(m)
  expect_match(md, "^---\\n")
  expect_match(md, "type: 'coding'")
  expect_match(md, "linked_codes: \\['c1', 'c2'\\]")
  expect_match(md, "Body line 1")
  expect_match(md, "Body line 2")
})

test_that("markdown_to_memo round-trips body content byte-equivalently", {
  m_orig <- make_memo(
    "Initial reflection: corpus surfaces themes around adherence.\n\nBut entries from contributor X are over-represented; need to interrogate.",
    type = "theoretical",
    linked_themes = c("Adherence")
  )
  md <- memo_to_markdown(m_orig)
  m_parsed <- markdown_to_memo(md)
  expect_s3_class(m_parsed, "Memo")
  expect_equal(m_parsed$body, m_orig$body)
  expect_equal(m_parsed$id, m_orig$id)
  expect_equal(m_parsed$type, m_orig$type)
  expect_equal(m_parsed$linked_themes, m_orig$linked_themes)
})

test_that("Markdown round-trip preserves all link arrays", {
  m_orig <- make_memo(
    "linked memo",
    type = "operational",
    linked_codes = c("c1", "c2", "c3"),
    linked_themes = c("T1", "T2"),
    linked_entries = c("e1", "e2"),
    linked_prior_memo = "memo_prior_xyz"
  )
  md <- memo_to_markdown(m_orig)
  m_parsed <- markdown_to_memo(md)
  expect_equal(m_parsed$linked_codes, m_orig$linked_codes)
  expect_equal(m_parsed$linked_themes, m_orig$linked_themes)
  expect_equal(m_parsed$linked_entries, m_orig$linked_entries)
  expect_equal(m_parsed$linked_prior_memo, "memo_prior_xyz")
})

test_that("Markdown round-trip preserves NULL linked_prior_memo as NA", {
  m_orig <- make_memo("standalone", linked_prior_memo = NULL)
  md <- memo_to_markdown(m_orig)
  expect_match(md, "linked_prior_memo: null")
  m_parsed <- markdown_to_memo(md)
  expect_true(is.na(m_parsed$linked_prior_memo))
})

test_that("Markdown round-trip handles bodies with apostrophes + quotes + colons", {
  body <- "It's tough; \"bad\" feelings: \nthings like 'hopelessness' come up.\n\nKey takeaway: this isn't simple."
  m_orig <- make_memo(body)
  md <- memo_to_markdown(m_orig)
  m_parsed <- markdown_to_memo(md)
  expect_equal(m_parsed$body, body)
})

test_that("Markdown round-trip handles author names with apostrophes", {
  m_orig <- make_memo("body", author = "O'Brien")
  md <- memo_to_markdown(m_orig)
  m_parsed <- markdown_to_memo(md)
  expect_equal(m_parsed$author, "O'Brien")
})

test_that("markdown_to_memo rejects input without YAML frontmatter", {
  expect_error(markdown_to_memo("just body content, no frontmatter"),
               "does not start with a YAML frontmatter")
})

test_that("markdown_to_memo rejects unterminated frontmatter", {
  bad <- "---\nid: xyz\nauthor: x\n(no closing dashes)"
  expect_error(markdown_to_memo(bad),
               "not terminated by '---'")
})

# ---- Persistence: persist_memos + load_memos ----------------------------

test_that("persist_memos writes one .md per memo under run_dir/memos/", {
  log <- create_reflection_log()
  log <- add_memo(log, body = "first", type = "operational")
  log <- add_memo(log, body = "second", type = "theoretical")
  d <- withr::local_tempdir()
  paths <- persist_memos(log, d)
  expect_length(paths, 2L)
  expect_true(all(file.exists(paths)))
  expect_true(all(grepl("\\.md$", paths)))
  expect_true(dir.exists(file.path(d, "memos")))
})

test_that("persist_memos returns empty when log has zero memos", {
  log <- create_reflection_log()
  d <- withr::local_tempdir()
  paths <- persist_memos(log, d)
  expect_length(paths, 0L)
})

test_that("persist_memos is idempotent (re-call produces byte-equivalent output)", {
  log <- create_reflection_log()
  log <- add_memo(log, body = "first", id = "memo_FIXED_ID")
  d <- withr::local_tempdir()
  paths1 <- persist_memos(log, d)
  bytes1 <- readBin(paths1[1], what = "raw", n = 1e6)
  paths2 <- persist_memos(log, d)
  bytes2 <- readBin(paths2[1], what = "raw", n = 1e6)
  expect_equal(bytes1, bytes2)
})

test_that("load_memos round-trips persisted memos", {
  log <- create_reflection_log()
  log <- add_memo(log, body = "first body", type = "coding")
  log <- add_memo(log, body = "second body", type = "operational")
  d <- withr::local_tempdir()
  persist_memos(log, d)
  loaded <- load_memos(d)
  expect_length(loaded, 2L)
  bodies <- vapply(loaded, function(m) m$body, character(1))
  expect_setequal(bodies, c("first body", "second body"))
  for (m in loaded) expect_s3_class(m, "Memo")
})

test_that("load_memos returns empty list when memos dir absent", {
  d <- withr::local_tempdir()
  loaded <- load_memos(d)
  expect_equal(loaded, list())
})

test_that("load_memos returns empty list when memos dir is empty", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "memos"))
  expect_equal(load_memos(d), list())
})

test_that("load_memos sorts by timestamp ascending", {
  log <- create_reflection_log()
  log <- add_memo(log, body = "later", id = "memo_z",
                    timestamp = "2026-05-03T15:00:00-0400")
  log <- add_memo(log, body = "earlier", id = "memo_a",
                    timestamp = "2026-05-03T10:00:00-0400")
  d <- withr::local_tempdir()
  persist_memos(log, d)
  loaded <- load_memos(d)
  expect_equal(loaded[[1]]$body, "earlier")
  expect_equal(loaded[[2]]$body, "later")
})

test_that("load_memos skips malformed .md files but logs a warning", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "memos"))
  # Write one good memo + one malformed
  log <- create_reflection_log()
  log <- add_memo(log, body = "good", id = "memo_good")
  persist_memos(log, d)
  writeLines("not a memo, no frontmatter",
              file.path(d, "memos", "memo_bad.md"))
  loaded <- suppressWarnings(load_memos(d))
  expect_length(loaded, 1L)
  expect_equal(loaded[[1]]$id, "memo_good")
})

# ---- Audit log integration ----------------------------------------------

test_that("add_memo writes a memo_added decision to the audit log when supplied", {
  d <- withr::local_tempdir()
  audit <- init_audit_log(d, config = list(methodology = list(
    mode = "reflexive_scaffold")))
  on.exit(close_audit_log(audit), add = TRUE)
  log <- create_reflection_log()
  log <- add_memo(log, body = "test memo", type = "coding",
                    audit_log = audit)
  close_audit_log(audit)
  audit_lines <- readLines(file.path(d, "ai_decisions.jsonl"))
  expect_true(any(grepl("memo_added", audit_lines)))
})

# ---- ResearcherReflectionLog schema 1.2.0 -------------------------------

test_that("ResearcherReflectionLog schema is 1.2.0", {
  log <- create_reflection_log()
  expect_equal(log$schema_version, "1.2.0")
})

test_that("print.ResearcherReflectionLog surfaces typed-memo by-type breakdown", {
  log <- create_reflection_log()
  log <- add_memo(log, body = "op1", type = "operational")
  log <- add_memo(log, body = "op2", type = "operational")
  log <- add_memo(log, body = "th1", type = "theoretical")
  out <- capture.output(print(log))
  expect_true(any(grepl("operational: 2", out)))
  expect_true(any(grepl("theoretical: 1", out)))
})

# ---- HTML escaping in the report's memo block --------------------------

test_that(".render_memo_block escapes HTML-active characters in body, links, author", {
  m <- make_memo(
    "Researcher note: <script>alert('xss')</script>\n& other thoughts: \"quoted\"",
    type = "theoretical",
    author = "<evil>",
    linked_themes = c("T<bad>"),
    linked_codes = c("c&1"),
    linked_entries = c("e<1>"),
    linked_prior_memo = "memo_<x>"
  )
  html <- pakhom:::.render_memo_block(m)
  # Raw script tag must NOT appear
  expect_no_match(html, "<script>alert\\('xss'\\)</script>")
  # Escaped form should be present
  expect_match(html, "&lt;script&gt;")
  # Escaped author + theme + code + entry + prior
  expect_match(html, "&lt;evil&gt;")
  expect_match(html, "T&lt;bad&gt;")
  expect_match(html, "c&amp;1")
  expect_match(html, "e&lt;1&gt;")
  expect_match(html, "memo_&lt;x&gt;")
})

test_that(".build_mode1_memo_section renders empty-state notice when no memos", {
  log <- create_reflection_log()
  html <- pakhom:::.build_mode1_memo_section(log)
  expect_match(html, "Researcher Reflexive Memos \\(M1\\.3 / AC6\\)")
  expect_match(html, "No memos were authored")
})

test_that(".build_mode1_memo_section renders by-type breakdown + chronological timeline", {
  log <- create_reflection_log()
  log <- add_memo(log, body = "later", type = "theoretical",
                    timestamp = "2026-05-03T15:00:00-0400")
  log <- add_memo(log, body = "earlier", type = "operational",
                    timestamp = "2026-05-03T10:00:00-0400")
  html <- pakhom:::.build_mode1_memo_section(log)
  # Both types in by-type rollup
  expect_match(html, "operational")
  expect_match(html, "theoretical")
  # Earlier appears before later (chronological order)
  pos_earlier <- regexpr("earlier", html)
  pos_later   <- regexpr("later",   html)
  expect_true(pos_earlier > 0 && pos_later > pos_earlier)
})

test_that(".build_mode1_memo_section ignores non-Memo entries in log$memos (back-compat)", {
  # A schema 1.0.0 / 1.1.0 log may have raw entries in the memos slot
  # that are not Memo S3 objects. The renderer must not crash and
  # must treat the run as having zero memos.
  log <- create_reflection_log()
  log$memos <- list(list(text = "old shape, no class"))
  html <- pakhom:::.build_mode1_memo_section(log)
  expect_match(html, "No memos were authored")
})
