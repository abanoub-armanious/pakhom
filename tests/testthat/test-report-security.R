# Regression tests for R/17_report.R security:
#   FIX 1 -- NA-sentiment quote must not crash the report-string BUILD phase.
#   FIX 2 -- AI free-text (<script> etc.) must be HTML-neutralized before it is
#            interpolated into the report, while intended Markdown is preserved.
# (FIX 3, CDN SRI/crossorigin, is a static-string change asserted at the bottom.)

# -----------------------------------------------------------------------------
# FIX 1: NA sentiment in a representative quote
# -----------------------------------------------------------------------------
# This site (.build_thematic_section's "Representative Voices" loop) runs while
# BUILDING the Rmd string, NOT inside a knitr chunk -- so an error here aborts
# the whole report build; error=TRUE only rescues failures inside rendered
# chunks. Before the fix, `q$sentiment < THRESHOLD` on an NA threw
# "missing value where TRUE/FALSE needed" and killed generate_report().

test_that("FIX1: an NA-sentiment quote does not crash the thematic-section build", {
  ts_one <- list(
    description = "Theme with an NA-sentiment quote",
    n_entries = 12,
    pct_of_total = 24,
    sentiment = list(mean = 0.1, pct_negative = 25, pct_positive = 30),
    intensity = list(mean = 0.5),
    keywords = c("k1"),
    quotes_with_context = list(
      representative = list(
        text = "A quote whose sentiment score is missing.",
        sentiment = NA_real_,            # <- the crash trigger
        emotion = "mixed"
      )
    ),
    subtheme_stats = list(),
    metric_cols = character(0),
    theme_kind = "framework"
  )
  theme_stats <- list("Theme 1" = ts_one)

  expect_no_error(
    out <- pakhom:::.build_thematic_section(
      theme_stats  = theme_stats,
      theme_order  = names(theme_stats),
      n_themes     = length(theme_stats),
      export_files = list(theme_csv_files = list()),
      config       = list(analysis = list(themes = list(max_inline_themes = 100L)))
    )
  )
  # The quote still renders; the NA degrades to the neutral / "N/A" presentation
  # exactly like the already-guarded detail-page path.
  expect_true(grepl("sentiment score is missing", out, fixed = TRUE))
  expect_true(grepl("Sentiment: N/A", out, fixed = TRUE))
  expect_true(grepl('class="quote-box neutral"', out, fixed = TRUE))
})

test_that("FIX1: a NULL sentiment field is also tolerated", {
  ts_one <- list(
    description = "Theme with a NULL-sentiment quote",
    n_entries = 5, pct_of_total = 10,
    sentiment = list(mean = 0.0, pct_negative = 10, pct_positive = 10),
    intensity = list(mean = 0.2),
    keywords = character(0),
    quotes_with_context = list(
      representative = list(text = "No sentiment key at all.", emotion = "N/A")
    ),
    subtheme_stats = list(), metric_cols = character(0), theme_kind = "framework"
  )
  expect_no_error(
    out <- pakhom:::.build_thematic_section(
      theme_stats  = list("T" = ts_one),
      theme_order  = "T",
      n_themes     = 1L,
      export_files = list(theme_csv_files = list()),
      config       = list(analysis = list(themes = list(max_inline_themes = 100L)))
    )
  )
  expect_true(grepl("No sentiment key at all", out, fixed = TRUE))
})

# -----------------------------------------------------------------------------
# FIX 2: AI-prose sanitizer
# -----------------------------------------------------------------------------
test_that("FIX2: .sanitize_ai_prose neutralizes HTML tags", {
  expect_equal(
    pakhom:::.sanitize_ai_prose("<script>alert(1)</script>"),
    "&lt;script&gt;alert(1)&lt;/script&gt;"
  )
  expect_equal(
    pakhom:::.sanitize_ai_prose('<img src=x onerror="alert(1)">'),
    '&lt;img src=x onerror="alert(1)"&gt;'
  )
  # A bare ampersand is amped; existing entities and our own &bull;/&mdash;
  # are left intact (no double-escaping).
  expect_equal(pakhom:::.sanitize_ai_prose("Tom & Jerry"), "Tom &amp; Jerry")
  expect_equal(pakhom:::.sanitize_ai_prose("a &amp; b &bull; c"), "a &amp; b &bull; c")
  expect_equal(pakhom:::.sanitize_ai_prose("&#39; &#x2014;"), "&#39; &#x2014;")
})

test_that("FIX2: .sanitize_ai_prose preserves intended Markdown", {
  # Bold, emphasis, links and list markers contain no <,>,or bare & so they
  # pass through verbatim -- the renderer still styles them.
  md <- "**bold** _em_ [link](http://x)\n- item one\n- item two"
  out <- pakhom:::.sanitize_ai_prose(md)
  expect_equal(out, md)
  expect_true(grepl("**bold**", out, fixed = TRUE))
  expect_true(grepl("[link](http://x)", out, fixed = TRUE))
  # A literal '>' inside the AI's OWN text is escaped on purpose (it is an
  # injection vector). The "> " blockquote markers the report prepends are
  # added AFTER sanitizing, so author-side blockquotes are unaffected.
  expect_equal(pakhom:::.sanitize_ai_prose("a > b"), "a &gt; b")
})

test_that("FIX2: .sanitize_ai_prose handles NULL / NA", {
  expect_equal(pakhom:::.sanitize_ai_prose(NULL), "")
  expect_equal(pakhom:::.sanitize_ai_prose(NA), "")
  expect_equal(pakhom:::.sanitize_ai_prose(NA_character_), "")
})

# ----------------------------------------------------------------------------
# FIX2b: dangerous markdown-link URL schemes are defanged.
# The report renders via pandoc, which turns [text](url) into <a href="url">
# WITHOUT vetting the scheme and DECODES HTML entities in the URL first -- so a
# prompt-injected [x](javascript:..) (or its entity-obfuscated variants) would
# otherwise become a clickable javascript: href. <,>,& escaping alone does not
# stop this because the link is [](), not a <tag>.
# ----------------------------------------------------------------------------
test_that("FIX2b: javascript:/data:/vbscript: links are stripped to plain text", {
  s <- pakhom:::.sanitize_ai_prose
  # Inline link with a dangerous scheme -> the [..](..) markup is removed, the
  # visible text is kept, so pandoc can never emit a script: href.
  expect_false(grepl("](", s("[click](javascript:alert(1))"), fixed = TRUE))
  expect_false(grepl("](", s("[x](data:text/html,stuff)"),     fixed = TRUE))
  expect_false(grepl("](", s("[x](vbscript:msgbox(1))"),       fixed = TRUE))
  # Images too (the src= would otherwise carry the payload).
  expect_false(grepl("](", s("![pic](javascript:alert(1))"),   fixed = TRUE))
})

test_that("FIX2b: entity-obfuscated dangerous schemes are also defanged", {
  s <- pakhom:::.sanitize_ai_prose
  # pandoc decodes &colon; -> : ; &#106; -> j ; hex; and one &amp; layer.
  for (m in c("[a](javascript&colon;alert(1))",
              "[b](&#106;avascript:alert(1))",
              "[c](javascript&#x3a;alert(1))",
              "[d](&#x6a;avascript:alert(1))")) {
    expect_false(grepl("](", s(m), fixed = TRUE), info = m)
  }
})

test_that("FIX2b: a control char embedded in a link scheme cannot smuggle a live javascript: href", {
  # Adversarial-audit finding: pandoc strips a carriage return (and other C0
  # control chars) from a link destination, while .MD_LINK_RE stops the URL
  # capture at the same whitespace -- so "[x](java<CR>script:...)" slipped past
  # the scheme allowlist yet rendered as a live javascript: href. The sanitizer
  # removes the control chars up front. Empirically CR was the exploitable
  # vector (pandoc percent-encodes tab/newline), but the whole C0-control class
  # is stripped for defence in depth.
  s <- pakhom:::.sanitize_ai_prose
  for (ctrl in c("\r", "\v", "\f", "\a", "\b")) {
    out <- s(paste0("[x](java", ctrl, "script:alertX)"))
    expect_false(grepl("](", out, fixed = TRUE))          # link defanged to text
    expect_false(grepl("javascript", out, fixed = TRUE))  # scheme gone
  }
  # a leading control char before the scheme is likewise neutralized
  expect_false(grepl("javascript", s("[x](\rjavascript:alertX)"), fixed = TRUE))
  # tab and newline (legitimate Markdown structure) are preserved
  expect_equal(s("line1\nline2"), "line1\nline2")
  expect_equal(s("a\tb"), "a\tb")
})

test_that("FIX2b: a dangerous reference-style definition is neutralized to #", {
  out <- pakhom:::.sanitize_ai_prose("see [it][1]\n\n[1]: javascript:alert(1)")
  # The reference URL is rewritten so [it][1] resolves to '#', not javascript:.
  expect_false(grepl("javascript:", out, fixed = TRUE))
  expect_true(grepl("]: #", out, fixed = TRUE))
})

test_that("FIX2b: safe links and ordinary prose are left untouched", {
  s <- pakhom:::.sanitize_ai_prose
  # Allowlisted schemes + relative/anchor links survive verbatim.
  expect_equal(s("[ok](https://example.com)"), "[ok](https://example.com)")
  expect_equal(s("[m](mailto:a@b.com)"),       "[m](mailto:a@b.com)")
  expect_equal(s("[r](/local/path)"),          "[r](/local/path)")
  expect_equal(s("[a](#anchor)"),              "[a](#anchor)")
  # A safe URL with balanced parens must NOT be mangled.
  expect_equal(s("[w](http://en.wikipedia.org/wiki/R_(language))"),
                  "[w](http://en.wikipedia.org/wiki/R_(language))")
  # Prose with colons that are NOT links stays put (no false positives).
  expect_equal(s("The ratio was 3:1; see section 2: methods."),
                  "The ratio was 3:1; see section 2: methods.")
})

test_that("FIX2b: .url_scheme_unsafe allowlists correctly", {
  u <- pakhom:::.url_scheme_unsafe
  expect_true(u("javascript:alert(1)"))
  expect_true(u("data:text/html,x"))
  expect_true(u("vbscript:x"))
  expect_true(u("file:///etc/passwd"))
  expect_true(u("javascript&colon;x"))     # entity colon
  expect_true(u("&#106;avascript:x"))      # entity scheme letter
  expect_false(u("https://x.com"))
  expect_false(u("mailto:a@b.com"))
  expect_false(u("/relative/path"))        # no scheme
  expect_false(u("#anchor"))
  expect_false(u("?q=1"))
})

test_that("FIX2: executive summary <script> is neutralized in the report build", {
  data <- sample_data(8)
  stats <- aggregate_overall_statistics(
    data, mock_theme_set(), consolidated = NULL,
    learning_context = NULL, config = mock_config()
  )
  payload <- "All good <script>alert('xss')</script> and **bold** kept."
  ai_synthesis <- list(executive_summary = payload, conclusion = "Done.")
  ef <- list(sentiment_file = "sentiment_scores.csv",
             correlations_file = "correlations.csv", codes_file = "codes.csv")

  rmd <- pakhom:::.build_rmd_content(
    overall_stats = stats,
    theme_stats   = list(),
    theme_order   = character(0),
    ai_synthesis  = ai_synthesis,
    corr_interpretation = NULL,
    insights      = list(),
    export_files  = ef,
    config        = mock_config()
  )
  # The raw tag must NOT survive; the neutralized form must be present; the
  # Markdown emphasis must be untouched.
  expect_false(grepl("<script>", rmd, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", rmd, fixed = TRUE))
  expect_true(grepl("**bold** kept", rmd, fixed = TRUE))
})

test_that("FIX2: synthesis conclusion + implications + key findings are neutralized", {
  insights <- list(
    key_findings = list(
      list(insight = "Finding <script>x</script>",
           explanation = "Because <img onerror=1> reasons.")
    ),
    theoretical_implications = "Theory <iframe></iframe> implication.",
    practical_implications   = "Practice <svg/onload=1> implication."
  )
  ai_synthesis <- list(conclusion = "Wrap-up <script>steal()</script> here.")

  out <- pakhom:::.build_synthesis_section(insights, ai_synthesis = ai_synthesis)
  expect_false(grepl("<script>", out, fixed = TRUE))
  expect_false(grepl("<img onerror", out, fixed = TRUE))
  expect_false(grepl("<iframe>", out, fixed = TRUE))
  expect_false(grepl("<svg/onload", out, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;steal()&lt;/script&gt;", out, fixed = TRUE))
})

test_that("FIX2: saturation articulation/rationale are neutralized inside blockquotes", {
  coding_state <- list(
    codebook = as.list(paste0("code", 1:5)),
    saturation = list(
      reached = TRUE,
      reached_at_coded = 40,
      reached_at_entry = 50,
      total_entries_at_saturation = 60,
      curve = data.frame(
        entries_coded = c(10, 20, 30, 40),
        n_codes = c(2, 3, 4, 5),
        new_codes_in_window = c(2, 1, 1, 1)
      ),
      ai_articulation = "Observed <script>evil()</script> plateau.",
      ai_rationale    = "Justified by <img src=x onerror=1> stability."
    )
  )
  out <- pakhom:::.build_saturation_section(coding_state)
  # The security property: no live tag can open. The attribute word "onerror"
  # may remain as inert TEXT (it's harmless once '<' is escaped), so we assert
  # the tag delimiters are gone, not the substring.
  expect_false(grepl("<script>", out, fixed = TRUE))
  expect_false(grepl("<img", out, fixed = TRUE))
  expect_true(grepl("&lt;img src=x onerror=1&gt;", out, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;evil()&lt;/script&gt;", out, fixed = TRUE))
  # The "> " blockquote markers we prepend ourselves are still literal Markdown
  # (the sanitizer ran on the AI text BEFORE we added them).
  expect_true(grepl("\n> ", out, fixed = TRUE))
})

# -----------------------------------------------------------------------------
# FIX 3: self-contained report -- jQuery + DataTables are VENDORED (no CDN)
# -----------------------------------------------------------------------------
# The interactive entries table on each theme-detail page used to load jQuery +
# DataTables from public CDNs. Those libraries are now bundled in inst/rmd/ and
# copied into the report's theme_details/ dir, then referenced by local
# filename -- so a finished report renders its tables OFFLINE, forever, with no
# network dependency, CDN version-rot, or remote-payload (supply-chain) surface.
test_that("FIX3: theme-detail pages vendor jQuery/DataTables locally, not via a CDN", {
  td <- tempfile("themedetails")
  dir.create(td)
  ts_one <- list(
    description = "d", n_entries = 3, pct_of_total = 100,
    sentiment = list(mean = 0.0, pct_negative = 0, pct_positive = 0),
    intensity = list(mean = 0.1), keywords = character(0),
    quotes_with_context = NULL, subtheme_stats = list(),
    metric_cols = character(0), theme_kind = "framework"
  )
  pakhom:::.generate_theme_detail_htmls(
    theme_stats  = list("T" = ts_one),
    theme_order  = "T",
    export_files = list(theme_csv_files = list()),
    output_dir   = td
  )
  detail_dir <- file.path(td, "theme_details")

  # (a) The three vendored assets are copied in beside the detail pages.
  assets <- c("jquery-3.7.1.min.js",
              "jquery.dataTables.min.js",
              "jquery.dataTables.min.css")
  for (a in assets) {
    expect_true(file.exists(file.path(detail_dir, a)),
                info = paste("vendored asset missing:", a))
    expect_gt(file.info(file.path(detail_dir, a))$size, 0)
  }
  # The bundled bytes are the real libraries (cheap content sanity check that
  # catches a corrupt / wrong-version asset).
  expect_match(
    paste(readLines(file.path(detail_dir, "jquery-3.7.1.min.js"),
                    n = 2, warn = FALSE), collapse = " "),
    "jQuery v3.7.1", fixed = TRUE
  )
  expect_match(
    paste(readLines(file.path(detail_dir, "jquery.dataTables.min.js"),
                    n = 2, warn = FALSE), collapse = " "),
    "DataTables 1.13.8", fixed = TRUE
  )

  # (b) The detail HTML references them by LOCAL filename and contains NO CDN
  #     host -- nothing is fetched over the network when the report is opened.
  #     (Scope the glob to the theme_*.html so we don't pick up a vendored
  #     asset, which now also lives in theme_details/.)
  html <- paste(
    readLines(list.files(detail_dir, pattern = "^theme_.*\\.html$",
                         full.names = TRUE)[1]),
    collapse = "\n"
  )
  expect_true(grepl('src="jquery-3.7.1.min.js"', html, fixed = TRUE))
  expect_true(grepl('src="jquery.dataTables.min.js"', html, fixed = TRUE))
  expect_true(grepl('href="jquery.dataTables.min.css"', html, fixed = TRUE))
  expect_false(grepl("cdn.datatables.net", html, fixed = TRUE))
  expect_false(grepl("code.jquery.com", html, fixed = TRUE))
})
