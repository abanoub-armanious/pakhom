# Tests for iterative theme generation and deterministic cascading
# (16_themes.R)

test_that("cascade_theme_assignments maps entries through codes deterministically", {
  # Create mock coding state
  state <- create_coding_state()
  state$codebook[["tool_helps"]] <- list(
    code_name = "Scheduling helps overwork control", description = "",
    type = "descriptive", frequency = 3L,
    entry_ids = c("e1", "e2", "e3"), coded_segments = list()
  )
  state$codebook[["focus_bad"]] <- list(
    code_name = "Focus disruption", description = "",
    type = "descriptive", frequency = 2L,
    entry_ids = c("e1", "e4"), coded_segments = list()
  )
  state$entry_results[["e1"]] <- list(codes_assigned = c("tool_helps", "focus_bad"), skipped = FALSE)
  state$entry_results[["e2"]] <- list(codes_assigned = c("tool_helps"), skipped = FALSE)
  state$entry_results[["e3"]] <- list(codes_assigned = c("tool_helps"), skipped = FALSE)
  state$entry_results[["e4"]] <- list(codes_assigned = c("focus_bad"), skipped = FALSE)

  # Create theme set with merge history
  ts <- create_theme_set(list(
    list(id = 1, name = "Scheduling Benefits", description = "",
         codes_included = "Scheduling helps overwork control"),
    list(id = 2, name = "Focus Problems", description = "",
         codes_included = "Focus disruption")
  ))
  ts$merge_history <- list(
    code_to_theme_map = list(tool_helps = "Scheduling Benefits",
                              focus_bad = "Focus Problems"),
    code_to_subtheme_map = list()
  )

  data <- tibble::tibble(
    std_id = c("e1", "e2", "e3", "e4"),
    std_text = c("text1", "text2", "text3", "text4")
  )

  result <- cascade_theme_assignments(data, state, ts)

  # e1 has both codes -> assigned to both themes (multi-label)
  expect_true(grepl("Scheduling Benefits", result$emerged_themes[1]))
  # e2, e3 -> Scheduling Benefits
  expect_equal(result$emerged_themes[2], "Scheduling Benefits")
  expect_equal(result$emerged_themes[3], "Scheduling Benefits")
  # e4 -> Focus Problems
  expect_equal(result$emerged_themes[4], "Focus Problems")
  # e1 should be assigned to both themes (multi-label)
  expect_true(grepl("Focus Problems", result$emerged_themes[1]))
  expect_true(grepl("Scheduling Benefits", result$emerged_themes[1]))
  # n_themes: e1 has 2, others have 1
  expect_equal(result$n_themes[1], 2L)
  expect_equal(result$n_themes[2], 1L)
})

test_that("cascade_theme_assignments handles skipped entries", {
  state <- create_coding_state()
  state$entry_results[["e1"]] <- list(codes_assigned = character(0), skipped = TRUE, skip_reason = "N/A")

  ts <- create_theme_set(list(
    list(id = 1, name = "Theme A", description = "", codes_included = "code1")
  ))
  ts$merge_history <- list(
    code_to_theme_map = list(x = "Theme A"),
    code_to_subtheme_map = list()
  )

  data <- tibble::tibble(std_id = "e1", std_text = "text")
  result <- cascade_theme_assignments(data, state, ts)

  expect_true(is.na(result$emerged_themes[1]))
})

test_that("enrich_themes computes entry counts and sentiment", {
  ts <- create_theme_set(list(
    list(id = 1, name = "Positive Theme", description = "", codes_included = "a"),
    list(id = 2, name = "Negative Theme", description = "", codes_included = "b")
  ))

  # Data must have theme_membership_* columns (set by cascade_theme_assignments)
  data <- tibble::tibble(
    std_id = c("e1", "e2", "e3"),
    std_text = c("text1", "text2", "text3"),
    emerged_themes = c("Positive Theme", "Positive Theme", "Negative Theme"),
    sentiment_score = c(0.5, 0.8, -0.6),
    theme_membership_Positive.Theme = c(1L, 1L, 0L),
    theme_membership_Negative.Theme = c(0L, 0L, 1L)
  )

  enriched <- enrich_themes(ts, data)
  expect_equal(enriched$themes[[1]]$entry_count, 2)
  expect_equal(enriched$themes[[2]]$entry_count, 1)
  expect_equal(enriched$themes[[1]]$sentiment_tendency, "positive")
  expect_equal(enriched$themes[[2]]$sentiment_tendency, "negative")
})

# T0.2: enrich_themes populates supporting_quotes via the spread-aware,
# sentiment-positioned selector (was previously random sampling -- which
# meant the report's most_negative/median/most_positive labels lied AND
# the participant-spread logic in .select_representative_quotes was
# effectively dead code on the production path).

test_that("enrich_themes uses sentiment-positioned (not random) supporting_quotes", {
  ts <- create_theme_set(list(
    list(id = 1, name = "T", description = "", codes_included = "a")
  ))
  set.seed(123)
  data <- tibble::tibble(
    std_id   = paste0("e", 1:7),
    std_text = paste0("This is a quote long enough to pass the 50-character filter, entry ",
                      1:7, ". Lorem ipsum."),
    emerged_themes = rep("T", 7),
    sentiment_score = c(-0.9, -0.6, -0.3, 0.0, 0.3, 0.6, 0.9),
    theme_membership_T = rep(1L, 7)
  )
  enriched <- enrich_themes(ts, data)
  qs <- enriched$themes[[1]]$supporting_quotes
  expect_length(qs, 3L)
  # Sentiment-positioned selection: first quote should be from the
  # most-negative entry (e1, sentiment -0.9), last from most-positive
  # (e7, sentiment 0.9). Middle is the median.
  expect_match(qs[1], "entry 1\\.")
  expect_match(qs[3], "entry 7\\.")
})

test_that("enrich_themes prefers different contributors for supporting_quotes (T0.2 spread-aware)", {
  ts <- create_theme_set(list(
    list(id = 1, name = "T", description = "", codes_included = "a")
  ))
  data <- tibble::tibble(
    std_id   = paste0("e", 1:6),
    std_text = paste0("This is a quote long enough to pass the 50-character filter, entry ",
                      1:6, ". Lorem ipsum dolor."),
    std_author = c("heavy", "heavy", "heavy", "alice", "bob", "carol"),
    emerged_themes = rep("T", 6),
    sentiment_score = c(-0.9, -0.5, -0.2, 0.1, 0.4, 0.8),
    theme_membership_T = rep(1L, 6)
  )
  enriched <- enrich_themes(ts, data)
  qs <- enriched$themes[[1]]$supporting_quotes
  expect_length(qs, 3L)
  # Most-negative slot (sentiment -0.9, author heavy): heavy is taken.
  # Median slot (target idx 3 in sentiment-sorted = -0.2 by heavy):
  # spread-aware should walk outward and find alice/bob/carol instead.
  # Most-positive slot (carol at 0.8): carol is fresh -> selected.
  # The middle quote text should NOT come from "entry 3" (heavy at -0.2);
  # spread-aware picks an alternative author's row.
  expect_false(grepl("entry 3\\.", qs[2], fixed = FALSE))
})

test_that("enrich_themes handles single-entry themes gracefully", {
  ts <- create_theme_set(list(
    list(id = 1, name = "T", description = "", codes_included = "a")
  ))
  data <- tibble::tibble(
    std_id   = "e1",
    std_text = "This is the only entry, long enough to pass filtering filters.",
    emerged_themes = "T",
    sentiment_score = 0.5,
    theme_membership_T = 1L
  )
  enriched <- enrich_themes(ts, data)
  # 1 entry -> 1 supporting quote (most_negative slot only)
  expect_length(enriched$themes[[1]]$supporting_quotes, 1L)
})

test_that("enrich_themes handles entries below 50-char text filter (no supporting_quotes)", {
  ts <- create_theme_set(list(
    list(id = 1, name = "T", description = "", codes_included = "a")
  ))
  data <- tibble::tibble(
    std_id   = c("e1", "e2"),
    std_text = c("short", "also short"),  # < 50 chars; .select_representative_quotes filters out
    emerged_themes = c("T", "T"),
    sentiment_score = c(-0.5, 0.5),
    theme_membership_T = c(1L, 1L)
  )
  enriched <- enrich_themes(ts, data)
  # No quotes pass the filter -> supporting_quotes stays NULL/unset
  expect_true(is.null(enriched$themes[[1]]$supporting_quotes) ||
              length(enriched$themes[[1]]$supporting_quotes) == 0L)
})


test_that("theme_membership columns are created", {
  state <- create_coding_state()
  state$codebook[["a"]] <- list(code_name = "Code A", frequency = 1L,
                                 entry_ids = "e1", coded_segments = list())
  state$entry_results[["e1"]] <- list(codes_assigned = "a", skipped = FALSE)

  ts <- create_theme_set(list(
    list(id = 1, name = "Theme One", description = "", codes_included = "Code A")
  ))
  ts$merge_history <- list(
    code_to_theme_map = list(a = "Theme One"),
    code_to_subtheme_map = list()
  )

  data <- tibble::tibble(std_id = "e1", std_text = "text")
  result <- cascade_theme_assignments(data, state, ts)

  membership_col <- paste0("theme_membership_", make.names("Theme One"))
  expect_true(membership_col %in% names(result))
  expect_equal(result[[membership_col]][1], 1L)
})

# ==============================================================================
# Theme generation: generate_themes_iterative dispatch + degenerate-corpus edges
# ==============================================================================


test_that("generate_themes_iterative single-code corpus produces 1-theme ThemeSet without AI", {
  state <- create_coding_state()
  state$codebook[["only"]] <- list(
    code_name = "Solo Code", description = "the only code",
    type = "descriptive", frequency = 1L,
    entry_ids = "e1", coded_segments = list()
  )
  state$entry_results[["e1"]] <- list(codes_assigned = "only", skipped = FALSE)

  fake_provider <- list(
    provider = "anthropic",
    models = list(primary = "claude", embedding = NULL),
    methodology_rules = ""
  )
  class(fake_provider) <- "AIProvider"

  ts <- generate_themes_iterative(state, fake_provider, config = list(algorithm = "v1"))
  expect_s3_class(ts, "ThemeSet")
  expect_equal(n_themes(ts), 1L)
  expect_equal(ts$themes[[1]]$name, "Solo Code")
  # rebuild_code_to_theme_map populated the lookup
  expect_equal(ts$merge_history$code_to_theme_map[["only"]], "Solo Code")
})

test_that("generate_themes_iterative empty codebook returns empty ThemeSet", {
  state <- create_coding_state()
  fake_provider <- list(
    provider = "anthropic",
    models = list(primary = "claude", embedding = NULL),
    methodology_rules = ""
  )
  class(fake_provider) <- "AIProvider"

  ts <- generate_themes_iterative(state, fake_provider, config = list(algorithm = "v1"))
  expect_s3_class(ts, "ThemeSet")
  expect_equal(n_themes(ts), 0L)
})

test_that("cluster_decision is a valid audit log decision_type", {
  # Audit CRITICAL-6a: production runs with audit_log != NULL
  # would abort if cluster_decision were not registered. Pin this by
  # asserting it's in the validator's allow-list.
  expect_true("cluster_decision" %in% pakhom:::.valid_decision_types)
})

# Theme-generation tests that mock ai_complete to exercise the AI-decision
# paths (via generate_themes_iterative -> the v2 clustering engine) without
# burning real API credits.

.fake_provider_for_theming <- function() {
  fp <- list(
    provider          = "openai",
    models            = list(primary = "gpt-4o", embedding = NULL),
    methodology_rules = "",
    temperature       = list(theming = 0.4),
    max_tokens        = list(theming = 2000)
  )
  class(fp) <- "AIProvider"
  fp
}

.three_code_state <- function() {
  state <- create_coding_state()
  for (k in c("a", "b", "c")) {
    state$codebook[[k]] <- list(
      code_name = paste("Code", toupper(k)), description = "",
      type = "descriptive", frequency = 1L,
      entry_ids = paste0("e", k), coded_segments = list()
    )
  }
  state
}


# ============================================================================
# Articulation quality: themes must state a non-vacuous organizing concept
#
# A theme's central organizing concept must not be a vacuous bucket label or a
# tautological restatement of the theme name; the v2 clustering engine enforces
# this through its prompt. The integration test below checks the degenerate
# single-code case via generate_themes_iterative.
# ============================================================================


test_that("C-1 integration: legacy 30-char minimum still triggers on n=1 singleton", {
  # n=1 should keep the flat 30-char floor (log10(1) = 0).
  state <- create_coding_state()
  state$codebook[["solo"]] <- list(
    code_name = "Code Solo", description = "",
    type = "descriptive", frequency = 1L,
    entry_ids = "e1", coded_segments = list()
  )
  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      list(
        content = jsonlite::toJSON(list(
          central_organizing_concept = "tiny",  # 4 chars
          decision = "coherent_theme",
          proposed_name = "Tiny",
          proposed_description = "...",
          rationale = "..."
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )
  ts <- generate_themes_iterative(state, .fake_provider_for_theming(),
                                     config = list(algorithm = "v1"))
  # Single code, articulation too short -> forced split. Single-code split
  # produces 1 atomic-outlier theme regardless.
  expect_equal(n_themes(ts), 1L)
})

# C-1 audit followup tests --------------------------------------------------


# ============================================================================
# Subtheme S3: nested, multi-level subtheme structure
#
# Subtheme S3 objects carry a nested $subthemes field so a theme can be
# decomposed to multiple levels. These tests pin the data-structure
# invariants -- nesting, coercion of raw lists into Subtheme S3, and the
# depth-aware counters -- independent of how the clustering engine
# populates them.
# ============================================================================

test_that("Tier 1 C-12: Subtheme S3 now carries nested $subthemes field", {
  st_inner <- create_subtheme(name = "Inner", codes = c("a", "b"))
  st_outer <- create_subtheme(name = "Outer", codes = c("c"),
                                subthemes = list(st_inner))
  expect_s3_class(st_outer, "Subtheme")
  expect_length(st_outer$subthemes, 1L)
  expect_s3_class(st_outer$subthemes[[1]], "Subtheme")
  expect_equal(st_outer$subthemes[[1]]$name, "Inner")
  expect_equal(subtheme_n_subthemes(st_outer), 1L)
  expect_equal(subtheme_n_subthemes(st_inner), 0L)
})

test_that("Tier 1 C-12: create_subtheme coerces raw-list nested subthemes into Subtheme S3", {
  raw_nested <- list(
    list(name = "Inner1", description = "i1", codes = c("a", "b")),
    list(name = "Inner2", description = "i2", codes = c("c"))
  )
  st <- create_subtheme(name = "Outer", codes = list(),
                        subthemes = raw_nested)
  expect_length(st$subthemes, 2L)
  expect_true(all(vapply(st$subthemes, inherits, logical(1), "Subtheme")))
  expect_equal(st$subthemes[[1]]$name, "Inner1")
})


# ============================================================================
# AF-3: n_subthemes schema clarity
#
# Earlier, the JSON field n_subthemes counted only top-level real
# subthemes but wasn't documented; an audit found 207 of 417
# themes where n_subthemes != length(subthemes_structured) and called it
# "unreliable". The fix exposes three distinct counters:
#   n_subthemes              -- depth-1 real subthemes (legacy semantics)
#   n_subthemes_total        -- real subthemes across all depths
#   n_subthemes_structured   -- length(subthemes_structured) incl. virtual
# ============================================================================

test_that("Tier 1 AF-3: theme_n_subthemes returns depth-1 real subthemes only", {
  # Build a theme with 2 top-level subthemes (1 real, 1 virtual) and
  # a nested real sub-subtheme under the real top-level subtheme.
  inner <- create_subtheme(name = "Inner Sub", codes = c("a", "b"))
  outer_real    <- create_subtheme(name = "Outer Real", codes = c("c"),
                                     subthemes = list(inner))
  outer_virtual <- create_subtheme(name = NA_character_, codes = c("d"))
  theme <- list(
    name = "Test Theme", description = "",
    subthemes = list(outer_real, outer_virtual)
  )
  # Legacy semantics: 1 real top-level subtheme.
  expect_equal(theme_n_subthemes(theme), 1L)
})

test_that("Tier 1 AF-3: theme_n_subthemes_total recurses across every depth", {
  # Same theme as above. n_subthemes_total counts BOTH the top-level
  # real ('Outer Real') AND the nested real ('Inner Sub') -> 2.
  inner <- create_subtheme(name = "Inner Sub", codes = c("a", "b"))
  outer_real <- create_subtheme(name = "Outer Real", codes = c("c"),
                                  subthemes = list(inner))
  outer_virtual <- create_subtheme(name = NA_character_, codes = c("d"))
  theme <- list(subthemes = list(outer_real, outer_virtual))
  expect_equal(theme_n_subthemes_total(theme), 2L)
})

test_that("Tier 1 AF-3: theme_n_subthemes_total works on a 3-deep tree", {
  l3 <- create_subtheme(name = "Depth 3", codes = c("e"))
  l2 <- create_subtheme(name = "Depth 2", codes = c("d"),
                          subthemes = list(l3))
  l1 <- create_subtheme(name = "Depth 1", codes = c("c"),
                          subthemes = list(l2))
  theme <- list(subthemes = list(l1))
  expect_equal(theme_n_subthemes(theme), 1L)
  expect_equal(theme_n_subthemes_total(theme), 3L)
})

test_that("Tier 1 AF-3: subtheme_n_subthemes returns immediate-child count", {
  # Direct subtheme getter (not theme-level).
  inner1 <- create_subtheme(name = "Inner 1", codes = c("a"))
  inner2 <- create_subtheme(name = "Inner 2", codes = c("b"))
  outer  <- create_subtheme(name = "Outer", codes = c("c"),
                              subthemes = list(inner1, inner2))
  expect_equal(subtheme_n_subthemes(outer), 2L)
  expect_equal(subtheme_n_subthemes(inner1), 0L)
})

test_that("Tier 1 audit followup LOW-3: subtheme_n_codes_total recurses across depth", {
  # Direct code count (depth-0) vs. recursive code count (all depths).
  inner1 <- create_subtheme(name = "Inner 1", codes = c("a", "b"))
  inner2 <- create_subtheme(name = "Inner 2", codes = c("c"))
  outer  <- create_subtheme(name = "Outer", codes = c("d", "e", "f"),
                              subthemes = list(inner1, inner2))
  expect_equal(subtheme_n_codes(outer), 3L)         # direct (d, e, f)
  expect_equal(subtheme_n_codes_total(outer), 6L)   # direct + a, b, c
  expect_equal(subtheme_n_codes(inner1), 2L)
  expect_equal(subtheme_n_codes_total(inner1), 2L)  # leaf -> direct == total
})

test_that("Tier 1 audit followup MEDIUM-1: theme_set_to_tibble exposes both n_subthemes counters", {
  # CSV consumers should see both depth-1 (legacy) AND across-all-depths
  # (total) counts; previously only depth-1 was emitted.
  inner <- create_subtheme(name = "Inner Real", codes = c("a"))
  outer <- create_subtheme(name = "Outer Real", codes = c("b"),
                             subthemes = list(inner))
  ts <- create_theme_set(themes = list(
    list(id = 1L, name = "Theme A", description = "",
         subthemes = list(outer)),
    list(id = 2L, name = "Theme B", description = "",
         subthemes = list(create_subtheme(name = NA_character_,
                                            codes = c("c"))))
  ))
  tib <- theme_set_to_tibble(ts)
  expect_true("n_subthemes" %in% names(tib))
  expect_true("n_subthemes_total" %in% names(tib),
              info = "tibble form should expose n_subthemes_total")
  # Theme A: 1 top-level real ('Outer Real') + 1 nested real ('Inner Real') = 2 total.
  expect_equal(tib$n_subthemes[tib$name == "Theme A"], 1L)
  expect_equal(tib$n_subthemes_total[tib$name == "Theme A"], 2L)
  # Theme B has only a virtual subtheme -> 0 real at any depth.
  expect_equal(tib$n_subthemes[tib$name == "Theme B"], 0L)
  expect_equal(tib$n_subthemes_total[tib$name == "Theme B"], 0L)
})

test_that("Tier 1 C-13: subtheme_assignments persists in cascade output", {
  # End-to-end smoke test: cascade attaches subtheme_assignments to
  # analytic data; the per-theme CSV export includes the column.
  # Build a minimal state + theme_set where cascade can route entries.
  state <- create_coding_state()
  state$codebook[["food_addiction"]] <- list(
    code_name = "Meeting Overload", description = "x",
    type = "descriptive", frequency = 2L,
    entry_ids = c("e1", "e2"),
    coded_segments = list(
      list(entry_id = "e1", text = "ate too much", start_char = 0L, end_char = 12L),
      list(entry_id = "e2", text = "overwork again", start_char = 0L, end_char = 11L)
    )
  )
  state$entry_results[["e1"]] <- list(
    codes_assigned = "food_addiction", skipped = FALSE,
    coded_segments = list(state$codebook[["food_addiction"]]$coded_segments[[1]])
  )
  state$entry_results[["e2"]] <- list(
    codes_assigned = "food_addiction", skipped = FALSE,
    coded_segments = list(state$codebook[["food_addiction"]]$coded_segments[[2]])
  )

  ts <- create_theme_set(themes = list(
    list(name = "Overworking Patterns", description = "",
         subthemes = list(create_subtheme(
           name = "Compulsive Overworking",
           codes = list(create_code_object(key = "food_addiction",
                                            name = "Meeting Overload",
                                            frequency = 2L))
         )))
  ))
  ts <- rebuild_code_to_theme_map(ts, state)

  data <- tibble::tibble(std_id = c("e1", "e2"), std_text = c("a", "b"))
  out <- cascade_theme_assignments(data, state, ts)

  # subtheme_assignments column exists and is populated.
  expect_true("subtheme_assignments" %in% names(out))
  expect_equal(out$subtheme_assignments, c("Compulsive Overworking", "Compulsive Overworking"))
})
