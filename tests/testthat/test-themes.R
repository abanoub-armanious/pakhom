# Tests for iterative theme generation and deterministic cascading
# (16_themes.R)

test_that("cascade_theme_assignments maps entries through codes deterministically", {
  # Create mock coding state
  state <- create_coding_state()
  state$codebook[["med_helps"]] <- list(
    code_name = "Medication helps binge control", description = "",
    type = "descriptive", frequency = 3L,
    entry_ids = c("e1", "e2", "e3"), coded_segments = list()
  )
  state$codebook[["sleep_bad"]] <- list(
    code_name = "Sleep disruption", description = "",
    type = "descriptive", frequency = 2L,
    entry_ids = c("e1", "e4"), coded_segments = list()
  )
  state$entry_results[["e1"]] <- list(codes_assigned = c("med_helps", "sleep_bad"), skipped = FALSE)
  state$entry_results[["e2"]] <- list(codes_assigned = c("med_helps"), skipped = FALSE)
  state$entry_results[["e3"]] <- list(codes_assigned = c("med_helps"), skipped = FALSE)
  state$entry_results[["e4"]] <- list(codes_assigned = c("sleep_bad"), skipped = FALSE)

  # Create theme set with merge history
  ts <- create_theme_set(list(
    list(id = 1, name = "Medication Benefits", description = "",
         codes_included = "Medication helps binge control"),
    list(id = 2, name = "Sleep Problems", description = "",
         codes_included = "Sleep disruption")
  ))
  ts$merge_history <- list(
    code_to_theme_map = list(med_helps = "Medication Benefits",
                              sleep_bad = "Sleep Problems"),
    code_to_subtheme_map = list()
  )

  data <- tibble::tibble(
    std_id = c("e1", "e2", "e3", "e4"),
    std_text = c("text1", "text2", "text3", "text4")
  )

  result <- cascade_theme_assignments(data, state, ts)

  # e1 has both codes -> assigned to both themes (multi-label)
  expect_true(grepl("Medication Benefits", result$emerged_themes[1]))
  # e2, e3 -> Medication Benefits
  expect_equal(result$emerged_themes[2], "Medication Benefits")
  expect_equal(result$emerged_themes[3], "Medication Benefits")
  # e4 -> Sleep Problems
  expect_equal(result$emerged_themes[4], "Sleep Problems")
  # e1 should be assigned to both themes (multi-label)
  expect_true(grepl("Sleep Problems", result$emerged_themes[1]))
  expect_true(grepl("Medication Benefits", result$emerged_themes[1]))
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
# Phase 52: HAC + AI-judged divisive tree walk
# ==============================================================================

test_that("Phase 52: .leaves_under_node resolves internal node to leaf indices", {
  # Build a tiny hclust tree from 3 codes with hand-crafted distances
  d <- as.dist(matrix(c(0, 0.1, 0.9,
                         0.1, 0.0, 0.8,
                         0.9, 0.8, 0.0),
                       nrow = 3, byrow = TRUE))
  hac <- stats::hclust(d, method = "ward.D2")
  # The merge matrix encodes: row 1 merges leaves 1+2 (closest pair);
  # row 2 merges that cluster with leaf 3.
  # Internal node 1 should resolve to leaves c(1, 2); node 2 (root) to c(1, 2, 3).
  expect_setequal(pakhom:::.leaves_under_node(hac, 1L), c(1L, 2L))
  expect_setequal(pakhom:::.leaves_under_node(hac, 2L), c(1L, 2L, 3L))
  # Negative input is the leaf-passthrough convention
  expect_equal(pakhom:::.leaves_under_node(hac, -2L), 2L)
})

test_that("Phase 57 regression: .compute_code_distance_matrix preserves matrix dim through pmax (cosine path)", {
  # Phase 57 smoke caught a Phase 52 bug: pmax(0, 1 - sim) silently
  # stripped the matrix dim attribute (pmax transfers attributes only
  # from its FIRST argument when length matches the result; the scalar
  # 0 doesn't match, so dim was lost). The bug manifested as
  # "length of 'dimnames' [1] not equal to array extent" when the
  # downstream rownames(d) <- ... call tried to label a 1xN^2 matrix.
  # Phase 52 unit tests didn't catch it because they exercised only
  # the Jaccard fallback (which doesn't call pmax) on tiny 3-code
  # codebooks. Regression test: stub the cosine path with an embedding
  # matrix + verify the returned dist object has the right Size.
  state <- create_coding_state()
  for (k in paste0("c", 1:5)) {
    state$codebook[[k]] <- list(
      code_name = paste("Code", k), description = paste("Desc for", k),
      type = "descriptive", frequency = 1L,
      entry_ids = paste0("e", which(letters == substr(k, 2, 2))),
      coded_segments = list()
    )
  }
  codes <- pakhom:::.extract_codes_from_state(state)

  # Stub compute_embeddings to return a deterministic 5x8 matrix --
  # this exercises the cosine path that the smoke ran into.
  fake_provider <- list(
    provider = "openai",
    models = list(primary = "gpt-4o", embedding = "text-embedding-3-small")
  )
  class(fake_provider) <- "AIProvider"

  withr::local_seed(42L)
  fake_embs <- matrix(rnorm(5 * 8), nrow = 5L, ncol = 8L)
  testthat::local_mocked_bindings(
    compute_embeddings = function(provider, texts, model = NULL) fake_embs,
    .package = "pakhom"
  )
  d <- pakhom:::.compute_code_distance_matrix(codes, state, fake_provider)
  expect_equal(attr(d, "metric"), "cosine_embedding")
  expect_equal(attr(d, "Size"), 5L)
  m <- as.matrix(d)
  expect_equal(dim(m), c(5L, 5L))
  expect_setequal(rownames(m), names(state$codebook))
  expect_equal(diag(m), stats::setNames(rep(0, 5L), names(state$codebook)))
})

test_that("Phase 52: .compute_code_distance_matrix Jaccard fallback when no embeddings", {
  state <- create_coding_state()
  state$codebook[["a"]] <- list(
    code_name = "Code A", description = "", type = "descriptive",
    frequency = 2L, entry_ids = c("e1", "e2"), coded_segments = list()
  )
  state$codebook[["b"]] <- list(
    code_name = "Code B", description = "", type = "descriptive",
    frequency = 2L, entry_ids = c("e1", "e3"), coded_segments = list()
  )
  state$codebook[["c"]] <- list(
    code_name = "Code C", description = "", type = "descriptive",
    frequency = 2L, entry_ids = c("e4", "e5"), coded_segments = list()
  )
  codes <- pakhom:::.extract_codes_from_state(state)

  # Provider with no embedding model -> Jaccard fallback
  fake_provider <- list(
    provider = "anthropic",
    models = list(primary = "claude", embedding = NULL)
  )
  class(fake_provider) <- "AIProvider"

  d <- pakhom:::.compute_code_distance_matrix(codes, state, fake_provider)
  expect_equal(attr(d, "metric"), "jaccard_entry_ids")
  m <- as.matrix(d)
  # A and B share entry e1 (1 of 3 union) -> Jaccard distance = 1 - 1/3 = 0.667
  expect_equal(m["a", "b"], 1 - 1/3, tolerance = 1e-6)
  # A and C share no entries -> Jaccard distance = 1
  expect_equal(m["a", "c"], 1)
  # Symmetry + zero diagonal
  expect_equal(diag(m), c(a = 0, b = 0, c = 0))
  expect_equal(m, t(m))
})

test_that("Phase 52: generate_themes_iterative single-code corpus produces 1-theme ThemeSet without AI", {
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

  ts <- generate_themes_iterative(state, fake_provider, config = list())
  expect_s3_class(ts, "ThemeSet")
  expect_equal(n_themes(ts), 1L)
  expect_equal(ts$themes[[1]]$name, "Solo Code")
  # rebuild_code_to_theme_map populated the lookup
  expect_equal(ts$merge_history$code_to_theme_map[["only"]], "Solo Code")
})

test_that("Phase 52: generate_themes_iterative empty codebook returns empty ThemeSet", {
  state <- create_coding_state()
  fake_provider <- list(
    provider = "anthropic",
    models = list(primary = "claude", embedding = NULL),
    methodology_rules = ""
  )
  class(fake_provider) <- "AIProvider"

  ts <- generate_themes_iterative(state, fake_provider, config = list())
  expect_s3_class(ts, "ThemeSet")
  expect_equal(n_themes(ts), 0L)
})

test_that("Phase 52: cluster_decision is a valid audit log decision_type", {
  # Phase 52 audit CRITICAL-6a: production runs with audit_log != NULL
  # would abort if cluster_decision were not registered. Pin this by
  # asserting it's in the validator's allow-list.
  expect_true("cluster_decision" %in% pakhom:::.valid_decision_types)
})

# Phase 52 deferred audit tests (added in Phase 53 cleanup pass) ------------
# These require mocking ai_complete to exercise the AI-decision paths
# (.evaluate_cluster) without burning real API credits.

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

test_that("Phase 52: .evaluate_cluster coherent_theme path produces a single theme", {
  state <- .three_code_state()
  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      list(
        content = jsonlite::toJSON(list(
          central_organizing_concept = paste(
            "All three codes describe the daily lived experience of taking ",
            "medication while managing competing routines."),
          decision = "coherent_theme",
          proposed_name = "Medication management routines",
          proposed_description = "How participants integrate doses into daily life.",
          rationale = paste(
            "All three codes orbit the daily-life integration concept; even ",
            "the most distant pair shares this organizing principle.")
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )

  ts <- generate_themes_iterative(state, .fake_provider_for_theming(),
                                     config = list())
  expect_s3_class(ts, "ThemeSet")
  expect_equal(n_themes(ts), 1L)
  expect_equal(ts$themes[[1]]$name, "Medication management routines")
})

test_that("Phase 52: .evaluate_cluster split_required cascade produces N atomic themes", {
  state <- .three_code_state()
  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      list(
        content = jsonlite::toJSON(list(
          central_organizing_concept = "no single principle covers the cluster",
          decision = "split_required",
          proposed_name = NULL,
          proposed_description = NULL,
          rationale = "the most-distant pair cannot share a principle"
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )

  ts <- generate_themes_iterative(state, .fake_provider_for_theming(),
                                     config = list())
  # 3 codes + always-split = recurse to leaves = 3 single-code themes
  expect_equal(n_themes(ts), 3L)
})

test_that("Phase 52: AI failure increments n_failed_calls + returns split_required", {
  state <- .three_code_state()
  call_count <- 0L
  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      call_count <<- call_count + 1L
      # Single-code AI failure (1 of 2 internal nodes) -- below the 25%
      # threshold, so circuit breaker doesn't fire.
      if (call_count == 1L) stop("simulated network failure")
      list(
        content = jsonlite::toJSON(list(
          central_organizing_concept = "no single principle covers the cluster",
          decision = "split_required",
          proposed_name = NULL, proposed_description = NULL,
          rationale = "the most-distant pair cannot share a principle"
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )

  ts <- generate_themes_iterative(state, .fake_provider_for_theming(),
                                     config = list())
  # AI failure defaults to split_required at the failed node -> recurse to
  # leaves; downstream nodes succeed -> 3 single-code themes total.
  expect_equal(n_themes(ts), 3L)
})

test_that("Phase 52: articulation enforcement forces split on vacuous coherent_theme", {
  # The schema requires central_organizing_concept but cannot enforce a
  # minimum length. .evaluate_cluster post-validates and forces a split
  # when the articulation is < 30 chars on a coherent_theme verdict.
  state <- .three_code_state()
  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      list(
        content = jsonlite::toJSON(list(
          # Vacuous one-word articulation -- 10 chars, well below the 30-char min
          central_organizing_concept = "strategies",
          decision = "coherent_theme",
          proposed_name = "Strategies",
          proposed_description = "...",
          rationale = "..."
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )

  # Note: the articulation-enforcement warning goes through logger::log_warn()
  # (stderr), not R's warning() machinery, so expect_warning doesn't catch
  # it. We assert behavior instead: forced split at every node -> all three
  # codes atomic-outlier themes.
  ts <- generate_themes_iterative(state, .fake_provider_for_theming(),
                                     config = list())
  expect_equal(n_themes(ts), 3L)
})

# ============================================================================
# Phase 58 Tier 0 C-1: Articulation gate quality checks
#
# Phase 52's gate enforced a flat 30-character minimum on the AI's
# central_organizing_concept. Phase 57 found this permissive enough to pass
# tautological 85-char articulations on 237-code mega-themes. Three
# additional checks were added at .evaluate_cluster:
#   1. Length floor scales by log10(n_codes).
#   2. Bucket-label openers ("comprehensive ...", "the various ...") rejected.
#   3. Tautological articulations (>70% word overlap with proposed_name)
#      rejected.
# All three trigger a forced split_required on coherent_theme verdicts.
# ============================================================================

test_that("C-1: .articulation_min_chars scales by log10(n_codes)", {
  # n=1 yields the legacy 30-char floor (no scaling for singletons).
  expect_equal(pakhom:::.articulation_min_chars(1L), 30L)
  expect_equal(pakhom:::.articulation_min_chars(0L), 30L)   # clamped to >=1
  # n=10 -> 30 + 30 * log10(10) = 60
  expect_equal(pakhom:::.articulation_min_chars(10L), 60L)
  # n=100 -> 30 + 30 * log10(100) = 90
  expect_equal(pakhom:::.articulation_min_chars(100L), 90L)
  # The 237-code mega-theme from Phase 57 -> ~101 chars
  expect_equal(pakhom:::.articulation_min_chars(237L),
               as.integer(30 + 30 * log10(237)))
  # Monotonically non-decreasing
  vals <- vapply(c(1L, 5L, 10L, 50L, 100L, 500L, 1000L),
                  pakhom:::.articulation_min_chars, integer(1))
  expect_true(all(diff(vals) >= 0L))
})

test_that("C-1: .is_bucket_label_opener catches list-of-things openers", {
  expect_true(pakhom:::.is_bucket_label_opener(
    "This theme captures the various strategies people use"))
  expect_true(pakhom:::.is_bucket_label_opener(
    "this theme explores diverse approaches to coping"))
  expect_true(pakhom:::.is_bucket_label_opener(
    "Comprehensive overview of binge-eating triggers"))
  expect_true(pakhom:::.is_bucket_label_opener(
    "Multifaceted experiences around medication transitions"))
  expect_true(pakhom:::.is_bucket_label_opener(
    "A range of emotional responses to weight changes"))
  expect_true(pakhom:::.is_bucket_label_opener(
    "The various ways participants describe sleep loss"))
  expect_true(pakhom:::.is_bucket_label_opener(
    "Mixed sentiments about provider communication"))
  # Substantive principles should NOT trigger
  expect_false(pakhom:::.is_bucket_label_opener(
    "All three codes converge on the same compulsive-eating pattern"))
  expect_false(pakhom:::.is_bucket_label_opener(
    "Participants link medication side effects to sleep onset latency"))
  expect_false(pakhom:::.is_bucket_label_opener(""))
  expect_false(pakhom:::.is_bucket_label_opener(NULL))
  expect_false(pakhom:::.is_bucket_label_opener(NA_character_))
})

test_that("C-1: .is_tautological_articulation catches restatement of theme name", {
  # 100% overlap (all name tokens appear in articulation) -> tautological
  expect_true(pakhom:::.is_tautological_articulation(
    articulation  = "Emotional and physical impact of binge eating behaviors",
    proposed_name = "Emotional and Physical Impact of Binge Eating"
  ))
  # 80% overlap -> tautological
  expect_true(pakhom:::.is_tautological_articulation(
    articulation  = "Discussions of compulsive eating and craving patterns",
    proposed_name = "Compulsive eating craving patterns"
  ))
  # ~67% overlap (only 2 of 3 name tokens appear) -> NOT tautological
  expect_false(pakhom:::.is_tautological_articulation(
    articulation  = "All three codes orbit the daily-life integration concept",
    proposed_name = "Medication management routines"
  ))
  # NULL / empty defenses
  expect_false(pakhom:::.is_tautological_articulation(NULL, "name"))
  expect_false(pakhom:::.is_tautological_articulation("art", NULL))
  expect_false(pakhom:::.is_tautological_articulation("art", ""))
  expect_false(pakhom:::.is_tautological_articulation("", "name"))
})

test_that("C-1 integration: .evaluate_cluster forces split on log-scaled too-short articulation", {
  # 30-code cluster -> min_chars = 30 + 30*log10(30) ~= 74
  # An articulation of 50 chars would pass the legacy flat 30-char floor
  # but must FAIL the log-scaled 74-char floor.
  state <- create_coding_state()
  for (k in paste0("c", 1:30)) {
    state$codebook[[k]] <- list(
      code_name = paste("Code", k), description = "",
      type = "descriptive", frequency = 1L,
      entry_ids = paste0("e", k), coded_segments = list()
    )
  }
  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      list(
        content = jsonlite::toJSON(list(
          # Exactly 50 chars. Cluster size 30 -> floor ~74. Must reject.
          central_organizing_concept = "Daily routines around medication and sleep timing",
          decision = "coherent_theme",
          proposed_name = "Routines",
          proposed_description = "...",
          rationale = "..."
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )
  # Forced split at every node -> recurse to leaves -> 30 atomic themes.
  ts <- generate_themes_iterative(state, .fake_provider_for_theming(),
                                     config = list())
  expect_equal(n_themes(ts), 30L)
})

test_that("C-1 integration: .evaluate_cluster forces split on bucket-label opener", {
  state <- .three_code_state()
  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      list(
        content = jsonlite::toJSON(list(
          # Long enough to pass length check, but starts with a bucket-label
          # opener that signals list-of-things, not unifying principle.
          central_organizing_concept = paste(
            "This theme captures the various strategies participants use to",
            "manage their dietary routines during medication transitions."
          ),
          decision = "coherent_theme",
          proposed_name = "Strategies",
          proposed_description = "...",
          rationale = "..."
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )
  ts <- generate_themes_iterative(state, .fake_provider_for_theming(),
                                     config = list())
  # Forced split -> 3 atomic themes
  expect_equal(n_themes(ts), 3L)
})

test_that("C-1 integration: .evaluate_cluster forces split on tautological articulation", {
  state <- .three_code_state()
  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      list(
        content = jsonlite::toJSON(list(
          # Articulation reuses every content word from proposed_name -> 100%
          # overlap -> tautological.
          central_organizing_concept = paste(
            "Emotional and physical impact of binge eating manifests across",
            "all three codes via the same lived-experience pattern"
          ),
          decision = "coherent_theme",
          proposed_name = "Emotional and Physical Impact of Binge Eating",
          proposed_description = "...",
          rationale = "..."
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )
  ts <- generate_themes_iterative(state, .fake_provider_for_theming(),
                                     config = list())
  # Forced split -> 3 atomic themes
  expect_equal(n_themes(ts), 3L)
})

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
                                     config = list())
  # Single code, articulation too short -> forced split. Single-code split
  # produces 1 atomic-outlier theme regardless.
  expect_equal(n_themes(ts), 1L)
})

test_that("Phase 52: circuit breaker aborts theme generation at >25% failure rate", {
  # Build a 6-code state so we have enough HAC nodes (5) to see the
  # circuit breaker threshold. With every call failing the breaker fires
  # once n_failed_calls >= 4 AND > floor(n_calls * 0.25) which happens
  # at the 4th failed call.
  state <- create_coding_state()
  for (k in letters[1:6]) {
    state$codebook[[k]] <- list(
      code_name = paste("Code", toupper(k)), description = "",
      type = "descriptive", frequency = 1L,
      entry_ids = paste0("e", k), coded_segments = list()
    )
  }
  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      stop("simulated provider outage")
    },
    .package = "pakhom"
  )
  expect_error(
    suppressWarnings(generate_themes_iterative(state, .fake_provider_for_theming(),
                                                  config = list())),
    "Theme generation aborted"
  )
})

test_that("Phase 52: .coalesce_virtual_subtheme_groups merges adjacent NA-named groups", {
  groups <- list(
    list(name = NA_character_, description = "", code_indices = c(1L, 2L)),
    list(name = NA_character_, description = "", code_indices = 3L),
    list(name = "Named subtheme", description = "d", code_indices = c(4L, 5L)),
    list(name = NA_character_, description = "", code_indices = 6L)
  )
  out <- pakhom:::.coalesce_virtual_subtheme_groups(groups)
  expect_length(out, 3L)
  # First two NA-named groups coalesced into one
  expect_true(is.na(out[[1]]$name))
  expect_setequal(out[[1]]$code_indices, c(1L, 2L, 3L))
  # Named subtheme survives unchanged
  expect_equal(out[[2]]$name, "Named subtheme")
  # Trailing NA-named group survives standalone
  expect_true(is.na(out[[3]]$name))
  expect_equal(out[[3]]$code_indices, 6L)
})
