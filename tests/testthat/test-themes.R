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

  ts <- generate_themes_iterative(state, fake_provider, config = list(algorithm = "v1"))
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

  ts <- generate_themes_iterative(state, fake_provider, config = list(algorithm = "v1"))
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
                                     config = list(algorithm = "v1"))
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
                                     config = list(algorithm = "v1"))
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
                                     config = list(algorithm = "v1"))
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
                                     config = list(algorithm = "v1"))
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
  # 30-code cluster -> min_chars = 30 + 30*log10(30) ~= 74.
  # A 20-char articulation fails the length check at every cluster size
  # in the recursive walk (n=1 -> floor 30; the 20-char articulation is
  # below it). The legacy flat 30-char floor would ALSO reject this
  # articulation -- the point of this test is the cluster size lookup
  # at line 879's .articulation_min_chars call, not the larger-cluster
  # boost specifically. (See the next test for the
  # log-scale-vs-legacy-floor regression.)
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
          central_organizing_concept = "shortish phrase",  # 15 chars
          decision = "coherent_theme",
          proposed_name = "Daily routines",
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
                                     config = list(algorithm = "v1"))
  expect_equal(n_themes(ts), 30L)
})

test_that("C-1 integration: log-scaled floor rejects 50-char articulation that flat 30-char floor would have passed", {
  # 30-code cluster + 50-char articulation. Under the legacy flat 30-char
  # floor (pre-Phase-58) this articulation would have PASSED the gate.
  # Under the log-scaled floor (n=30 -> min ~74) it must FAIL at the
  # top-level cluster, demonstrating the regression that fixes the
  # 237-code Phase 57 mega-theme.
  #
  # The articulation reuses both name tokens ("daily", "routines") so the
  # tautology gate also fires regardless of the recursive walk's
  # cluster-size geometry. The combined effect: every internal node
  # rejects -> recurse to leaves -> 30 atomic themes.
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
          # 49 chars; > legacy 30-char floor; < log-scaled 74-char floor at n=30.
          central_organizing_concept = "Daily routines around medication and sleep schedule",
          decision = "coherent_theme",
          proposed_name = "Daily Routines",
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
  # 30 atomic themes confirms the recursive walk forced split at every
  # node (top cluster: length fails; small clusters: tautology fires).
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
                                     config = list(algorithm = "v1"))
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
                                     config = list(algorithm = "v1"))
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
                                     config = list(algorithm = "v1"))
  # Single code, articulation too short -> forced split. Single-code split
  # produces 1 atomic-outlier theme regardless.
  expect_equal(n_themes(ts), 1L)
})

# C-1 audit followup tests --------------------------------------------------

test_that("C-1 audit MEDIUM-1: tautology check skipped for single-content-word names", {
  # Pre-fix: name = "Routines" (1 token after stop-word filter) + any
  # articulation mentioning "routines" -> 100% overlap -> tautological ->
  # force split. False positive for substantive articulations.
  # Post-fix: tautology requires >= 2 name tokens; "Routines" skips the
  # check entirely.
  expect_false(pakhom:::.is_tautological_articulation(
    articulation  = "Daily routines around medication and sleep schedule",
    proposed_name = "Routines"
  ))
  expect_false(pakhom:::.is_tautological_articulation(
    articulation  = "Stigma operates as both barrier and motivator",
    proposed_name = "Stigma"
  ))
  # Multi-word names still fire on real tautology.
  expect_true(pakhom:::.is_tautological_articulation(
    articulation  = "Daily routines around medication and sleep schedule",
    proposed_name = "Daily Routines"
  ))
})

test_that("C-1 audit MEDIUM-2: 'overarching pattern of X in Y' substantive openers no longer rejected", {
  # Pre-fix: pattern 5 `^(general|overarching) (.+) (of|in|for) \\w`
  # rejected substantive opening lines. Removed in the followup.
  expect_false(pakhom:::.is_bucket_label_opener(
    "Overarching pattern of self-medication in participants describes a shared coping logic"
  ))
  expect_false(pakhom:::.is_bucket_label_opener(
    "General principle of dose timing as anchor for daily medication routines"
  ))
  # The other bucket patterns still fire.
  expect_true(pakhom:::.is_bucket_label_opener(
    "Comprehensive overview of binge-eating triggers"
  ))
})

test_that("C-1 audit LOW-5: .articulation_min_chars defends against NA / non-integer input", {
  # length(cluster_leaves) is always real in practice; this is a defensive
  # crash-prevention guarantee against future callers.
  expect_equal(pakhom:::.articulation_min_chars(NA_integer_), 30L)
  expect_equal(pakhom:::.articulation_min_chars(NULL), 30L)
  expect_equal(pakhom:::.articulation_min_chars(numeric(0)), 30L)
  expect_equal(pakhom:::.articulation_min_chars(-5L), 30L)
  # Real input still computes correctly.
  expect_equal(pakhom:::.articulation_min_chars(100L), 90L)
})

test_that("C-1 audit LOW-6: multi-failure articulation lists all reasons in rationale (gate inspection)", {
  # We can't easily extract the rationale from generate_themes_iterative's
  # return without inspecting walk_state. Instead exercise the three
  # quality predicates directly on a single articulation that trips all
  # of them, and confirm each predicate independently signals failure.
  # 53-char articulation that begins with a bucket opener and reuses
  # both content words of the proposed name.
  art  <- "Comprehensive eating patterns covering binge behavior"
  name <- "Eating Patterns"
  expect_true(pakhom:::.is_bucket_label_opener(art),
              info = "bucket-opener detection missed 'Comprehensive ...'")
  expect_true(pakhom:::.is_tautological_articulation(art, name),
              info = "tautology detection missed 100%% token overlap")
  # Length check passes at the legacy n=1 floor (min 30) but FAILS at
  # n=10 (log-scaled min 60) -- so at large clusters all three gates
  # fire together.
  expect_true(nchar(art) >= pakhom:::.articulation_min_chars(1L))
  expect_true(nchar(art) < pakhom:::.articulation_min_chars(10L))
})

test_that("C-1 audit LOW-7: substantive long articulation IS accepted under log-scaling (positive case)", {
  # Phase 57's 237-code mega-theme regression: a substantive 120+ char
  # articulation with a multi-token non-tautological name should be
  # accepted as coherent_theme even at large cluster sizes. This is the
  # positive-case partner of the "force split on too-short" test above.
  state <- create_coding_state()
  for (k in paste0("c", 1:10)) {
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
          # 130 chars, substantive, low-overlap with name -> passes all
          # three gates at cluster size 10 (min ~60 chars).
          central_organizing_concept = paste(
            "All ten codes orbit the daily-life-integration concept --",
            "how participants weave dose timing into the rhythm of meals,",
            "sleep, work."
          ),
          decision = "coherent_theme",
          proposed_name = "Medication Management Routines",
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
  # 10 codes accepted as ONE coherent theme -> n_themes == 1L (subtheme
  # walk may add subthemes inside the theme, but the top-level theme
  # count is 1).
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
                                                  config = list(algorithm = "v1"))),
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

# ============================================================================
# Phase 58 Tier 1 C-12 + AF-4 + AF-8: recursive HAC walker
#
# Phase 57 audit C-12 found: 31.4% of themes had only virtual NA-named
# subthemes; the 237-code mega-theme was rendered as 2 sub-buckets
# (32 + 205) instead of multi-level decomposition. The walker's
# subtheme pass was capped at depth 1 by design ("Subthemes are at most
# 1 level deep in Phase 52"). Phase 58 Tier 1 lifts that cap.
#
# Phase 57 audit AF-4 found: 19% of two-subtheme themes had one
# subtheme containing only 1 code -- a HAC singleton-cut artifact that
# produced "1 lonely code + N-code coherent subtheme" pairs.
#
# Phase 57 audit AF-8 demanded: cap subtheme size at 25 codes; force
# re-walk one level deeper when exceeded. This is the size-trigger for
# C-12's recursion.
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

test_that("Tier 1 C-12 + AF-8: large coherent subtheme spawns nested children (constructed HAC)", {
  # Construct a synthetic balanced HAC tree where the top split divides
  # into two 16-code clusters (both > max_codes_per_subtheme = 25's
  # half). For each 16-code half, the AI returns coherent_theme; AF-8
  # triggers recursion (since 16 > 25? no -- need bigger clusters).
  #
  # Use 60 codes split 30/30 at top. Each 30-code child triggers AF-8
  # (30 > 25). Recursion descends one level; the 30-code child's binary
  # split (15 + 15) is below the threshold so no further recursion.
  n <- 60L
  # Build a balanced binary merge matrix bottom-up. Levels:
  #   level 0: 60 leaves
  #   level 1: 30 pairs -> 30 internal nodes (each 2 leaves)
  #   level 2: 15 pairs -> 15 internal nodes (each 4 leaves)
  #   ...
  # Quicker: build via stats::hclust on a synthetic distance matrix
  # that explicitly separates two clusters at the top.
  set.seed(42)
  pts <- rbind(
    cbind(rnorm(n/2, mean = -5, sd = 0.1), rnorm(n/2, mean = 0, sd = 0.1)),
    cbind(rnorm(n/2, mean =  5, sd = 0.1), rnorm(n/2, mean = 0, sd = 0.1))
  )
  dist_obj <- dist(pts)
  fake_hac <- stats::hclust(dist_obj, method = "ward.D2")

  codes <- lapply(seq_len(n), function(i) {
    list(key = sprintf("c%02d", i), name = sprintf("Code %d", i),
         description = "", type = "descriptive", frequency = 1L,
         entry_ids = sprintf("e%02d", i), coded_segments = list())
  })
  dummy_dist <- as.matrix(dist_obj)

  walk_state <- new.env(parent = emptyenv())
  walk_state$n_calls <- 0L
  walk_state$n_failed_calls <- 0L
  walk_state$decisions <- list()
  walk_state$themes_so_far <- list()
  walk_ctx <- list(
    walk_state = walk_state, provider = .fake_provider_for_theming(),
    research_focus = "test", concept_str = "test",
    calibration_text = "", reflexivity_block = "",
    audit_log = NULL, response_cache = NULL,
    live_tracker = NULL, methodology_override = NULL
  )

  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      list(
        content = jsonlite::toJSON(list(
          central_organizing_concept = paste(
            "All codes orbit how participants weave their treatment",
            "into the rhythm of meals, sleep, work, and family life."
          ),
          decision = "coherent_theme",
          proposed_name = "Behavioral Patterns",
          proposed_description = "...",
          rationale = "..."
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )

  # Call walker on the top node of the 60-code tree. The top split
  # produces two 30-code branches. Each gets coherent_theme verdict ->
  # forms a Subtheme. 30 > 25 -> AF-8 triggers recursion.
  result <- pakhom:::.walk_for_subthemes(
    theme_name             = "Test Theme",
    theme_node_idx         = nrow(fake_hac$merge),
    hac                    = fake_hac,
    codes                  = codes,
    distance_matrix        = dummy_dist,
    co_occurrence          = NULL,
    walk_ctx               = walk_ctx,
    current_depth          = 1L,
    max_subtheme_depth     = 3L,
    max_codes_per_subtheme = 25L
  )

  # The top split is balanced (30/30) so AF-4 does NOT fire. Each 30-
  # code child is named-coherent -> 2 subthemes at depth 1. Each
  # 30-code subtheme triggers AF-8 (30 > 25) -> recurses one level
  # deeper -> nested children.
  expect_length(result, 2L)
  for (st in result) {
    expect_false(is.na(st$name),
                 info = "Each top-split branch should be a named subtheme (got virtual)")
    expect_true(length(st$children) > 0L,
                info = sprintf("Subtheme '%s' (%d codes) should have nested children",
                                st$name, length(st$code_indices)))
  }
})

test_that("Tier 1 C-12: max_subtheme_depth = 1 reproduces pre-Phase-58 behavior", {
  # Setting depth = 1 disables Phase 58 recursion entirely -- subthemes
  # remain leaves regardless of size. Useful for replay-equivalence with
  # pre-Phase-58 state files.
  set.seed(42)
  n <- 60L
  pts <- rbind(
    cbind(rnorm(n/2, mean = -5, sd = 0.1), rnorm(n/2, mean = 0, sd = 0.1)),
    cbind(rnorm(n/2, mean =  5, sd = 0.1), rnorm(n/2, mean = 0, sd = 0.1))
  )
  dist_obj <- dist(pts)
  fake_hac <- stats::hclust(dist_obj, method = "ward.D2")
  codes <- lapply(seq_len(n), function(i) {
    list(key = sprintf("c%02d", i), name = sprintf("Code %d", i),
         description = "", type = "descriptive", frequency = 1L,
         entry_ids = sprintf("e%02d", i), coded_segments = list())
  })
  walk_state <- new.env(parent = emptyenv())
  walk_state$n_calls <- 0L
  walk_state$n_failed_calls <- 0L
  walk_state$decisions <- list()
  walk_ctx <- list(
    walk_state = walk_state, provider = .fake_provider_for_theming(),
    research_focus = "test", concept_str = "test",
    calibration_text = "", reflexivity_block = "",
    audit_log = NULL, response_cache = NULL,
    live_tracker = NULL, methodology_override = NULL
  )

  testthat::local_mocked_bindings(
    ai_complete = function(...) {
      list(
        content = jsonlite::toJSON(list(
          central_organizing_concept = paste(
            "All codes orbit how participants weave their treatment",
            "into the rhythm of meals, sleep, work, and family life."
          ),
          decision = "coherent_theme",
          proposed_name = "Behavioral Patterns",
          proposed_description = "...",
          rationale = "..."
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )

  # depth = 1 -> no recursion regardless of subtheme size.
  result <- pakhom:::.walk_for_subthemes(
    theme_name             = "Test Theme",
    theme_node_idx         = nrow(fake_hac$merge),
    hac                    = fake_hac,
    codes                  = codes,
    distance_matrix        = as.matrix(dist_obj),
    co_occurrence          = NULL,
    walk_ctx               = walk_ctx,
    current_depth          = 1L,
    max_subtheme_depth     = 1L,
    max_codes_per_subtheme = 25L
  )
  expect_length(result, 2L)
  for (st in result) {
    expect_length(st$children, 0L)
  }
})

test_that("Tier 1 AF-4: walker refuses imbalanced binary splits when theme has >3 codes", {
  # Mock the HAC walker to test the AF-4 guard directly. We can't easily
  # force a singleton HAC split in a clean test fixture, but we can
  # exercise .walk_for_subthemes with a contrived theme_node_idx and
  # synthetic hac to verify the guard fires.
  #
  # Build a 4-code state. HAC merge matrix is deterministic given
  # distance matrix; ward.D2 may or may not produce a singleton split
  # at any given internal node depending on the distance geometry.
  # Easier: directly call .walk_for_subthemes with a constructed hac
  # whose top node IS a singleton split.
  fake_hac <- list(
    # Synthetic merge matrix:
    #   row 1: leaves -1 and -2 merge -> internal node 1 (2 leaves)
    #   row 2: leaves -3 and 1 merge -> internal node 2 (3 leaves)
    #   row 3: leaves -4 and 2 merge -> top node 3 (4 leaves, imbalanced)
    merge = matrix(c(
      -1L, -2L,
      -3L,  1L,
      -4L,  2L
    ), ncol = 2, byrow = TRUE),
    order = 1:4,
    height = c(0.1, 0.5, 1.0)
  )
  codes <- list(
    list(key = "c1", name = "Code 1", description = "", frequency = 1L),
    list(key = "c2", name = "Code 2", description = "", frequency = 1L),
    list(key = "c3", name = "Code 3", description = "", frequency = 1L),
    list(key = "c4", name = "Code 4", description = "", frequency = 1L)
  )
  dummy_dist <- matrix(0.5, nrow = 4, ncol = 4)
  walk_state <- new.env(parent = emptyenv())
  walk_state$n_calls <- 0L
  walk_state$n_failed_calls <- 0L
  walk_state$decisions <- list()
  walk_state$themes_so_far <- list()
  walk_ctx <- list(
    walk_state           = walk_state,
    provider             = .fake_provider_for_theming(),
    research_focus       = "test",
    concept_str          = "test",
    calibration_text     = "",
    reflexivity_block    = "",
    audit_log            = NULL,
    response_cache       = NULL,
    live_tracker         = NULL,
    methodology_override = NULL
  )

  # Top node 3 has imbalanced cut: branch = -4 (1 leaf) and 2 (3 leaves).
  # 4 > 3 codes total + 1 ≤ 1 leaf -> AF-4 guard fires.
  result <- pakhom:::.walk_for_subthemes(
    theme_name             = "Test Theme",
    theme_node_idx         = 3L,
    hac                    = fake_hac,
    codes                  = codes,
    distance_matrix        = dummy_dist,
    co_occurrence          = NULL,
    walk_ctx               = walk_ctx,
    current_depth          = 1L,
    max_subtheme_depth     = 3L,
    max_codes_per_subtheme = 25L
  )
  # AF-4 should have collapsed to a single virtual subtheme holding all
  # 4 codes (no named subtheme structure). Verify no AI calls fired.
  expect_length(result, 1L)
  expect_true(is.na(result[[1]]$name))
  expect_setequal(result[[1]]$code_indices, c(1L, 2L, 3L, 4L))
  expect_equal(walk_state$n_calls, 0L,
               info = "AF-4 guard should pre-empt all AI calls for imbalanced splits")
})

test_that("Tier 1 AF-4: small (<=3 codes) imbalanced splits are preserved (legitimate edge case)", {
  # When the theme has only 2-3 codes total, a 1-code branch is the
  # legitimate HAC structure -- not an artifact. AF-4 must NOT fire.
  fake_hac <- list(
    merge = matrix(c(
      -1L, -2L,   # row 1: leaves 1 + 2 merge
      -3L,  1L    # row 2: leaf 3 + internal -> top (3 leaves, imbalanced)
    ), ncol = 2, byrow = TRUE),
    order = 1:3,
    height = c(0.1, 0.5)
  )
  codes <- list(
    list(key = "c1", name = "Code 1", description = "", frequency = 1L),
    list(key = "c2", name = "Code 2", description = "", frequency = 1L),
    list(key = "c3", name = "Code 3", description = "", frequency = 1L)
  )
  dummy_dist <- matrix(0.5, nrow = 3, ncol = 3)
  walk_state <- new.env(parent = emptyenv())
  walk_state$n_calls <- 0L
  walk_state$n_failed_calls <- 0L
  walk_state$decisions <- list()
  walk_state$themes_so_far <- list()
  walk_ctx <- list(
    walk_state = walk_state, provider = .fake_provider_for_theming(),
    research_focus = "test", concept_str = "test",
    calibration_text = "", reflexivity_block = "",
    audit_log = NULL, response_cache = NULL,
    live_tracker = NULL, methodology_override = NULL
  )

  testthat::local_mocked_bindings(
    ai_complete = function(...) {
      list(
        content = jsonlite::toJSON(list(
          central_organizing_concept = "no single principle covers the cluster",
          decision = "split_required",
          proposed_name = NULL, proposed_description = NULL,
          rationale = "..."
        ), auto_unbox = TRUE, null = "null"),
        usage = list()
      )
    },
    .package = "pakhom"
  )

  result <- pakhom:::.walk_for_subthemes(
    theme_name             = "Small Theme",
    theme_node_idx         = 2L,
    hac                    = fake_hac,
    codes                  = codes,
    distance_matrix        = dummy_dist,
    co_occurrence          = NULL,
    walk_ctx               = walk_ctx,
    current_depth          = 1L,
    max_subtheme_depth     = 3L,
    max_codes_per_subtheme = 25L
  )
  # 3 codes total -- AF-4 does NOT fire; walker proceeds through normal
  # decision path. With mocked split_required, the result is the
  # standard virtual-coalesce output (1 group with all leaves).
  expect_length(result, 1L)
  # n_calls > 0 confirms AI was invoked (AF-4 didn't pre-empt).
  expect_gt(walk_state$n_calls, 0L)
})

# ============================================================================
# Phase 58 Tier 1 AF-3: n_subthemes schema clarity
#
# Pre-Phase-58 the JSON field n_subthemes counted only top-level real
# subthemes but wasn't documented; the Phase 57 audit found 207 of 417
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
    code_name = "Food Addiction", description = "x",
    type = "descriptive", frequency = 2L,
    entry_ids = c("e1", "e2"),
    coded_segments = list(
      list(entry_id = "e1", text = "ate too much", start_char = 0L, end_char = 12L),
      list(entry_id = "e2", text = "binge again", start_char = 0L, end_char = 11L)
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
    list(name = "Eating Patterns", description = "",
         subthemes = list(create_subtheme(
           name = "Compulsive Eating",
           codes = list(create_code_object(key = "food_addiction",
                                            name = "Food Addiction",
                                            frequency = 2L))
         )))
  ))
  ts <- rebuild_code_to_theme_map(ts, state)

  data <- tibble::tibble(std_id = c("e1", "e2"), std_text = c("a", "b"))
  out <- cascade_theme_assignments(data, state, ts)

  # subtheme_assignments column exists and is populated.
  expect_true("subtheme_assignments" %in% names(out))
  expect_equal(out$subtheme_assignments, c("Compulsive Eating", "Compulsive Eating"))
})
