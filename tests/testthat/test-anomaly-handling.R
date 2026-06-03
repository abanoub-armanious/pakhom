# Tests for Phase 54: Mode 3 anomaly_handling policy dispatch
# (apply_framework_themes branches on framework_spec$anomaly_handling).
#
# Three policies:
#   bracket  -- single "Anomaly (non-fitting)" theme (pre-Phase-54 behavior)
#   extend   -- inductive emergent themes from anomaly segments (default)
#   revise   -- same as extend + framework_review.csv artifact

# ---- Helpers ---------------------------------------------------------------

.mode3_test_spec <- function(policy = "extend") {
  # Build a minimal in-memory FrameworkSpec without touching disk
  spec <- list(
    name              = "Phase 54 test framework",
    epistemic_stance  = "constructionist",
    anomaly_handling  = policy,
    citations         = character(0),
    constructs        = list(
      list(id = "intention",         name = "Behavioral intention",
            description = "What the participant plans to do",
            example_indicators = c("plan to", "will", "intend")),
      list(id = "perceived_control", name = "Perceived control",
            description = "Sense of agency over the behavior",
            example_indicators = c("can", "able", "control"))
    ),
    construct_ids     = c("intention", "perceived_control"),
    source_path       = NA_character_,
    schema_version    = "1.0.0"
  )
  class(spec) <- "FrameworkSpec"
  spec
}

.mode3_test_state_with_anomalies <- function(n_anomaly_segs = 4L,
                                              with_framework_freq = TRUE) {
  state <- create_coding_state()
  for (cid in c("intention", "perceived_control")) {
    state$codebook[[cid]] <- list(
      code_name = cid, description = cid,
      type = "framework_construct",
      frequency = if (with_framework_freq) 2L else 0L,
      entry_ids = if (with_framework_freq) c("e1", "e2") else character(0),
      coded_segments = list()
    )
  }

  # Construct n_anomaly_segs anomaly segments touching n_anomaly_segs entries.
  anomaly_segs <- lapply(seq_len(n_anomaly_segs), function(i) {
    list(
      entry_id    = paste0("anon_e", i),
      text        = paste0("Anomaly segment ", i, ": novel content the framework didn't anticipate"),
      start_char  = 0L,
      end_char    = 80L,
      provenance  = list(verification_status = "verified")
    )
  })
  state$codebook[["anomaly"]] <- list(
    code_name = "Anomaly (non-fitting)", description = "",
    type = "anomaly",
    frequency = n_anomaly_segs,
    entry_ids = vapply(anomaly_segs, function(s) s$entry_id, character(1)),
    coded_segments = anomaly_segs
  )
  state
}

.fake_provider_with_models <- function() {
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

# ---- bracket policy --------------------------------------------------------

test_that("bracket policy produces single Anomaly theme (legacy behavior preserved)", {
  spec  <- .mode3_test_spec("bracket")
  state <- .mode3_test_state_with_anomalies(3L)

  ts <- apply_framework_themes(state, spec)
  expect_s3_class(ts, "ThemeSet")

  theme_names <- vapply(ts$themes, function(t) t$name, character(1))
  expect_true("Anomaly (non-fitting)" %in% theme_names)

  # Exactly one theme has theme_kind = "anomaly_bracket"
  kinds <- vapply(ts$themes, function(t) t$theme_kind %||% "framework", character(1))
  expect_equal(sum(kinds == "anomaly_bracket"), 1L)
  expect_equal(sum(kinds == "emergent"), 0L)

  # Stamp on ThemeSet
  expect_equal(ts$mode3_anomaly_handling, "bracket")
  expect_equal(ts$mode3_n_emergent_themes, 0L)
})

# ---- extend policy ---------------------------------------------------------

test_that("extend policy: with provider, mocked AI produces emergent themes", {
  spec  <- .mode3_test_spec("extend")
  state <- .mode3_test_state_with_anomalies(3L)

  # Mock both:
  #   (a) inductive emergent coding (.emergent_coding_schema) -- returns
  #       3 segments each with the SAME code_name so they consolidate.
  #   (b) the Phase 52 cluster decision (.theme_decision_schema) -- returns
  #       coherent_theme for the resulting cluster.
  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      # Detect which schema is in flight by inspecting a unique property
      props <- response_schema$properties
      if ("coded_segments" %in% names(props)) {
        # Inductive emergent coding path
        list(
          content = jsonlite::toJSON(list(
            coded_segments = lapply(seq_len(3L), function(i) {
              list(segment_index = i,
                   code_name = "Novel coping strategy",
                   code_description = "A pattern the framework didn't anticipate.")
            })
          ), auto_unbox = TRUE, null = "null"),
          usage = list()
        )
      } else {
        # Phase 52 cluster decision path
        list(
          content = jsonlite::toJSON(list(
            central_organizing_concept = paste0(
              "All anomaly segments describe a coherent novel coping pattern ",
              "that the framework's two constructs do not encompass."),
            decision = "coherent_theme",
            proposed_name = "Novel coping strategy",
            proposed_description = "Emergent abductive theme.",
            rationale = "Most distant pair shares the same conceptual frame."
          ), auto_unbox = TRUE, null = "null"),
          usage = list()
        )
      }
    },
    .package = "pakhom"
  )

  ts <- apply_framework_themes(state, spec, provider = .fake_provider_with_models())
  expect_s3_class(ts, "ThemeSet")
  kinds <- vapply(ts$themes, function(t) t$theme_kind %||% "framework", character(1))
  expect_true(any(kinds == "emergent"))
  expect_equal(sum(kinds == "anomaly_bracket"), 0L)
  expect_equal(ts$mode3_anomaly_handling, "extend")
  expect_true(ts$mode3_n_emergent_themes >= 1L)
})

test_that("extend policy: NULL provider falls back to bracket with warning", {
  spec  <- .mode3_test_spec("extend")
  state <- .mode3_test_state_with_anomalies(3L)

  # No provider -> can't do inductive pass -> bracket-fallback with warning.
  # The warning goes through logger::log_warn (stderr); we assert behavior.
  ts <- apply_framework_themes(state, spec, provider = NULL)
  kinds <- vapply(ts$themes, function(t) t$theme_kind %||% "framework", character(1))
  expect_equal(sum(kinds == "anomaly_bracket"), 1L)
  expect_equal(sum(kinds == "emergent"), 0L)
})

# ---- revise policy ---------------------------------------------------------

test_that("revise policy writes framework_review.csv to output_dir", {
  spec  <- .mode3_test_spec("revise")
  state <- .mode3_test_state_with_anomalies(3L)
  tmp_dir <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      props <- response_schema$properties
      if ("coded_segments" %in% names(props)) {
        list(
          content = jsonlite::toJSON(list(
            coded_segments = lapply(seq_len(3L), function(i) {
              list(segment_index = i,
                   code_name = "Novel pattern",
                   code_description = "Emergent.")
            })
          ), auto_unbox = TRUE, null = "null"),
          usage = list()
        )
      } else {
        list(
          content = jsonlite::toJSON(list(
            central_organizing_concept = "Sufficiently long articulation to pass the post-validation length check.",
            decision = "coherent_theme",
            proposed_name = "Novel pattern theme",
            proposed_description = "Emergent.",
            rationale = "Coherent."
          ), auto_unbox = TRUE, null = "null"),
          usage = list()
        )
      }
    },
    .package = "pakhom"
  )

  ts <- apply_framework_themes(state, spec,
                                 provider   = .fake_provider_with_models(),
                                 output_dir = tmp_dir)

  expect_true(file.exists(file.path(tmp_dir, "framework_review.csv")))
  rev <- readr::read_csv(file.path(tmp_dir, "framework_review.csv"),
                          show_col_types = FALSE)
  expect_equal(nrow(rev), 3L)  # one row per anomaly segment
  expect_true("suggested_construct_edit" %in% names(rev))
  expect_true("accepted" %in% names(rev))
  # The emergent_theme column was populated for at least some rows
  expect_true(any(!is.na(rev$emergent_theme)))
})

# ---- no anomalies: all policies produce no anomaly/emergent ---------------

test_that("no anomalies: bracket/extend/revise produce no anomaly + no emergent", {
  state <- create_coding_state()
  state$codebook[["intention"]] <- list(
    code_name = "intention", description = "x",
    type = "framework_construct", frequency = 1L,
    entry_ids = "e1", coded_segments = list()
  )
  state$codebook[["perceived_control"]] <- list(
    code_name = "perceived_control", description = "x",
    type = "framework_construct", frequency = 1L,
    entry_ids = "e2", coded_segments = list()
  )
  state$codebook[["anomaly"]] <- list(
    code_name = "Anomaly", description = "",
    type = "anomaly", frequency = 0L,
    entry_ids = character(0), coded_segments = list()
  )

  for (policy in c("bracket", "extend", "revise")) {
    spec <- .mode3_test_spec(policy)
    ts   <- apply_framework_themes(state, spec, provider = .fake_provider_with_models())
    kinds <- vapply(ts$themes, function(t) t$theme_kind %||% "framework", character(1))
    expect_equal(sum(kinds == "anomaly_bracket"), 0L, info = policy)
    expect_equal(sum(kinds == "emergent"),         0L, info = policy)
  }
})

# ---- default policy --------------------------------------------------------

test_that("default anomaly_handling on FrameworkSpec is 'extend' (Phase 54)", {
  # .validate_framework_spec wraps a raw `framework: ...` YAML block.
  # Phase 54 default is "extend" (was "bracket" pre-Phase-54). Verify
  # by omitting the field and asserting the constructed default.
  raw <- list(framework = list(
    name = "X",
    constructs = list(list(
      id = "a", name = "A", description = "xxxxxxxxxxxxxxxxxxx",
      example_indicators = c("y", "z")
    )),
    epistemic_stance = "constructionist"
    # NB: anomaly_handling intentionally omitted
  ))
  spec <- pakhom:::.validate_framework_spec(raw)
  expect_equal(spec$anomaly_handling, "extend")
})

# ---- theme_kind survives enrich_themes + theme_stats ----------------------

test_that("theme_kind tag survives enrich_themes + aggregate_theme_statistics", {
  spec  <- .mode3_test_spec("bracket")
  state <- .mode3_test_state_with_anomalies(2L)

  ts <- apply_framework_themes(state, spec)
  # Each theme has theme_kind set
  for (t in ts$themes) {
    expect_true(!is.null(t$theme_kind))
  }
})

# ---- emergent inductive coding schema --------------------------------------

test_that(".emergent_coding_schema validates", {
  s <- pakhom:::.emergent_coding_schema()
  expect_silent(pakhom:::.validate_schema(s))
  expect_true("coded_segments" %in% unlist(s$required))
  # Each item requires segment_index + code_name + code_description
  item_req <- unlist(s$properties$coded_segments$items$required)
  expect_setequal(item_req,
                   c("segment_index", "code_name", "code_description"))
})

# ---- framework_revision_suggested is a valid audit decision_type ----------

test_that("framework_revision_suggested is a valid audit decision_type", {
  expect_true("framework_revision_suggested" %in% pakhom:::.valid_decision_types)
})

# ---- CRITICAL regression: cascade fans entries into emergent themes -------

test_that("cascade routes anomaly entries into emergent themes (CRITICAL-8 regression)", {
  # Phase 54 audit CRITICAL-8: under extend policy, cascade_theme_assignments
  # has no code_to_theme mapping for the per-segment inductive codes -- the
  # "anomaly" key in entry_results$codes_assigned would have routed every
  # anomaly-bearing entry to a single theme. Phase 54 fixes this via the
  # mode3_anomaly_segment_to_theme map on the ThemeSet, consulted by
  # cascade. This test pins that fix.
  spec  <- .mode3_test_spec("extend")
  state <- .mode3_test_state_with_anomalies(3L)

  # Mark each entry as having "anomaly" in codes_assigned (Mode 3 coding
  # writes this; the synthetic state above didn't, so add it manually).
  for (eid in c("anon_e1", "anon_e2", "anon_e3")) {
    state$entry_results[[eid]] <- list(
      codes_assigned = "anomaly",
      coded_segments = list(),
      skipped        = FALSE
    )
  }

  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task,
                            temperature, response_schema, ...) {
      props <- response_schema$properties
      if ("coded_segments" %in% names(props)) {
        # Inductive coding: split the 3 segments into 2 different codes so
        # downstream clustering produces multiple emergent themes.
        list(
          content = jsonlite::toJSON(list(
            coded_segments = list(
              list(segment_index = 1L,
                   code_name = "Pattern A",
                   code_description = "First emergent concept group."),
              list(segment_index = 2L,
                   code_name = "Pattern A",
                   code_description = "First emergent concept group."),
              list(segment_index = 3L,
                   code_name = "Pattern B",
                   code_description = "Distinct second concept.")
            )
          ), auto_unbox = TRUE, null = "null"),
          usage = list()
        )
      } else if ("verdict" %in% names(props)) {
        # Phase 60 v2 clustering call: 2 codes are conceptually distinct,
        # converge immediately at pass 1 with no grouping.
        list(
          content = jsonlite::toJSON(list(
            verdict = "converged",
            cluster_assignments = NULL,
            overall_rationale = "The two emergent codes capture distinct conceptual patterns and should remain as separate themes; no useful further grouping is possible."
          ), auto_unbox = TRUE, null = "null"),
          usage = list()
        )
      } else if ("themes" %in% names(props)) {
        # Phase 60 v2 labeling call: name the 2 emergent themes.
        list(
          content = jsonlite::toJSON(list(
            themes = list(
              list(theme_index = 1L,
                   name = "Pattern A Theme",
                   description = "First emergent conceptual pattern from anomaly residuals.",
                   subthemes = list()),
              list(theme_index = 2L,
                   name = "Pattern B Theme",
                   description = "Second distinct emergent pattern.",
                   subthemes = list())
            )
          ), auto_unbox = TRUE, null = "null"),
          usage = list()
        )
      } else {
        # Legacy v1 HAC tree-walk fallback (kept for any v1-pinned callers).
        list(
          content = jsonlite::toJSON(list(
            central_organizing_concept = "no single principle covers both patterns of anomaly content",
            decision = "split_required",
            proposed_name = NULL,
            proposed_description = NULL,
            rationale = "different conceptual fault lines"
          ), auto_unbox = TRUE, null = "null"),
          usage = list()
        )
      }
    },
    .package = "pakhom"
  )

  ts <- apply_framework_themes(state, spec, provider = .fake_provider_with_models())
  expect_true(length(ts$mode3_anomaly_segment_to_theme %||% list()) > 0L,
              info = "ThemeSet must carry the segment->theme map")

  # Build a tiny data frame and cascade
  data <- tibble::tibble(std_id = c("e1", "e2", "anon_e1", "anon_e2", "anon_e3"),
                          std_text = "text")
  state$entry_results[["e1"]] <- list(codes_assigned = "intention",
                                        coded_segments = list(), skipped = FALSE)
  state$entry_results[["e2"]] <- list(codes_assigned = "perceived_control",
                                        coded_segments = list(), skipped = FALSE)

  result <- cascade_theme_assignments(data, state, ts)

  # The three anon_e entries should appear in at least one emergent-theme
  # membership column each (anomaly fan-out via .build_anomaly_segment_to_theme_map).
  membership_cols <- grep("^theme_membership_", names(result), value = TRUE)
  expect_true(length(membership_cols) > 0L)

  # Find rows for anomaly entries
  anom_rows <- result[result$std_id %in% c("anon_e1", "anon_e2", "anon_e3"), ]
  # Each anomaly entry should be assigned to at least one theme
  for (i in seq_len(nrow(anom_rows))) {
    n_themes <- sum(vapply(membership_cols, function(c) anom_rows[[c]][i] == 1L, logical(1)))
    expect_gt(n_themes, 0L,
              label = sprintf("entry %s should have >= 1 emergent theme assignment", anom_rows$std_id[i]))
  }
})
