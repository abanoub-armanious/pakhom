# Phase 63: research-question coverage lens.
#
# A late, post-hoc AI pass over (research_focus + concepts + final themes) that
# judges WHERE each named focus facet landed. The AI judges; the package renders.
# Dispersion across themes is a valid inductive outcome, never a coverage failure.
# Principle guards: no package taxonomy of content; the only enum is the AI's own
# structural self-assessment; nothing is hidden.

# A valid AI coverage response (matches .research_coverage_schema()).
.rc_fake_response <- function() {
  list(
    facets = list(
      list(facet = "physical effects", coverage_level = "dispersed",
           supporting_codes = list("Fatigue", "Stomach pain"),
           landed_in_themes = list("Emotional Aftermath", "Behavioral Coping"),
           coverage_note = "Coded but spread across themes -- a valid emergent outcome."),
      list(facet = "sleep", coverage_level = "not_surfaced",
           supporting_codes = list(), landed_in_themes = list(),
           coverage_note = "The corpus did not raise sleep.")
    ),
    overall_note = "Most facets addressed; sleep was not surfaced."
  )
}

.rc_mock_ai <- function() {
  function(provider, prompt, system_prompt, task, temperature = 0,
           response_schema = NULL, ...) {
    list(content = jsonlite::toJSON(.rc_fake_response(), auto_unbox = TRUE, null = "null"),
         usage = list())
  }
}

.rc_theme_set <- function() {
  # A minimal object with non-empty $themes is enough; ai_complete is mocked, so
  # the prompt's theme block content is irrelevant to these tests.
  list(themes = list(
    list(name = "Emotional Aftermath", description = "guilt + shame after a binge"),
    list(name = "Behavioral Coping",   description = "restriction, exercise, hiding")
  ))
}

test_that("research-coverage schema is OpenAI strict-mode valid", {
  sch <- .research_coverage_schema()
  expect_silent(.validate_schema(sch))
  facet_props <- sch$properties$facets$items$properties
  expect_true(all(c("facet", "coverage_level", "supporting_codes",
                    "landed_in_themes", "coverage_note") %in% names(facet_props)))
  expect_identical(facet_props$coverage_level$enum,
                   list("central", "dispersed", "peripheral", "not_surfaced"))
  expect_true("overall_note" %in% unlist(sch$required))
})

test_that(".coerce_coverage_facet keeps facets, vectorizes, and is defensive", {
  f <- .coerce_coverage_facet(list(facet = "recovery", coverage_level = "central",
        supporting_codes = list("Recovery strategy"),
        landed_in_themes = list("Recovery"), coverage_note = "Forms its own theme."))
  expect_identical(f$facet, "recovery")
  expect_identical(f$coverage_level, "central")
  expect_identical(f$supporting_codes, "Recovery strategy")
  # out-of-enum level -> conservative descriptive bucket, never dropped or inflated
  f2 <- .coerce_coverage_facet(list(facet = "x", coverage_level = "BOGUS",
        landed_in_themes = list("A", "B")))
  expect_true(f2$coverage_level %in% .RESEARCH_COVERAGE_LEVELS)
  expect_identical(f2$coverage_level, "dispersed")  # has themes -> dispersed
  expect_null(.coerce_coverage_facet(list(coverage_level = "central")))  # no facet -> dropped
  expect_null(.coerce_coverage_facet(list(facet = "")))                  # empty facet -> dropped
})

test_that("assess_research_coverage parses the AI response into a ResearchCoverage", {
  testthat::local_mocked_bindings(ai_complete = .rc_mock_ai())
  cov <- assess_research_coverage(
    research_focus = "lived experience: emotional triggers, physical effects, sleep",
    concepts = c("binge eating", "sleep"),
    theme_set = .rc_theme_set(), provider = list())
  expect_s3_class(cov, "ResearchCoverage")
  expect_equal(length(cov$facets), 2L)
  expect_identical(cov$facets[[1]]$facet, "physical effects")
  expect_identical(cov$facets[[1]]$coverage_level, "dispersed")
  expect_identical(cov$facets[[2]]$coverage_level, "not_surfaced")
  expect_match(cov$overall_note, "sleep")
})

test_that("assess_research_coverage returns empty coverage (NO AI call) when there are no themes", {
  called <- new.env(parent = emptyenv()); called$n <- 0L
  testthat::local_mocked_bindings(ai_complete = function(...) { called$n <- called$n + 1L; stop("should not be called") })
  cov <- assess_research_coverage("focus", NULL, list(themes = list()), provider = list())
  expect_s3_class(cov, "ResearchCoverage")
  expect_equal(length(cov$facets), 0L)
  expect_equal(called$n, 0L)
})

test_that("assess_research_coverage AUDITED path does not crash (research_coverage decision-type registered)", {
  # Regression guard for the C1-class landmine: log_ai_decision validates step AND
  # decision_type. The prior session's 64 methodology tests all used audit=NULL and
  # missed exactly this. Here we exercise the AUDITED path end-to-end.
  td <- file.path(tempdir(), paste0("rc_audit_", as.integer(Sys.time())))
  dir.create(td, showWarnings = FALSE, recursive = TRUE)
  audit <- init_audit_log(td, config = NULL)
  testthat::local_mocked_bindings(ai_complete = .rc_mock_ai())
  expect_no_error(
    cov <- assess_research_coverage("focus: physical effects, sleep", c("a"),
             .rc_theme_set(), provider = list(), audit_log = audit)
  )
  expect_s3_class(cov, "ResearchCoverage")
  # the decision was written with the registered type
  dec <- file.path(td, "ai_decisions.jsonl")
  if (file.exists(dec)) {
    expect_true(any(grepl("research_coverage", readLines(dec, warn = FALSE))))
  }
})

test_that("ResearchCoverage round-trips through serialize + the run-dir loader", {
  cov <- new_research_coverage(facets = list(
    list(facet = "physical effects", coverage_level = "dispersed",
         supporting_codes = c("Fatigue"), landed_in_themes = c("Aftermath", "Coping"),
         coverage_note = "spread")),
    overall_note = "ok")
  td <- file.path(tempdir(), paste0("rc_arch_", as.integer(Sys.time())))
  dir.create(td, showWarnings = FALSE, recursive = TRUE)
  archive_research_coverage(cov, td)
  expect_true(file.exists(file.path(td, "rules", "research_coverage.json")))
  expect_true(file.exists(file.path(td, "rules", "research_coverage.md")))
  back <- .load_research_coverage_from_run_dir(td)
  expect_s3_class(back, "ResearchCoverage")
  expect_equal(length(back$facets), 1L)
  expect_identical(back$facets[[1]]$coverage_level, "dispersed")
  expect_identical(back$facets[[1]]$landed_in_themes, c("Aftermath", "Coping"))
  expect_null(.load_research_coverage_from_run_dir(file.path(td, "nonexistent")))
})

test_that(".build_research_coverage_section renders facets + frames dispersion as valid; omits when empty", {
  cov <- new_research_coverage(facets = list(
    list(facet = "physical effects", coverage_level = "dispersed",
         supporting_codes = c("Fatigue", "Stomach pain"),
         landed_in_themes = c("Emotional Aftermath", "Behavioral Coping"),
         coverage_note = "Spread across themes -- valid emergent grouping."),
    list(facet = "sleep", coverage_level = "not_surfaced",
         supporting_codes = character(0), landed_in_themes = character(0),
         coverage_note = "Corpus did not raise sleep.")),
    overall_note = "Most facets addressed.")
  html <- .build_research_coverage_section(cov)
  expect_match(html, "research-coverage-table", fixed = TRUE)
  expect_match(html, "rc-dispersed", fixed = TRUE)
  expect_match(html, "Dispersed across themes", fixed = TRUE)
  expect_match(html, "valid outcome of inductive thematic analysis", fixed = TRUE)
  expect_match(html, "Mode 3", fixed = TRUE)               # pointer to guaranteed coverage
  expect_match(html, "Not surfaced in corpus", fixed = TRUE)
  expect_match(html, "not a score", fixed = TRUE)          # framed as a map, not a scorecard
  # back-compat: no facets / NULL -> section omitted entirely
  expect_identical(.build_research_coverage_section(new_research_coverage()), "")
  expect_identical(.build_research_coverage_section(NULL), "")
})
