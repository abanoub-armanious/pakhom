# Phase 62.1 -- metric relevance/provenance framing. The methodology assistant
# judges, in free-form prose, what each numeric column MEASURES and whether it is
# a substantive measure of the phenomenon vs incidental source/platform metadata,
# reasoned against the research focus. The report groups the per-metric
# interpretations accordingly. This is principle-clean: a FREE STRING (not an
# enum, not a fixed taxonomy the researcher configures), and the grouping reads
# the AI's OWN prose -- never the package classifying the column.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- schema: strict-mode + free-string (no enum) ----------------------------

test_that("metric_intelligence schema adds metric_provenance to properties AND required (strict-mode)", {
  s <- pakhom:::.metric_intelligence_schema()
  expect_silent(pakhom:::.validate_schema(s))            # required must ⊇ properties
  cr <- s$properties$metrics$items
  expect_true("metric_provenance" %in% names(cr$properties))
  expect_true("metric_provenance" %in% unlist(cr$required))
  expect_equal(cr$properties$metric_provenance$type, "string")
})

test_that("metric_provenance is a FREE STRING, not an enum (LBP-1 / no menu)", {
  s <- pakhom:::.metric_intelligence_schema()
  cr <- s$properties$metrics$items
  expect_null(cr$properties$metric_provenance$enum)
  # and the primitive field stays a free string too (regression guard)
  expect_null(cr$properties$requested_primitives$items$properties$primitive$enum)
})

# ---- coercion / serialization round-trip + R7 back-compat -------------------

test_that(".coerce_column_record keeps metric_provenance; serializes only when non-empty", {
  rec <- list(column_name = "score", column_description = "upvotes; heavy-tailed",
              requested_primitives = list(list(primitive = "prim_median", rationale = "r")),
              interpretation_note = "cite median",
              metric_provenance = "platform engagement metadata; reflects reception, not the phenomenon")
  co <- pakhom:::.coerce_column_record(rec)
  expect_equal(co$metric_provenance, rec$metric_provenance)
  ser <- pakhom:::.column_record_to_list(co)
  expect_equal(ser$metric_provenance, rec$metric_provenance)
})

test_that("R7 replay back-compat: an OLD pinned block without metric_provenance loads as \"\"", {
  old <- list(column_name = "score", column_description = "d",
              requested_primitives = list(list(primitive = "prim_median", rationale = "r")),
              interpretation_note = "n")            # no metric_provenance field
  co <- pakhom:::.coerce_column_record(old)
  expect_identical(co$metric_provenance, "")
})

test_that("BC-3 byte-identical: empty provenance is NOT serialized (no extra key leaks)", {
  old <- list(column_name = "score", column_description = "d",
              requested_primitives = list(list(primitive = "prim_median", rationale = "r")),
              interpretation_note = "n")
  ser <- pakhom:::.column_record_to_list(pakhom:::.coerce_column_record(old))
  expect_null(ser$metric_provenance)
  expect_setequal(names(ser),
                  c("column_name", "column_description", "requested_primitives",
                    "interpretation_note"))
})

# ---- prose-driven grouping (the AI's judgment, not ours) --------------------

test_that(".metric_provenance_group routes the AI's prose, not the column", {
  meta <- list(metric_provenance = "Reddit net upvotes; platform engagement metadata.")
  subst <- list(metric_provenance = "A validated PHQ-9 depression severity score.")
  none  <- list(metric_provenance = "")
  expect_equal(pakhom:::.metric_provenance_group(meta), "metadata")
  expect_equal(pakhom:::.metric_provenance_group(subst), "substantive")
  expect_equal(pakhom:::.metric_provenance_group(none), "")   # ungrouped -> back-compat
})

# ---- renderer: grouping + relevance column + back-compat --------------------

.mk_prov_rec <- function(name, prov) list(
  column_name = name, column_description = paste(name, "desc"),
  requested_primitives = list(list(primitive = "prim_median", rationale = "r")),
  interpretation_note = "read it", metric_provenance = prov)

test_that("Methodology Setup groups substantive vs metadata + shows the relevance column", {
  mi <- new_metric_interpretation(metrics = list(
    .mk_prov_rec("phq9", "A validated depression severity score; substantive measure of the phenomenon."),
    .mk_prov_rec("score", "Reddit net upvotes; platform engagement metadata, reflects reception not the phenomenon.")),
    source = "ai")
  art <- new_methodology_articulations(
    new_relevance_criterion(relevance_criterion = "on-focus iff it concerns the lived experience of X under study"),
    mi, research_focus = "f", source = "ai")
  h <- pakhom:::.build_methodology_setup_section(art)
  expect_match(h, "substantive measures", fixed = TRUE)
  expect_match(h, "source / engagement metadata", fixed = TRUE)
  expect_match(h, "Relevance to focus", fixed = TRUE)
  expect_match(h, "validated depression severity", fixed = TRUE)   # AI prose rendered
  expect_match(h, "reception/salience signals", fixed = TRUE)      # metadata caveat
  expect_lt(regexpr("phq9", h), regexpr(">score<", h))             # substantive group first
})

test_that("Methodology Setup is BACK-COMPAT: no provenance -> single neutral table, no grouping", {
  mi <- new_metric_interpretation(metrics = list(
    .mk_prov_rec("score", ""), .mk_prov_rec("ratio", "")), source = "ai")
  art <- new_methodology_articulations(
    new_relevance_criterion(relevance_criterion = "on-focus iff it concerns the lived experience of X under study"),
    mi, source = "ai")
  h <- pakhom:::.build_methodology_setup_section(art)
  expect_match(h, "<h3>Metric interpretations</h3>", fixed = TRUE)  # original single table
  expect_false(grepl("substantive measures", h, fixed = TRUE))      # no grouping
  expect_match(h, "&mdash;", fixed = TRUE)                          # empty provenance -> em dash, not fabricated
})
