# Regression tests for the Batch-10 provenance/IRR correctness fixes (audit 2026-06-11).

test_that("set-based Krippendorff alpha is NA for a single coded unit (degenerate)", {
  expect_true(is.na(pakhom:::.set_krippendorff_alpha(list(c("a")), list(c("a")))))
  # Two units with identical sets is well-defined (perfect agreement).
  expect_equal(pakhom:::.set_krippendorff_alpha(list(c("a"), c("b")),
                                                list(c("a"), c("b"))), 1.0)
})

test_that("verify_quotes flags a missing-corpus source as drifted WITH a reason", {
  q <- make_quote("doc1", "data_entry", "hello world", 0L, 5L, "hello")
  out <- verify_quotes(list(q), corpus_lookup = list())  # doc1 absent
  expect_equal(out[[1]]$verification_status, "drifted")
  expect_equal(out[[1]]$verification_failure_reason, "source_missing_from_corpus")
})

test_that("a fabricated quote on the no-provider path keeps the real (step-3) failure reason", {
  # Quote text that is NOT in the source -> fails steps 1-3; with no embedding
  # provider, step 4 is skipped but must NOT overwrite the real reason.
  q <- make_quote("doc1", "data_entry", "the actual source text", 0L, 5L,
                  "totally absent phrase")
  out <- verify_quote(q, "the actual source text", provider = NULL)
  expect_equal(out$verification_status, "fabricated")
  expect_equal(out$verification_failure_reason, "step3_substring_not_found")
})
