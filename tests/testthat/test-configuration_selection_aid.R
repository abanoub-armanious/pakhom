# ==============================================================================
# configuration_selection_aid() tests
# ==============================================================================
# Verifies the configuration aid returns sensible per-mode + per-scale guidance,
# matching the empirical evidence from the configuration deep-dive.
# ==============================================================================

test_that("Mode 2 narrow_intersection returns 5-8 theme range for 250-entry corpus", {
  # An early calibration run was exactly this configuration: 250 entries, narrow
  # intersection focus (medication x sleep x binge eating). It produced
  # 6 themes from 40 codes. The aid's bracket should cover that empirical
  # data point.
  rec <- configuration_selection_aid(
    mode = "codebook_collaborative",
    corpus_size = 250L,
    focus_shape = "narrow_intersection"
  )
  expect_equal(rec$expected_themes, c(5L, 8L))
  expect_true(6L >= rec$expected_themes[1] && 6L <= rec$expected_themes[2],
              info = "actual (6 themes) must be inside the recommendation")
  expect_equal(rec$expected_passes, 1L)  # 40-50 codes -> 1 pass
  expect_true(rec$recommended_review_points$after_coding,
              info = "250-entry corpus is large enough for after_coding review")
})

test_that("Mode 2 broad focus large corpus matches empirical evidence", {
  # An early calibration run: 250-entry corpus, broad emotional-triggers focus,
  # produced 157 codes -> 7 themes in 3 substantive passes. The aid should
  # bracket that outcome.
  rec <- configuration_selection_aid(
    mode = "codebook_collaborative",
    corpus_size = 250L,
    focus_shape = "broad"
  )
  expect_equal(rec$expected_themes, c(6L, 10L))
  expect_true(7L >= rec$expected_themes[1] && 7L <= rec$expected_themes[2],
              info = "actual (7 themes) must be inside the recommendation")
  expect_equal(rec$expected_passes, 3L)  # 157 codes -> 3 passes
})

test_that("explicit estimated_codebook_size overrides the corpus-size estimate", {
  rec_implicit <- configuration_selection_aid(
    mode = "codebook_collaborative",
    corpus_size = 250L,
    focus_shape = "narrow_intersection"
  )
  rec_explicit <- configuration_selection_aid(
    mode = "codebook_collaborative",
    corpus_size = 250L,
    estimated_codebook_size = 200L,
    focus_shape = "narrow_intersection"
  )
  # Larger codebook should bump expected_passes up
  expect_gt(rec_explicit$expected_passes, rec_implicit$expected_passes)
})

test_that("Mode 1 returns NA theme prediction + run_mode1 note", {
  rec <- configuration_selection_aid(
    mode = "reflexive_scaffold",
    corpus_size = 100L,
    focus_shape = "narrow_intersection"
  )
  expect_true(is.na(rec$expected_themes))
  expect_true(is.na(rec$expected_passes))
  expect_match(rec$notes, "Mode 1|reflexive_scaffold|provocateur|run_mode1")
  expect_false(rec$recommended_review_points$after_coding)
  expect_false(rec$recommended_review_points$after_themes)
})

test_that("Mode 3 returns NA theme prediction + framework note", {
  rec <- configuration_selection_aid(
    mode = "framework_applied",
    corpus_size = 250L,
    focus_shape = "single_focal"
  )
  expect_true(is.na(rec$expected_themes))
  expect_true(is.na(rec$expected_passes))
  expect_match(rec$notes, "Mode 3|framework|anomaly_handling")
})

test_that("out-of-bracket codebooks emit smoke-first guidance", {
  # Very small
  rec_small <- configuration_selection_aid(
    mode = "codebook_collaborative",
    corpus_size = 50L,
    estimated_codebook_size = 20L,
    focus_shape = "single_focal"
  )
  expect_match(rec_small$notes, "below.*bracket|smoke")

  # Very large
  rec_large <- configuration_selection_aid(
    mode = "codebook_collaborative",
    corpus_size = 5000L,
    estimated_codebook_size = 500L,
    focus_shape = "broad"
  )
  expect_match(rec_large$notes, "above.*bracket|smoke")
})

test_that("wall-time + API-spend estimates scale with corpus size", {
  rec_small <- configuration_selection_aid(
    mode = "codebook_collaborative",
    corpus_size = 50L,
    focus_shape = "single_focal"
  )
  rec_large <- configuration_selection_aid(
    mode = "codebook_collaborative",
    corpus_size = 500L,
    focus_shape = "single_focal"
  )
  expect_gt(rec_large$expected_wall_time_min, rec_small$expected_wall_time_min)
  expect_gt(rec_large$expected_api_spend_usd, rec_small$expected_api_spend_usd)
})

test_that("invalid corpus_size errors clearly", {
  expect_error(
    configuration_selection_aid("codebook_collaborative", corpus_size = -5),
    regexp = "corpus_size must be a positive integer"
  )
  expect_error(
    configuration_selection_aid("codebook_collaborative", corpus_size = "many"),
    regexp = "corpus_size"
  )
})

test_that("invalid mode / focus_shape errors clearly", {
  expect_error(
    configuration_selection_aid("nonexistent_mode", corpus_size = 100L),
    regexp = "should be one of"
  )
  expect_error(
    configuration_selection_aid("codebook_collaborative", corpus_size = 100L,
                                 focus_shape = "invalid_shape"),
    regexp = "should be one of"
  )
})

test_that("review-point recommendation adapts to corpus size + codebook size", {
  # Small corpus, small codebook -> after_themes off (no expected overlap pairs)
  rec_small <- configuration_selection_aid(
    mode = "codebook_collaborative",
    corpus_size = 50L,
    estimated_codebook_size = 30L,
    focus_shape = "narrow_intersection"
  )
  expect_false(rec_small$recommended_review_points$after_coding,
               info = "50-entry corpus does not warrant after_coding pause")

  # Large corpus, mid-sized codebook -> after_coding on
  rec_large <- configuration_selection_aid(
    mode = "codebook_collaborative",
    corpus_size = 500L,
    estimated_codebook_size = 80L,
    focus_shape = "single_focal"
  )
  expect_true(rec_large$recommended_review_points$after_coding,
              info = "500-entry corpus warrants after_coding pause")
})
