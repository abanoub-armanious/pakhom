# Regression tests for the Batch-9 reproducibility fixes (audit 2026-06-11).

test_that(".with_seed does not leak the caller's RNG state and is deterministic", {
  set.seed(99)
  before <- get(".Random.seed", envir = globalenv())
  v1 <- pakhom:::.with_seed(42, sample(1000, 5))
  after <- get(".Random.seed", envir = globalenv())
  expect_identical(before, after)          # caller's global stream untouched
  v2 <- pakhom:::.with_seed(42, sample(1000, 5))
  expect_identical(v1, v2)                  # same seed -> same draw
})

test_that(".with_seed pins the sample kind so a seed is portable across RNGkind settings", {
  old <- RNGkind()
  on.exit(RNGkind(old[1], old[2], old[3]), add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  ref <- pakhom:::.with_seed(7, sample(10000, 10))
  # A caller who globally selected the legacy "Rounding" sample kind still gets
  # the SAME draw, because .with_seed pins the kind internally.
  suppressWarnings(RNGkind("Mersenne-Twister", "Inversion", "Rounding"))
  alt <- pakhom:::.with_seed(7, sample(10000, 10))
  expect_identical(ref, alt)
})
