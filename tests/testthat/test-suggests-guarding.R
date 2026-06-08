# Regression tests: Suggests-only packages (withr, xml2) must be GUARDED so
# their absence degrades gracefully instead of crashing at runtime. Also pins
# the .with_seed() contract: reproducible AND non-perturbing to the global RNG,
# on both the withr branch and the withr-absent fallback.

# ---- .with_seed: determinism + RNG-restore (withr present) ------------------

test_that(".with_seed is deterministic for a fixed seed", {
  a <- pakhom:::.with_seed(123L, sample(1000L, 5L))
  b <- pakhom:::.with_seed(123L, sample(1000L, 5L))
  expect_identical(a, b)
})

test_that(".with_seed does not perturb the caller's global RNG stream", {
  set.seed(7L)
  before <- runif(3L)
  set.seed(7L)
  invisible(pakhom:::.with_seed(999L, sample(50L, 10L)))
  after <- runif(3L)
  expect_identical(before, after)
})

# ---- .with_seed: fallback path when withr (Suggests) is absent --------------

test_that(".with_seed fallback works + restores RNG when withr is absent", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) package != "withr",
    .package = "base"
  )
  # Still deterministic on the manual save/set/restore fallback path.
  a <- pakhom:::.with_seed(42L, sample(1000L, 5L))
  b <- pakhom:::.with_seed(42L, sample(1000L, 5L))
  expect_identical(a, b)
  # Still leaves the caller's global RNG stream untouched.
  set.seed(7L); before <- runif(3L)
  set.seed(7L); invisible(pakhom:::.with_seed(5L, sample(50L, 10L))); after <- runif(3L)
  expect_identical(before, after)
})

# ---- create_theme_network: withr absent must NOT crash (default pipeline) ---

test_that("create_theme_network does not crash when withr is absent (igraph present)", {
  skip_if_not_installed("igraph")
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) package != "withr",
    .package = "base"
  )
  ts <- create_theme_set(list(
    list(id = 1, name = "Alpha", description = "a", codes_included = "c1"),
    list(id = 2, name = "Beta",  description = "b", codes_included = "c2")
  ))
  data <- tibble::tibble(
    std_id = paste0("e", 1:6),
    theme_membership_Alpha = c(1L, 1L, 1L, 0L, 1L, 0L),
    theme_membership_Beta  = c(1L, 0L, 1L, 1L, 0L, 1L)
  )
  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)
  # The layout step (.with_seed -> igraph::layout_with_fr) is the previously
  # unguarded withr crash site; under withr-absent it must take the fallback.
  expect_no_error(
    create_theme_network(data, ts, output_path = out, min_cooccurrence = 1)
  )
})

# ---- xml2 absent: docx + qdpx parsers degrade to NULL, not a crash ---------

test_that(".extract_docx_text returns NULL (not a crash) when xml2 is absent", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) package != "xml2",
    .package = "base"
  )
  tmp <- tempfile(fileext = ".docx")
  file.create(tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_null(pakhom:::.extract_docx_text(tmp))
})

test_that(".parse_qdpx_deep returns NULL (not a crash) when xml2 is absent", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) package != "xml2",
    .package = "base"
  )
  tmp <- tempfile(fileext = ".qdpx")
  file.create(tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_null(pakhom:::.parse_qdpx_deep(tmp))
})
