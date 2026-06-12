# Route pakhom's logger output to a no-op appender for the duration of the test
# suite. See .pakhom_test_silent_appender in helper.R for the full rationale:
# many tests intentionally drive log_error()/log_warn()/log_info() code paths
# and PASS, but logger's default console appender writes them to stderr, where
# GitHub Actions turns them into ##[error]/##[warning]/##[notice] annotations --
# so a clean run (R CMD check Status: OK, testthat FAIL 0) appears to carry
# dozens of "errors". Silencing the console appender during tests keeps the CI
# summary truthful; package behaviour for real users is unchanged (they keep the
# normal console logging, which the package sets up itself).
if (requireNamespace("logger", quietly = TRUE)) {
  logger::log_appender(.pakhom_test_silent_appender)
}
