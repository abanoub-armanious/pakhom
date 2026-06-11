# Regression tests for the Batch-4 temporal/longitudinal fix (audit 2026-06-11).

test_that(".assign_period_labels gives NA timestamps an NA period (no fabricated earliest bucket)", {
  ts <- as.Date(c("2024-01-10", NA, "2024-06-15", NA))
  for (pt in c("daily", "weekly", "monthly", "quarterly")) {
    labs <- pakhom:::.assign_period_labels(ts, pt)
    # NA timestamps -> NA period (not "NA-WNA" / "00NA-Q..").
    expect_true(is.na(labs[2]), info = pt)
    expect_true(is.na(labs[4]), info = pt)
    # No real label contains the literal "NA".
    expect_false(any(grepl("NA", labs[!is.na(labs)])), info = pt)
    # sort(unique()) drops NA -> exactly the two real periods survive, and
    # neither junk bucket sorts to the front.
    periods <- sort(unique(labs))
    expect_equal(length(periods), 2L, info = pt)
  }
})
