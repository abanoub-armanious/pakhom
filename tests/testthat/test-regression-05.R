# Regression tests for the Batch-6 security fixes (audit 2026-06-11).

test_that(".md_cell escapes HTML and pipe characters for a markdown table cell", {
  # HTML special chars are entity-escaped (so AI/user text cannot inject HTML
  # once pandoc renders the table)...
  expect_equal(pakhom:::.md_cell("<script>alert(1)</script>"),
               "&lt;script&gt;alert(1)&lt;/script&gt;")
  # ...and a literal pipe is escaped so it cannot split the row into columns.
  expect_equal(pakhom:::.md_cell("a|b"), "a\\|b")
  expect_equal(pakhom:::.md_cell("joy | <b>x</b>"), "joy \\| &lt;b&gt;x&lt;/b&gt;")
  # Newlines collapse so a multi-line value cannot break the single-row cell.
  expect_false(grepl("\n", pakhom:::.md_cell("line1\nline2")))
  # NA / NULL render as empty, never the string "NA".
  expect_equal(pakhom:::.md_cell(NA), "")
  expect_equal(pakhom:::.md_cell(NULL), "")
})
