# Regression test for the Batch-13 QDPX offset-accuracy fix (audit 2026-06-11).

test_that("export_qdpx recomputes a WRONG model offset to the true text position", {
  skip_if_not_installed("xml2")
  data <- data.frame(
    std_id = "e1",
    std_text = "I really value my exercise routines every day.",
    stringsAsFactors = FALSE
  )
  true_start <- as.integer(regexpr("exercise routines", data$std_text,
                                   fixed = TRUE)[1] - 1L)   # 0-based
  true_end <- true_start + nchar("exercise routines")

  state <- create_coding_state()
  state$codebook[["routines"]] <- list(
    code_key = "routines", code_name = "Routines", description = "d",
    type = "descriptive", frequency = 1L, entry_ids = "e1",
    coded_segments = list(list(entry_id = "e1", text = "exercise routines",
                               start_char = 0L, end_char = 5L))
  )
  # entry_results carries WRONG offsets (0..5 would slice "I rea"); the export
  # must detect the mismatch and recompute from the text.
  state$entry_results[["e1"]] <- list(
    codes_assigned = "routines",
    coded_segments = list(list(code_key = "routines", code_name = "Routines",
                               text = "exercise routines",
                               start_char = 0L, end_char = 5L))
  )
  state$entries_processed <- "e1"

  out <- withr::local_tempfile(fileext = ".qdpx")
  export_qdpx(coding_state = state, data = data, output_path = out)
  tmp_dir <- withr::local_tempdir()
  utils::unzip(out, files = "project.qde", exdir = tmp_dir)
  qde <- paste(readLines(file.path(tmp_dir, "project.qde"), warn = FALSE),
               collapse = "\n")

  expect_match(qde, sprintf('startPosition="%d"', true_start), fixed = TRUE)
  expect_match(qde, sprintf('endPosition="%d"', true_end), fixed = TRUE)
  # The bogus offset must NOT have been emitted verbatim.
  expect_false(grepl('startPosition="0" endPosition="5"', qde, fixed = TRUE))
})
