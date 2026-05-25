# Phase 59 Stage 3: qdpx_export coverage
#
# covr revealed `R/qdpx_export.R` had 0% test coverage despite being
# 235 lines and recently modified for the meta-audit M4 UTC sweep
# (.qdpx_guid + creationDateTime). This file closes that gap with
# focused tests that exercise the constructor + GUID + XML build + zip
# packaging paths.

# Helper: build a minimal valid ProgressiveCodingState + data fixture.
.qdpx_minimal_fixture <- function() {
  data <- data.frame(
    std_id     = c("e1", "e2"),
    std_text   = c("First entry text about exercise routines.",
                   "Second entry text about diet patterns."),
    std_source = c("post", "comment"),
    std_author = c("user_a", "user_b"),
    std_date   = as.POSIXct(c("2026-01-01 10:00:00", "2026-01-02 12:00:00"),
                            tz = "UTC"),
    stringsAsFactors = FALSE
  )

  state <- create_coding_state()
  prov <- make_quote(
    source_doc_id      = "e1",
    source_doc_type    = "post",
    source_text        = data$std_text[1],
    start_char         = 14L,
    end_char           = 31L,
    exact_text         = "exercise routines",
    citation_source    = "pipeline_derived"
  )
  state$codebook[["routines"]] <- list(
    code_key      = "routines",
    code_name     = "Routines",
    description   = "Daily routines mentioned by participants.",
    type          = "descriptive",
    frequency     = 1L,
    entry_ids     = "e1",
    coded_segments = list(
      list(entry_id = "e1", text = "exercise routines",
           start_char = 14L, end_char = 31L,
           provenance = prov)
    )
  )
  state$entry_results[["e1"]] <- list(
    codes_assigned = "routines",
    coded_segments = list(list(
      code_key = "routines", code_name = "Routines",
      text = "exercise routines", start_char = 14L, end_char = 31L
    ))
  )
  state$entries_processed <- c(state$entries_processed, "e1")
  state$entries_skipped   <- c(state$entries_skipped,   "e2")
  state$entry_results[["e2"]] <- list(
    codes_assigned = character(0),
    coded_segments = list(),
    skipped = TRUE
  )

  list(state = state, data = data)
}

# ==========================================================================
# .qdpx_guid: UTC + uniqueness + tag suffix
# ==========================================================================

test_that(".qdpx_guid emits UTC YYYYMMDDHHMMSS prefix (meta-audit M4)", {
  set.seed(42L)
  g <- pakhom:::.qdpx_guid()
  # Format: TA-YYYYMMDDHHMMSS-NNNNNN
  expect_match(g, "^TA-[0-9]{14}-[0-9]{6}$", perl = TRUE)
})

test_that(".qdpx_guid appends sanitised tag suffix when given", {
  g <- pakhom:::.qdpx_guid("project")
  expect_match(g, "-project$")
  # Invalid chars stripped
  g2 <- pakhom:::.qdpx_guid("hello/world! 123")
  expect_match(g2, "-helloworld123$")
})

test_that(".qdpx_guid produces unique IDs across rapid calls", {
  ids <- replicate(20L, pakhom:::.qdpx_guid())
  expect_length(unique(ids), 20L)
})

# ==========================================================================
# export_qdpx: input validation
# ==========================================================================

test_that("export_qdpx rejects non-ProgressiveCodingState coding_state", {
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  expect_error(
    export_qdpx(coding_state = list(), data = fx$data, output_path = out),
    "ProgressiveCodingState"
  )
})

test_that("export_qdpx rejects malformed data frame", {
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  expect_error(
    export_qdpx(coding_state = fx$state,
                data = data.frame(x = 1, y = 2),
                output_path = out),
    "std_id"
  )
})

test_that("export_qdpx rejects empty output_path", {
  fx <- .qdpx_minimal_fixture()
  expect_error(
    export_qdpx(coding_state = fx$state, data = fx$data, output_path = ""),
    "output_path"
  )
})

test_that("export_qdpx rejects non-ThemeSet theme_set", {
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  expect_error(
    export_qdpx(coding_state = fx$state, data = fx$data, output_path = out,
                theme_set = list(themes = list())),
    "ThemeSet"
  )
})

test_that("export_qdpx auto-appends .qdpx extension", {
  skip_if_not_installed("xml2")
  fx <- .qdpx_minimal_fixture()
  out_noext <- withr::local_tempfile()
  result <- export_qdpx(coding_state = fx$state, data = fx$data,
                         output_path = out_noext)
  expect_match(result, "\\.qdpx$")
  expect_true(file.exists(result))
})

# ==========================================================================
# export_qdpx: end-to-end (creates the zip, contains required parts)
# ==========================================================================

test_that("export_qdpx produces a valid .qdpx zip with project.qde + Sources/", {
  skip_if_not_installed("xml2")
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  result <- export_qdpx(coding_state = fx$state, data = fx$data,
                         output_path = out, study_name = "Test Study")
  expect_true(file.exists(result))
  # QDPX is a zip containing project.qde + Sources/<files>
  entries <- utils::unzip(result, list = TRUE)$Name
  expect_true("project.qde" %in% entries)
  # QDPX spec uses "sources/" (lowercase) inside the .qdpx archive.
  expect_true(any(grepl("^sources/", entries)))
})

test_that("QDPX project.qde XML carries UTC creationDateTime (meta-audit M4)", {
  skip_if_not_installed("xml2")
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  export_qdpx(coding_state = fx$state, data = fx$data, output_path = out)
  # Unpack and inspect the XML
  tmp_dir <- withr::local_tempdir()
  utils::unzip(out, files = "project.qde", exdir = tmp_dir)
  doc <- xml2::read_xml(file.path(tmp_dir, "project.qde"))
  cdt <- xml2::xml_attr(doc, "creationDateTime")
  # ISO-8601 with Z suffix (UTC) per the M4 fix
  expect_match(cdt, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
               perl = TRUE)
})

test_that("QDPX project.qde XML contains the study_name + code", {
  skip_if_not_installed("xml2")
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  export_qdpx(coding_state = fx$state, data = fx$data, output_path = out,
              study_name = "Binge Eating Study")
  tmp_dir <- withr::local_tempdir()
  utils::unzip(out, files = "project.qde", exdir = tmp_dir)
  qde_text <- paste(readLines(file.path(tmp_dir, "project.qde"),
                                warn = FALSE),
                     collapse = "\n")
  expect_match(qde_text, "Binge Eating Study", fixed = TRUE)
  expect_match(qde_text, "Routines", fixed = TRUE)
})

test_that("export_qdpx with methodology_mode stamps the Description block", {
  skip_if_not_installed("xml2")
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  export_qdpx(coding_state = fx$state, data = fx$data, output_path = out,
              methodology_mode = "codebook_collaborative")
  tmp_dir <- withr::local_tempdir()
  utils::unzip(out, files = "project.qde", exdir = tmp_dir)
  qde_text <- paste(readLines(file.path(tmp_dir, "project.qde"),
                                warn = FALSE),
                     collapse = "\n")
  # Description block surfaces methodology mode (via methodology_label()
  # translation: "codebook_collaborative" -> "M2 - Codebook Collaborative")
  expect_match(qde_text, "Codebook Collaborative", fixed = TRUE)
})

test_that("export_qdpx with empty codebook still produces a valid sources-only zip", {
  skip_if_not_installed("xml2")
  state <- create_coding_state()  # empty codebook
  data <- data.frame(
    std_id = "e1", std_text = "Some text.",
    std_source = "post", std_author = "u", std_date = Sys.time(),
    stringsAsFactors = FALSE
  )
  out <- withr::local_tempfile(fileext = ".qdpx")
  result <- export_qdpx(coding_state = state, data = data, output_path = out)
  expect_true(file.exists(result))
  entries <- utils::unzip(result, list = TRUE)$Name
  expect_true("project.qde" %in% entries)
})
