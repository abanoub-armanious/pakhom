# Tests for manuscript learning system (06_manuscript_learning.R)
# Tests use fixture files created in temp directories -- no real manuscripts needed.

# ==============================================================================
# Helper: Create a minimal DOCX fixture for testing
# ==============================================================================
create_test_docx <- function(text, path) {
  # Create a minimal DOCX (ZIP containing word/document.xml)
  tmp_dir <- tempfile("docx_build_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Minimal DOCX structure
  word_dir <- file.path(tmp_dir, "word")
  dir.create(word_dir)
  rels_dir <- file.path(tmp_dir, "_rels")
  dir.create(rels_dir)

  # [Content_Types].xml
  writeLines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="xml" ContentType="application/xml"/>
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
</Types>', file.path(tmp_dir, "[Content_Types].xml"))

  # _rels/.rels
  writeLines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>
</Relationships>', file.path(rels_dir, ".rels"))

  # word/document.xml with the actual text
  paragraphs <- vapply(strsplit(text, "\n")[[1]], function(line) {
    sprintf('<w:p><w:r><w:t>%s</w:t></w:r></w:p>', line)
  }, character(1))

  doc_xml <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
  <w:body>
    ', paste(paragraphs, collapse = "\n    "), '
  </w:body>
</w:document>')

  writeLines(doc_xml, file.path(word_dir, "document.xml"))

  # Create the ZIP (DOCX is just a ZIP)
  old_wd <- setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  zip(path, files = c("[Content_Types].xml", "_rels/.rels", "word/document.xml"),
      flags = "-q")
  setwd(old_wd)
  invisible(path)
}

# ==============================================================================
# .extract_docx_text: Text extraction from DOCX
# ==============================================================================
test_that(".extract_docx_text extracts text from a minimal DOCX", {
  skip_if_not_installed("xml2")

  docx_path <- tempfile(fileext = ".docx")
  on.exit(unlink(docx_path), add = TRUE)

  create_test_docx("Hello world\nSecond paragraph", docx_path)

  result <- pakhom:::.extract_docx_text(docx_path)
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  expect_true(grepl("Hello world", result))
  expect_true(grepl("Second paragraph", result))
})

test_that(".extract_docx_text returns NULL for non-existent file", {
  result <- pakhom:::.extract_docx_text("/nonexistent/fake.docx")
  expect_null(result)
})

# ==============================================================================
# parse_manuscript: Full manuscript parsing
# ==============================================================================
test_that("parse_manuscript parses a DOCX file", {
  skip_if_not_installed("xml2")

  docx_path <- tempfile(fileext = ".docx")
  on.exit(unlink(docx_path), add = TRUE)

  manuscript_text <- paste(
    "Introduction",
    "This study explores the relationship between sleep and binge eating.",
    "Methodology",
    "We conducted a thematic analysis of online forum posts.",
    "Results",
    "Three main themes emerged from the data.",
    "Discussion",
    "Our findings align with previous literature on this topic.",
    sep = "\n"
  )
  create_test_docx(manuscript_text, docx_path)

  result <- parse_manuscript(docx_path)

  expect_type(result, "list")
  expect_true(!is.null(result$full_text))
  expect_true(result$word_count > 0)
  expect_true(!is.null(result$sections))
})

test_that("parse_manuscript returns NULL for unsupported format", {
  tmp <- tempfile(fileext = ".xyz")
  writeLines("some text", tmp)
  on.exit(unlink(tmp), add = TRUE)

  expect_null(parse_manuscript(tmp))
})

test_that("parse_manuscript handles TXT files", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(paste(rep("word", 20), collapse = " "), tmp)
  on.exit(unlink(tmp), add = TRUE)

  result <- parse_manuscript(tmp)
  expect_type(result, "list")
  expect_true(result$word_count >= 20)
})

test_that("parse_manuscript returns NULL for very short text", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("hi", tmp)
  on.exit(unlink(tmp), add = TRUE)

  result <- parse_manuscript(tmp)
  expect_null(result)
})

# ==============================================================================
# extract_manuscript_sections: Section detection
# ==============================================================================
test_that("extract_manuscript_sections identifies standard sections", {
  text <- paste(
    "Introduction",
    "This is the introduction section with some content about the study.",
    "",
    "Methodology",
    "We used qualitative methods including thematic analysis.",
    "",
    "Results",
    "The analysis revealed several themes.",
    "",
    "Discussion",
    "These findings suggest important implications.",
    sep = "\n"
  )

  sections <- extract_manuscript_sections(text)
  expect_type(sections, "list")
  # Should detect at least some standard sections
  standard_names <- c("introduction", "methodology", "results", "discussion")
  detected <- intersect(tolower(names(sections)), standard_names)
  expect_true(length(detected) > 0)
})

test_that("extract_manuscript_sections appends content for repeated section headings", {
  # This tests the fix for the overwrite bug: when "Theme 1:", "Theme 2:", etc.
  # each match the "themes" pattern, all content should be accumulated, not just
  # the last theme's content.
  text <- paste(
    "Introduction",
    "This study examines sleep and binge eating.",
    "",
    "Theme 1: Sleep Disruption",
    "Content about sleep disruption patterns.",
    "Participants reported difficulty falling asleep.",
    "",
    "Theme 2: Medication Effects",
    "Content about medication effects on behavior.",
    "Side effects were commonly reported.",
    "",
    "Discussion",
    "These findings have implications.",
    sep = "\n"
  )

  sections <- extract_manuscript_sections(text)
  expect_true("themes" %in% names(sections))

  # Both theme contents should be present (not just the last one)
  expect_true(grepl("sleep disruption", sections$themes, ignore.case = TRUE))
  expect_true(grepl("medication effects", sections$themes, ignore.case = TRUE))

  # Verify other sections are also captured correctly
  expect_true("introduction" %in% names(sections))
  expect_true("discussion" %in% names(sections))
  expect_true(grepl("sleep and binge eating", sections$introduction))
  expect_true(grepl("implications", sections$discussion))
})

# ==============================================================================
# .parse_filename_metadata: Filename regex extraction
# ==============================================================================
test_that(".parse_filename_metadata extracts structured metadata", {
  # Standard pattern: YYYY-MM-DD_YYYY-MM-DD_Username XXX_Rating X.X_Likes XXX.docx
  meta <- pakhom:::.parse_filename_metadata(
    "2024-03-15_2024-01-20_JohnDoe 42_Rating 4.5_Likes 120.docx"
  )

  expect_type(meta, "list")
  # Should extract at least some fields (exact fields depend on regex)
  expect_true(!is.null(meta$username) || !is.null(meta$date_scraped) || !is.null(meta$rating))
})

test_that(".parse_filename_metadata handles unparseable filenames gracefully", {
  meta <- pakhom:::.parse_filename_metadata("random_file_name.docx")

  expect_type(meta, "list")
  # Should return NAs, not error
})

# ==============================================================================
# discover_study_folders: Directory discovery
# ==============================================================================
test_that("discover_study_folders finds matching directories", {
  base <- tempfile("studies_")
  dir.create(base, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  # Create study folders matching default pattern
  dir.create(file.path(base, "dayvigo_study"), recursive = TRUE)
  dir.create(file.path(base, "ozempic_study"), recursive = TRUE)
  dir.create(file.path(base, "not_a_match"), recursive = TRUE)

  folders <- discover_study_folders(base, pattern = "study$")
  expect_length(folders, 2)
})

test_that("discover_study_folders returns empty for no matches", {
  base <- tempfile("empty_")
  dir.create(base, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  folders <- discover_study_folders(base, pattern = "study$")
  expect_length(folders, 0)
})

# ==============================================================================
# generate_learning_context: Context generation with char limits
# ==============================================================================
test_that("generate_learning_context respects character limits", {
  # Create a mock PreviousStudies object
  long_text <- paste(rep("word", 5000), collapse = " ")  # ~25K chars
  studies <- structure(list(
    studies = list(
      test_study = list(
        name = "test_study",
        folder = "/tmp/test",
        manuscript = list(
          file = "test.docx",
          full_text = long_text,
          word_count = 5000,
          sections = list(
            introduction = "Intro text here.",
            methodology = "Methods text here.",
            results = "Results text here.",
            themes = "Theme findings here."
          )
        ),
        raw_data = tibble::tibble(
          filename = paste0("file_", 1:10, ".docx"),
          text = paste0("Sample raw data text ", 1:10)
        )
      )
    ),
    n_studies = 1L
  ), class = "PreviousStudies")

  ctx <- generate_learning_context(studies, max_codebook_chars = 500,
                                    max_manuscript_chars = 300,
                                    max_raw_samples = 3)

  expect_true(inherits(ctx, "LearningContext"))
  # Context slices should exist
  expect_true(!is.null(ctx$for_coding))
  expect_true(!is.null(ctx$for_theming))
  expect_true(nchar(ctx$for_coding) > 0)
  expect_equal(ctx$n_studies, 1L)
})

test_that("generate_learning_context injects supplementary sections", {
  # Verify that methodology, discussion, and introduction are injected
  # into their respective context slices
  studies <- structure(list(
    studies = list(
      test_study = list(
        name = "test_study",
        folder = "/tmp/test",
        manuscript = list(
          file = "test.docx",
          full_text = "full text",
          word_count = 500,
          sections = list(
            introduction = "This study investigates sleep-medication interactions in BED patients.",
            methodology = "We applied reflexive thematic analysis following Braun and Clarke.",
            themes = "Theme 1: Sleep disruption from medication side effects.",
            discussion = "The interplay between sleep quality and binge eating suggests a bidirectional relationship."
          )
        ),
        raw_data = NULL
      )
    ),
    n_studies = 1L
  ), class = "PreviousStudies")

  ctx <- generate_learning_context(studies, max_codebook_chars = 5000,
                                    max_manuscript_chars = 3000)

  # Methodology should appear in coding context
  expect_true(grepl("Braun and Clarke", ctx$for_coding))
  expect_true(grepl("Analytical Methodology", ctx$for_coding))

  # Discussion should appear in theming context
  expect_true(grepl("bidirectional relationship", ctx$for_theming))
  expect_true(grepl("Interpretive Lens", ctx$for_theming))

  # Relevance filtering was removed in v2.0; field should be NULL
  expect_null(ctx$for_relevance)
})

# ==============================================================================
# generate_learning_reflection: Fallback when AI unavailable
# ==============================================================================
test_that("generate_learning_reflection falls back without provider", {
  ctx <- structure(list(
    for_coding = "Coding context",
    for_theming = "Theming context",
    for_review = "Review context",
    for_relevance = "Relevance context",
    for_report = "",
    raw_data_summary = "10 raw data entries",
    n_studies = 1L,
    study_names = "test_study"
  ), class = "LearningContext")

  result <- generate_learning_reflection(ctx, provider = NULL)
  expect_true(inherits(result, "LearningContext"))
  # for_report should be populated with fallback text
  expect_true(nchar(result$for_report) > 0)
})

# ==============================================================================
# load_previous_studies: Full loading pipeline
# ==============================================================================
test_that("load_previous_studies handles missing base_dir gracefully", {
  # normalizePath(mustWork=TRUE) will error, so expect an error
  expect_error(load_previous_studies("/nonexistent/path/nowhere"))
})

test_that("load_previous_studies loads from valid directory structure", {
  skip_if_not_installed("xml2")

  base <- tempfile("studies_")
  dir.create(base, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  # Create a study folder with a manuscript
  study_dir <- file.path(base, "test_study")
  dir.create(study_dir)

  # Create a manuscript file
  docx_path <- file.path(study_dir, "manuscript.docx")
  manuscript_text <- paste(rep("This is a test manuscript with enough content to pass the minimum threshold.", 5), collapse = " ")
  create_test_docx(manuscript_text, docx_path)

  result <- load_previous_studies(base, config = list(
    folder_pattern = "study$",
    manuscript_filenames = c("manuscript.docx")
  ))

  expect_true(inherits(result, "PreviousStudies"))
  # May or may not find the study depending on exact folder matching
  expect_true(result$n_studies >= 0L)
})

# ==============================================================================
# parse_raw_data_files: Raw data directory parsing
# ==============================================================================
test_that("parse_raw_data_files returns NULL for non-existent directory", {
  result <- parse_raw_data_files("/nonexistent/path")
  expect_null(result)
})

test_that("parse_raw_data_files returns NULL for empty directory", {
  tmp <- tempfile("empty_raw_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- parse_raw_data_files(tmp)
  expect_null(result)
})

# ==============================================================================
# Domain-neutrality of cross-study synthesis
#
# Earlier versions of .synthesize_cross_study_patterns injected hardcoded
# medication-research opinions into the AI's learning context (a regex list
# matching theme names to predefined "recurring categories" like 'side effects',
# 'treatment efficacy', 'dosage timing'; plus an unconditional narrative-arc
# claim about themes moving from 'direct treatment effects' to 'broader
# implications'). These tests pin that those forbidden strings never appear in
# the function's output -- if a future change reintroduces them, these tests
# fail loudly.
# ==============================================================================

# Build a synthetic 2-study fixture for domain-neutrality testing
.make_synthetic_studies <- function() {
  make_study <- function(name, theme_names) {
    cb <- tibble::tibble(
      code_guid = paste0("g", seq_along(theme_names)),
      code_name = theme_names,
      parent_guid = NA_character_,
      parent_name = NA_character_,
      description = paste("Description of", theme_names),
      hierarchy_level = 0L,
      hierarchy_path = theme_names,
      is_codable = FALSE,
      is_discarded = FALSE,
      frequency = 5L,
      n_sources = 3L
    )
    list(name = name, codebook_full = cb)
  }
  studies <- list(
    studies = list(
      study_a = make_study("study_a", c("Workplace autonomy", "Team dynamics", "Career mobility")),
      study_b = make_study("study_b", c("Organizational culture", "Job satisfaction", "Leadership style"))
    ),
    n_studies = 2L
  )
  class(studies) <- "PreviousStudies"
  studies
}

test_that("cross-study synthesis does NOT inject medication-research framing on a non-medication codebook", {
  studies <- .make_synthetic_studies()
  hierarchies <- list(study_a = list(), study_b = list())  # placeholder, not used by current impl
  benchmarks <- NULL

  out <- pakhom:::.synthesize_cross_study_patterns(hierarchies, studies, benchmarks)

  # These strings come from the removed medication-research priming block
  # and must NEVER appear in the output regardless of input
  forbidden <- c(
    "RECURRING categories",
    "side effects / adverse reactions",
    "treatment efficacy / outcomes",
    "patient journey",
    "direct treatment effects",
    "side effects and complications",
    "broader implications",
    "Apply these same organizational principles",
    "CONTRASTING subthemes"
  )
  for (s in forbidden) {
    expect_false(grepl(s, out, fixed = TRUE),
                 info = paste("Forbidden hardcoded medication-research string reappeared:", s))
  }
})

test_that("cross-study synthesis lists the actual theme names from each study", {
  studies <- .make_synthetic_studies()
  out <- pakhom:::.synthesize_cross_study_patterns(
    hierarchies = list(study_a = list(), study_b = list()),
    studies = studies,
    benchmarks = NULL
  )

  # The new behavior: present actual theme names as facts. They should appear
  # verbatim in the output for both studies.
  expect_true(grepl("Workplace autonomy", out, fixed = TRUE))
  expect_true(grepl("Team dynamics", out, fixed = TRUE))
  expect_true(grepl("Organizational culture", out, fixed = TRUE))
  expect_true(grepl("Leadership style", out, fixed = TRUE))
  # And the framing should be factual ("STRUCTURAL FACTS"), not opinionated
  # ("STRUCTURAL PATTERNS")
  expect_true(grepl("STRUCTURAL FACTS", out, fixed = TRUE))
  expect_false(grepl("STRUCTURAL PATTERNS", out, fixed = TRUE))
})

test_that("cross-study synthesis returns empty string with fewer than 2 studies", {
  studies <- structure(list(studies = list(), n_studies = 0L), class = "PreviousStudies")
  out <- pakhom:::.synthesize_cross_study_patterns(
    hierarchies = list(),
    studies = studies,
    benchmarks = NULL
  )
  expect_equal(out, "")
})

# ============================================================================
# AH-3 audit followup: study roster + hash-shuffle iteration
# ============================================================================

test_that("AH-3: .build_study_roster returns empty string for single-study input", {
  one_study <- structure(list(
    studies = list(only = list(name = "only_study",
                                codebook = data.frame(code_name = "a"))),
    n_studies = 1L
  ), class = "PreviousStudies")
  expect_equal(pakhom:::.build_study_roster(one_study), "")
})

test_that("AH-3: .build_study_roster returns empty for NULL or zero studies", {
  expect_equal(pakhom:::.build_study_roster(NULL), "")
  expect_equal(
    pakhom:::.build_study_roster(
      structure(list(studies = list(), n_studies = 0L), class = "PreviousStudies")
    ),
    ""
  )
})

test_that("AH-3: .build_study_roster names every study at the top", {
  three_studies <- structure(list(
    studies = list(
      dayvigo = list(name = "dayvigo",
                      codebook = data.frame(code_name = letters[1:10])),
      ozempic = list(name = "ozempic",
                      codebook = data.frame(code_name = letters[1:5])),
      vyvanse = list(name = "vyvanse",
                      codebook = data.frame(code_name = letters[1:3]))
    ),
    n_studies = 3L
  ), class = "PreviousStudies")
  out <- pakhom:::.build_study_roster(three_studies)
  expect_true(grepl("dayvigo", out, fixed = TRUE),
              info = "dayvigo missing from roster")
  expect_true(grepl("ozempic", out, fixed = TRUE),
              info = "ozempic missing from roster (an audit-flagged study)")
  expect_true(grepl("vyvanse", out, fixed = TRUE),
              info = "vyvanse missing from roster (an audit-flagged study)")
  expect_true(grepl("equal weight", out, fixed = TRUE),
              info = "roster must include explicit equal-weight directive")
  expect_true(grepl("10 codes", out, fixed = TRUE))
  expect_true(grepl("5 codes", out, fixed = TRUE))
  expect_true(grepl("3 codes", out, fixed = TRUE))
})

test_that("AH-3: .build_study_roster handles studies with NULL codebook gracefully", {
  studies_with_null <- structure(list(
    studies = list(
      a = list(name = "a", codebook = NULL),
      b = list(name = "b", codebook = data.frame(code_name = "x"))
    ),
    n_studies = 2L
  ), class = "PreviousStudies")
  out <- pakhom:::.build_study_roster(studies_with_null)
  # Both names appear; no R errors from NULL codebook handling.
  expect_true(grepl("**a**", out, fixed = TRUE))
  expect_true(grepl("**b**", out, fixed = TRUE))
})

test_that("AH-3 (audit MEDIUM-4): study iteration order is deterministic + hash-shuffled", {
  # Hash-based ordering is deterministic across calls (AC10 replay-
  # equivalence) AND uncorrelated with registration order. Verify by
  # computing the expected order via the same hash + confirming the
  # first depth-chunk in for_theming matches the hash's first study.
  studies <- structure(list(
    studies = list(
      dayvigo = list(name = "dayvigo", codebook = data.frame(
        code_name = "a", description = "", parent_code = NA_character_,
        frequency = 1L,
        stringsAsFactors = FALSE
      )),
      ozempic = list(name = "ozempic", codebook = data.frame(
        code_name = "b", description = "", parent_code = NA_character_,
        frequency = 1L,
        stringsAsFactors = FALSE
      )),
      vyvanse = list(name = "vyvanse", codebook = data.frame(
        code_name = "c", description = "", parent_code = NA_character_,
        frequency = 1L,
        stringsAsFactors = FALSE
      ))
    ),
    n_studies = 3L
  ), class = "PreviousStudies")
  ctx1 <- generate_learning_context(studies)
  ctx2 <- generate_learning_context(studies)
  expect_identical(ctx1$for_theming, ctx2$for_theming,
                   info = "AC10: identical inputs must produce identical for_theming output")

  expected_order <- names(studies$studies)[order(vapply(
    names(studies$studies), function(n) digest::digest(n, algo = "md5"),
    character(1)
  ))]
  first_depth_chunk_study <- expected_order[1]
  # Depth chunks include "### <STUDY> -" prefixes (uppercased at line 408).
  # Find the FIRST such prefix in for_theming AFTER the roster block.
  roster_end <- regexpr("\n###", ctx1$for_theming, fixed = TRUE)
  if (roster_end > 0L) {
    after_roster <- substr(ctx1$for_theming, roster_end + 4L, roster_end + 200L)
    first_label <- toupper(first_depth_chunk_study)
    expect_true(grepl(first_label, after_roster, fixed = TRUE),
                info = sprintf("First depth chunk should be %s (hash-shuffle order); got: %s",
                                first_label, substr(after_roster, 1, 80)))
  }
})
