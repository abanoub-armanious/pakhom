# qdpx_export coverage
#
# Exercises the constructor + GUID + XML build + zip packaging paths,
# plus REFI-QDA Project 1.0 structural conformance (RFC-4122 v4 GUIDs,
# Users block, PlainTextSelection offsets, internal:// GUID source
# paths, ProjectType element order) and an export -> import round-trip
# through the package's own .parse_qdpx_deep reader.

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
    start_char         = 23L,
    end_char           = 40L,
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
      text = "exercise routines", start_char = 23L, end_char = 40L
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
# .qdpx_guid: RFC-4122 v4 (REFI-QDA GUIDType) + uniqueness
# ==========================================================================

test_that(".qdpx_guid emits an RFC-4122 version-4 UUID (REFI-QDA GUIDType)", {
  g <- pakhom:::.qdpx_guid()
  # 8-4-4-4-12 hex, version nibble '4', variant nibble [89ab]
  expect_match(
    g,
    "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$",
    perl = TRUE
  )
})

test_that(".qdpx_guid produces unique IDs across rapid calls", {
  ids <- replicate(200L, pakhom:::.qdpx_guid())
  expect_length(unique(ids), 200L)
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

test_that("QDPX project.qde declares the REFI-QDA namespace on <Project>", {
  skip_if_not_installed("xml2")
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  export_qdpx(coding_state = fx$state, data = fx$data, output_path = out)
  tmp_dir <- withr::local_tempdir()
  utils::unzip(out, files = "project.qde", exdir = tmp_dir)
  qde <- file.path(tmp_dir, "project.qde")
  doc <- xml2::read_xml(qde)
  # The REFI-QDA Project Exchange schema's target namespace. Strict QDA
  # importers (NVivo / ATLAS.ti / MAXQDA) validate against the .xsd and
  # reject a namespace-less <Project>.
  expect_true("urn:QDA-XML:project:1.0" %in% as.character(xml2::xml_ns(doc)))
  raw <- readChar(qde, file.info(qde)$size, useBytes = TRUE)
  expect_true(grepl('xmlns="urn:QDA-XML:project:1.0"', raw, fixed = TRUE))
  # Descendants must INHERIT the default namespace, not be stranded in
  # no-namespace (which libxml2 would mark with an xmlns="" un-declaration).
  expect_false(grepl('xmlns=""', raw, fixed = TRUE))
  # Structure intact: the codebook's Code elements are still locatable.
  # local-name() is namespace-agnostic -- a bare ".//Code" xpath no longer
  # matches now that elements are in the default project namespace.
  expect_gte(length(xml2::xml_find_all(doc, ".//*[local-name()='Code']")), 1L)
})

test_that("QDPX project.qde XML contains the study_name + code", {
  skip_if_not_installed("xml2")
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  export_qdpx(coding_state = fx$state, data = fx$data, output_path = out,
              study_name = "Overwork Study")
  tmp_dir <- withr::local_tempdir()
  utils::unzip(out, files = "project.qde", exdir = tmp_dir)
  qde_text <- paste(readLines(file.path(tmp_dir, "project.qde"),
                                warn = FALSE),
                     collapse = "\n")
  expect_match(qde_text, "Overwork Study", fixed = TRUE)
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

# ==========================================================================
# Adversarial content: XML metacharacters, XML-1.0-illegal control chars, and
# unicode in code names / descriptions / source text must still yield a VALID,
# re-parseable .qde (researchers import this into NVivo/ATLAS.ti/MAXQDA, so a
# malformed export would silently break their workflow).
# ==========================================================================
test_that("export_qdpx survives XML metachars + control chars + unicode", {
  skip_if_not_installed("xml2")
  ctrl <- "\x01"; vtab <- "\x0B"  # both illegal in XML 1.0 text
  bad_text <- paste0("amp & lt< gt> quote\" apos' ctrl", ctrl,
                     " vtab", vtab, " emoji \U0001F600 café.")
  bad_name <- paste0("Focus & \"Wake\"<issues>", ctrl)

  data <- data.frame(
    std_id = c("e1", "e2"),
    std_text = c(bad_text, "plain second entry"),
    std_source = c("post", "comment"),
    std_author = c("u&a", "u<b>"),
    std_date = as.POSIXct(c("2026-01-01 10:00:00", "2026-01-02 12:00:00"), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  state <- create_coding_state()
  prov <- make_quote(source_doc_id = "e1", source_doc_type = "post",
                     source_text = bad_text, start_char = 0L, end_char = 10L,
                     exact_text = substr(bad_text, 1, 10),
                     citation_source = "pipeline_derived")
  state$codebook[["k1"]] <- list(
    code_key = "k1", code_name = bad_name,
    description = paste0("Desc with & < > and ctrl", ctrl, " char."),
    type = "descriptive", frequency = 1L, entry_ids = "e1",
    coded_segments = list(list(entry_id = "e1", text = substr(bad_text, 1, 10),
                               start_char = 0L, end_char = 10L, provenance = prov)))
  state$entry_results[["e1"]] <- list(
    codes_assigned = "k1",
    coded_segments = list(list(code_key = "k1", code_name = bad_name,
      text = substr(bad_text, 1, 10), start_char = 0L, end_char = 10L)))
  state$entries_processed <- "e1"

  out <- withr::local_tempfile(fileext = ".qdpx")
  expect_no_error(export_qdpx(coding_state = state, data = data, output_path = out))
  expect_true(file.exists(out))

  ex <- withr::local_tempdir()
  utils::unzip(out, exdir = ex)
  qde <- list.files(ex, pattern = "\\.qde$", recursive = TRUE, full.names = TRUE)
  expect_length(qde, 1L)

  # The crux: the .qde must be well-formed XML despite the adversarial input.
  doc <- xml2::read_xml(qde[1])
  expect_s3_class(doc, "xml_document")

  # The code name's meaningful tokens survive (xml2 un-escapes on read); the
  # angle-bracketed "<issues>" must NOT have leaked through as a raw element.
  raw <- readChar(qde[1], file.info(qde[1])$size, useBytes = TRUE)
  expect_false(grepl("<issues>", raw, fixed = TRUE))   # escaped, not a live tag
  expect_true(grepl("Focus", raw, fixed = TRUE))

  # Platform-independent guard against the libxml2-strictness split that made
  # this export pass on macOS yet be rejected by Linux/Windows CI. The illegal
  # C0 control characters must be STRIPPED before serialisation -- neither left
  # raw (strict libxml2 errors on read) nor handed to libxml2 to mangle (lenient
  # libxml2 substitutes U+FFFD and emits a "&#xFFFD;" reference, so the local
  # read_xml() passes while a strict parser rejects the same input).
  qde_bytes <- readBin(qde[1], "raw", n = file.info(qde[1])$size)
  expect_false(any(as.integer(qde_bytes) %in% c(1:8, 11, 12, 14:31)),
               info = "exported .qde must contain no XML-illegal control bytes")
  # Decoded attribute values + element text must be free of the illegal chars
  # AND of the U+FFFD replacement that signals an unstripped, mangled char.
  # local-name() so the query still finds Code elements now that they sit in
  # the default REFI-QDA project namespace (a bare ".//Code" matches nothing).
  parsed <- paste(c(xml2::xml_attr(xml2::xml_find_all(doc, ".//*[local-name()='Code']"), "name"),
                    xml2::xml_text(doc)), collapse = "\n")
  ctrl_class <- paste(intToUtf8(c(1:8, 11, 12, 14:31), multiple = TRUE), collapse = "")
  expect_false(grepl(paste0("[", ctrl_class, "]"), parsed),
               info = "no XML-illegal control char survives into the parsed .qde")
  expect_false(grepl(intToUtf8(0xFFFD), parsed, fixed = TRUE),
               info = "illegal chars must be stripped, not substituted with U+FFFD")

  # Source text files are written (the post bodies live in plain-text sources).
  txts <- list.files(ex, pattern = "\\.txt$", recursive = TRUE)
  expect_gte(length(txts), 1L)
})


# ==========================================================================
# REFI-QDA Project 1.0 structural conformance + round-trip
# ==========================================================================

test_that("QDPX project.qde is structurally conformant to REFI-QDA Project 1.0", {
  skip_if_not_installed("xml2")
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  export_qdpx(coding_state = fx$state, data = fx$data, output_path = out)
  tmp_dir <- withr::local_tempdir()
  utils::unzip(out, exdir = tmp_dir)
  doc <- xml2::read_xml(file.path(tmp_dir, "project.qde"))
  guid_re <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

  # (a) every GUID-typed attribute matches GUIDType
  all_nodes <- xml2::xml_find_all(doc, "//*")
  for (attr_name in c("guid", "targetGUID", "creatingUser")) {
    vals <- xml2::xml_attr(all_nodes, attr_name)
    vals <- vals[!is.na(vals)]
    expect_true(length(vals) > 0L,
                info = sprintf("no %s attributes found", attr_name))
    for (v in vals) expect_match(v, guid_re, perl = TRUE)
  }
  expect_match(xml2::xml_attr(doc, "creatingUserGUID"), guid_re, perl = TRUE)
  # the misspelled/miscased legacy attribute must be gone
  expect_true(is.na(xml2::xml_attr(doc, "creatingUserGuid")))

  # (b) no nonexistent <TextSelection>; all element names in the schema set
  local_names <- unique(xml2::xml_name(all_nodes))
  expect_false("TextSelection" %in% local_names)
  allowed <- c("Project", "Users", "User", "CodeBook", "Codes", "Code",
               "Description", "Sources", "TextSource", "PlainTextSelection",
               "Coding", "CodeRef")
  expect_true(all(local_names %in% allowed),
              info = paste("unexpected elements:",
                           paste(setdiff(local_names, allowed), collapse = ", ")))

  # (c) ProjectType sequence: Users < CodeBook < Sources < Description
  child_names <- xml2::xml_name(xml2::xml_children(doc))
  pos <- function(nm) match(nm, child_names)
  expect_lt(pos("Users"), pos("CodeBook"))
  expect_lt(pos("CodeBook"), pos("Sources"))
  expect_lt(pos("Sources"), pos("Description"))

  # (d) PlainTextSelection shape: required attrs, TextSource parent,
  #     Coding > CodeRef resolving to a declared Code guid; creatingUser
  #     and creatingUserGUID resolve to a declared User guid
  sels <- xml2::xml_find_all(doc, ".//*[local-name()=\'PlainTextSelection\']")
  expect_gte(length(sels), 1L)
  code_guids <- xml2::xml_attr(
    xml2::xml_find_all(doc, ".//*[local-name()=\'Code\']"), "guid")
  user_guids <- xml2::xml_attr(
    xml2::xml_find_all(doc, ".//*[local-name()=\'User\']"), "guid")
  expect_true(xml2::xml_attr(doc, "creatingUserGUID") %in% user_guids)
  for (sel in sels) {
    expect_false(is.na(xml2::xml_attr(sel, "guid")))
    expect_false(is.na(xml2::xml_attr(sel, "startPosition")))
    expect_false(is.na(xml2::xml_attr(sel, "endPosition")))
    expect_equal(xml2::xml_name(xml2::xml_parent(sel)), "TextSource")
    codings <- xml2::xml_find_all(sel, "./*[local-name()=\'Coding\']")
    expect_gte(length(codings), 1L)
    for (cd in codings) {
      expect_true(xml2::xml_attr(cd, "creatingUser") %in% user_guids)
      refs <- xml2::xml_find_all(cd, "./*[local-name()=\'CodeRef\']")
      expect_length(refs, 1L)
      expect_true(xml2::xml_attr(refs[[1]], "targetGUID") %in% code_guids)
    }
  }

  # (e) internal:// GUID source paths matching files in the archive
  srcs <- xml2::xml_find_all(doc, ".//*[local-name()=\'TextSource\']")
  entries <- utils::unzip(out, list = TRUE)$Name
  for (src in srcs) {
    p <- xml2::xml_attr(src, "plainTextPath")
    expect_match(p, paste0("^internal://[0-9a-fA-F-]{36}\\.txt$"), perl = TRUE)
    expect_true(paste0("sources/", sub("^internal://", "", p)) %in% entries)
    # the TextSource guid IS the file name (spec 8.3)
    expect_equal(paste0(xml2::xml_attr(src, "guid"), ".txt"),
                 sub("^internal://", "", p))
  }
})

test_that("QDPX export round-trips through pakhom's own .parse_qdpx_deep reader", {
  skip_if_not_installed("xml2")
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  export_qdpx(coding_state = fx$state, data = fx$data, output_path = out)

  parsed <- pakhom:::.parse_qdpx_deep(out)
  # The code comes back
  expect_true("Routines" %in% parsed$codebook$code_name)
  # The coding reference comes back with offsets + coded text intact
  # (pre-fix this was empty: offsets lived in a nonexistent <TextSelection>
  # element the reader never looked at)
  refs <- parsed$coding_references
  expect_gte(nrow(refs), 1L)
  r <- refs[refs$code_name == "Routines", ][1, ]
  expect_equal(as.integer(r$start_pos), 23L)
  expect_equal(as.integer(r$end_pos), 40L)
  expect_equal(r$coded_text, "exercise routines")
})

test_that("QDPX project.qde validates against the official XSD when available", {
  skip_if_not_installed("xml2")
  xsd_path <- Sys.getenv("REFI_QDA_PROJECT_XSD", "")
  skip_if(!nzchar(xsd_path) || !file.exists(xsd_path),
          "Set REFI_QDA_PROJECT_XSD to a local copy of the official Project.xsd")
  fx <- .qdpx_minimal_fixture()
  out <- withr::local_tempfile(fileext = ".qdpx")
  export_qdpx(coding_state = fx$state, data = fx$data, output_path = out)
  tmp_dir <- withr::local_tempdir()
  utils::unzip(out, files = "project.qde", exdir = tmp_dir)
  doc <- xml2::read_xml(file.path(tmp_dir, "project.qde"))
  schema <- xml2::read_xml(xsd_path)
  expect_true(xml2::xml_validate(doc, schema))
})
