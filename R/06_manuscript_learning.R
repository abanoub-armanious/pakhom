# ==============================================================================
# Manuscript Learning -- Parse Previous Analyses for Few-Shot Context
# ==============================================================================
# Fixes the #1 bug from the old script: manuscript learning now works.
# - Absolute paths required (no more relative path failures)
# - Proper DOCX parsing with section extraction
# - Filename metadata parsing for raw data files
# - Task-specific context slices (coding, theming, review)
# ==============================================================================

#' Discover study folders matching a pattern
#'
#' @param base_dir Absolute path to manual analyses directory
#' @param pattern Regex for folder names (default: matches "study" suffix)
#' @return Character vector of absolute folder paths, or empty vector
discover_study_folders <- function(base_dir, pattern = "study$") {
  base_dir <- normalizePath(base_dir, mustWork = TRUE)

  all_dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  matched <- all_dirs[grepl(pattern, basename(all_dirs), ignore.case = TRUE)]

  log_info("Found {length(matched)} study folders in {base_dir}")
  for (d in matched) log_info("  - {basename(d)}")

  matched
}

#' Load all previous studies from a base directory
#'
#' @param base_dir Absolute path to manual analyses root
#' @param config Learning config section from YAML
#' @return PreviousStudies S3 object
#' @export
load_previous_studies <- function(base_dir, config = list()) {
  config$folder_pattern <- config$folder_pattern %||% "study$"
  config$manuscript_filenames <- config$manuscript_filenames %||%
    c("finalized themes", "manuscript", "analysis")
  config$raw_data_subfolder <- config$raw_data_subfolder %||% "raw data"

  base_dir <- normalizePath(base_dir, mustWork = TRUE)
  folders <- discover_study_folders(base_dir, config$folder_pattern)

  studies <- list()

  for (folder in folders) {
    study_name <- basename(folder)
    log_info("Loading study: {study_name}")

    # Find manuscript file
    manuscript <- .find_manuscript(folder, config$manuscript_filenames)

    # Find raw data
    raw_dir <- file.path(folder, config$raw_data_subfolder)
    raw_data <- NULL
    if (dir.exists(raw_dir)) {
      raw_data <- parse_raw_data_files(raw_dir)
    }

    # Find and parse QDA codebook (Excel, CSV, or QDPX project files)
    codebook <- NULL

    # First: check for QDPX project files (NVivo exports) in folder and raw data subfolder
    qdpx_candidates <- list.files(folder, pattern = "\\.qdpx$",
                                   full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    if (length(qdpx_candidates) > 0) {
      codebook <- parse_codebook(qdpx_candidates[1])
    }

    # Second: check for Excel/CSV codebook exports in the study folder
    if (is.null(codebook)) {
      codebook_patterns <- config$codebook_patterns %||%
        c("codebook", "codes", "nvivo", "atlas", "maxqda")
      codebook_extensions <- c("xlsx", "xls", "csv")
      for (cb_pattern in codebook_patterns) {
        for (cb_ext in codebook_extensions) {
          candidates <- list.files(folder, pattern = paste0(cb_pattern, ".*\\.", cb_ext, "$"),
                                    full.names = TRUE, ignore.case = TRUE)
          if (length(candidates) > 0) {
            codebook <- parse_codebook(candidates[1])
            if (!is.null(codebook)) break
          }
        }
        if (!is.null(codebook)) break
      }
    }

    # Extract deep QDPX data if available (attached by parse_codebook)
    qdpx_deep <- attr(codebook, "qdpx_deep")
    coding_references <- if (!is.null(qdpx_deep)) qdpx_deep$coding_references else NULL
    source_texts <- if (!is.null(qdpx_deep)) qdpx_deep$sources else NULL
    codebook_hierarchy <- if (!is.null(qdpx_deep)) qdpx_deep$hierarchy else NULL
    codebook_full <- if (!is.null(qdpx_deep)) qdpx_deep$codebook_full else NULL

    studies[[study_name]] <- list(
      name = study_name,
      folder = folder,
      manuscript = manuscript,
      raw_data = raw_data,
      codebook = codebook,
      codebook_full = codebook_full,
      coding_references = coding_references,
      source_texts = source_texts,
      codebook_hierarchy = codebook_hierarchy
    )
  }

  obj <- list(studies = studies, n_studies = length(studies))
  class(obj) <- "PreviousStudies"

  log_info("Loaded {length(studies)} previous studies")
  obj
}

#' Parse a finalized themes manuscript (DOCX or PDF)
#'
#' @param file_path Absolute path to manuscript file
#' @return List with: study_name, full_text, word_count, sections (named list)
parse_manuscript <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))

  text <- switch(ext,
    docx = .extract_docx_text(file_path),
    pdf = .extract_pdf_text(file_path),
    txt = paste(readLines(file_path, warn = FALSE), collapse = "\n"),
    md = paste(readLines(file_path, warn = FALSE), collapse = "\n"),
    {
      log_warn("Unsupported manuscript format: {ext}")
      return(NULL)
    }
  )

  if (is.null(text) || nchar(text) < 50) {
    log_warn("Manuscript text too short or empty: {file_path}")
    return(NULL)
  }

  sections <- extract_manuscript_sections(text)

  list(
    file = file_path,
    full_text = text,
    word_count = length(strsplit(text, "\\s+")[[1]]),
    sections = sections
  )
}

#' Extract structured sections from manuscript text
#'
#' Identifies common academic sections by heading patterns.
#'
#' @param content Raw text from manuscript
#' @return Named list of sections: themes, methodology, findings, discussion, etc.
extract_manuscript_sections <- function(content) {
  # Split into lines
  lines <- strsplit(content, "\n")[[1]]
  lines <- trimws(lines)

  # Section heading patterns (case-insensitive)
  section_patterns <- list(
    introduction = "^(introduction|background|overview)",
    methodology = "^(subject.*method|method|methodology|approach|design|procedure)",
    results = "^(result|finding|outcome|analysis result)",
    themes = "^(theme\\s*\\d|theme\\s*[ivx]|theme:|thematic|identified theme|major theme|key theme)",
    discussion = "^(discussion|interpretation|implication)",
    conclusion = "^(conclusion|summary|closing)"
  )

  sections <- list()
  current_section <- "preamble"
  current_text <- character(0)

  for (line in lines) {
    if (nchar(line) == 0) next

    # Check if this line is a section heading
    found_section <- FALSE
    for (section_name in names(section_patterns)) {
      if (grepl(section_patterns[[section_name]], tolower(line))) {
        # Save accumulated text to the current section (append if it already exists)
        if (length(current_text) > 0) {
          new_content <- paste(current_text, collapse = "\n")
          if (!is.null(sections[[current_section]])) {
            sections[[current_section]] <- paste(
              sections[[current_section]], new_content, sep = "\n\n"
            )
          } else {
            sections[[current_section]] <- new_content
          }
        }
        current_section <- section_name
        current_text <- character(0)
        found_section <- TRUE
        break
      }
    }

    if (!found_section) {
      current_text <- c(current_text, line)
    }
  }

  # Save last section (append if it already exists)
  if (length(current_text) > 0) {
    new_content <- paste(current_text, collapse = "\n")
    if (!is.null(sections[[current_section]])) {
      sections[[current_section]] <- paste(
        sections[[current_section]], new_content, sep = "\n\n"
      )
    } else {
      sections[[current_section]] <- new_content
    }
  }

  sections
}

#' Parse raw data DOCX files and extract metadata from filenames
#'
#' Handles the specific naming convention:
#' YYYY-MM-DD_YYYY-MM-DD_Username XXX_Rating X.X_Likes XXX.docx
#'
#' @param raw_data_dir Path to raw data folder
#' @param max_files Maximum files to process (NULL = all)
#' @param seed Optional random seed for reproducible file sampling
#' @return tibble with: filename, text, username, rating, likes, date_scraped, date_posted
parse_raw_data_files <- function(raw_data_dir, max_files = NULL, seed = NULL) {
  files <- list.files(raw_data_dir, pattern = "\\.docx$",
                       full.names = TRUE, recursive = FALSE)

  if (length(files) == 0) {
    log_warn("No .docx files found in: {raw_data_dir}")
    return(NULL)
  }

  if (!is.null(max_files) && length(files) > max_files) {
    if (!is.null(seed)) {
      files <- withr::with_seed(seed, sample(files, max_files))
    } else {
      files <- sample(files, max_files)
    }
  }

  log_info("Parsing {length(files)} raw data files...")

  results <- lapply(files, function(f) {
    tryCatch({
      text <- .extract_docx_text(f)
      meta <- .parse_filename_metadata(basename(f))

      list(
        filename = basename(f),
        text = text %||% "",
        username = meta$username,
        rating = meta$rating,
        likes = meta$likes,
        date_scraped = meta$date_scraped,
        date_posted = meta$date_posted
      )
    }, error = function(e) {
      log_debug("Failed to parse {basename(f)}: {e$message}")
      NULL
    })
  })

  results <- results[!vapply(results, is.null, logical(1))]

  if (length(results) == 0) return(NULL)

  tibble(
    filename = vapply(results, function(r) r$filename, character(1)),
    text = vapply(results, function(r) r$text, character(1)),
    username = vapply(results, function(r) r$username %||% NA_character_, character(1)),
    rating = vapply(results, function(r) r$rating %||% NA_real_, numeric(1)),
    likes = vapply(results, function(r) r$likes %||% NA_integer_, integer(1)),
    date_scraped = vapply(results, function(r) r$date_scraped %||% NA_character_, character(1)),
    date_posted = vapply(results, function(r) r$date_posted %||% NA_character_, character(1))
  )
}

#' Format a codebook tibble as a human-readable hierarchy string
#'
#' Works with both deep QDPX codebooks (with hierarchy_level, is_codable,
#' is_discarded columns) and simpler codebooks (with parent_code column).
#' Dynamically adapts to whatever structure is available.
#'
#' @param cb Codebook tibble (either codebook_full from QDPX or basic codebook)
#' @param max_chars Maximum characters in output
#' @return Character string with formatted hierarchy
#' @keywords internal
.format_codebook_hierarchy <- function(cb, max_chars = 5000) {
  if (is.null(cb) || nrow(cb) == 0) return("")

  lines <- character(0)
  has_deep <- "hierarchy_level" %in% names(cb) && "is_codable" %in% names(cb)

  if (has_deep) {
    # Deep QDPX format: use hierarchy_level for indentation
    for (i in seq_len(nrow(cb))) {
      if (isTRUE(cb$is_discarded[i])) next  # skip discarded in main view

      indent <- paste(rep("  ", cb$hierarchy_level[i]), collapse = "")
      leaf_marker <- if (isTRUE(cb$is_codable[i])) " [CODE]" else ""
      freq_str <- if (cb$frequency[i] > 0) {
        sprintf(" (freq=%d, sources=%d)", cb$frequency[i], cb$n_sources[i])
      } else ""

      line <- paste0(indent, cb$code_name[i], leaf_marker, freq_str)

      # Add description if available (truncated)
      desc <- cb$description[i]
      if (!is.na(desc) && nchar(desc) > 10) {
        line <- paste0(line, "\n", indent, "  -> ", substr(desc, 1, 200))
      }

      lines <- c(lines, line)
    }
  } else {
    # Simpler format: use parent_code for grouping
    parents <- unique(cb$parent_code[!is.na(cb$parent_code)])
    top_level <- cb[is.na(cb$parent_code), ]

    for (i in seq_len(nrow(top_level))) {
      line <- top_level$code_name[i]
      freq_str <- if (!is.na(top_level$frequency[i]) && top_level$frequency[i] > 0) {
        sprintf(" (freq=%d)", top_level$frequency[i])
      } else ""
      lines <- c(lines, paste0(line, freq_str))

      # Find children
      children <- cb[!is.na(cb$parent_code) & cb$parent_code == top_level$code_name[i], ]
      for (j in seq_len(nrow(children))) {
        child_freq <- if (!is.na(children$frequency[j]) && children$frequency[j] > 0) {
          sprintf(" (freq=%d)", children$frequency[j])
        } else ""
        lines <- c(lines, paste0("  ", children$code_name[j], " [CODE]", child_freq))
      }
    }

    # Orphan codes (no parent, not a parent themselves)
    orphans <- cb[is.na(cb$parent_code) & !(cb$code_name %in% parents), ]
    if (nrow(orphans) > 0 && nrow(top_level) == 0) {
      for (i in seq_len(nrow(orphans))) {
        freq_str <- if (!is.na(orphans$frequency[i]) && orphans$frequency[i] > 0) {
          sprintf(" (freq=%d)", orphans$frequency[i])
        } else ""
        lines <- c(lines, paste0(orphans$code_name[i], " [CODE]", freq_str))
      }
    }
  }

  result <- paste(lines, collapse = "\n")
  if (nchar(result) > max_chars) {
    result <- paste0(substr(result, 1, max_chars - 20), "\n... [truncated]")
  }
  result
}

#' Generate task-specific learning context from previous analyses
#'
#' Produces a LearningContext object with separate slices for coding,
#' theming, and review prompts. Uses a CODEBOOK-FIRST approach: the
#' codebook hierarchy (themes, subthemes, codes, descriptions, frequencies,
#' and entry-level coding examples) is the primary learning source.
#' Manuscripts are supplementary, used only when codebook descriptions
#' are lacking.
#'
#' @param studies PreviousStudies object
#' @param max_codebook_chars Max characters for codebook context per study
#' @param max_manuscript_chars Max characters for manuscript supplements per study
#' @param max_raw_samples Max raw data examples per study
#' @return LearningContext S3 object
#' @export
generate_learning_context <- function(studies, max_codebook_chars = 20000,
                                       max_manuscript_chars = 8000,
                                       max_raw_samples = 5) {
  if (!inherits(studies, "PreviousStudies") || studies$n_studies == 0) {
    log_warn("No previous studies available for learning context")
    return(.empty_learning_context())
  }

  coding_parts <- character(0)
  coding_style_parts <- character(0)
  coding_discard_parts <- character(0)
  theming_parts <- character(0)
  review_parts <- character(0)
  entry_level_examples <- list()
  codebook_hierarchies <- list()
  raw_summary <- character(0)

  per_study_cb_budget <- max_codebook_chars / studies$n_studies
  per_study_ms_budget <- max_manuscript_chars / studies$n_studies

  for (study in studies$studies) {
    study_label <- toupper(study$name)
    has_codebook <- !is.null(study$codebook) && nrow(study$codebook) > 0
    has_deep_data <- !is.null(study$codebook_full)
    has_manuscript <- !is.null(study$manuscript)

    # ================================================================
    # PRIMARY SOURCE: Codebook hierarchy
    # ================================================================
    if (has_codebook) {
      cb <- if (has_deep_data) study$codebook_full else study$codebook

      # --- Coding context: full hierarchy with descriptions ---
      hierarchy_text <- .format_codebook_hierarchy(cb, per_study_cb_budget)
      coding_parts <- c(coding_parts, paste0(
        "### ", study_label, " - Codebook Structure (from human analysis):\n",
        "This is how an experienced researcher organized codes in a similar study.\n",
        "Learn from this structure: the granularity of codes, how they relate to ",
        "themes/subthemes, and the descriptive style.\n\n",
        hierarchy_text
      ))

      # --- Theming context: hierarchy as merge exemplar ---
      theming_parts <- c(theming_parts, paste0(
        "### ", study_label, " - How Codes Were Grouped Into Themes:\n",
        "The researcher organized codes into themes and subthemes as follows. ",
        "Use this as a structural reference for how codes naturally cluster.\n\n",
        hierarchy_text
      ))

      # --- Review context: theme names for specificity benchmark ---
      if (has_deep_data) {
        themes_only <- cb[cb$hierarchy_level == 0 & !cb$is_discarded, ]
      } else {
        themes_only <- cb[is.na(cb$parent_code), ]
      }
      if (nrow(themes_only) > 0) {
        theme_lines <- vapply(seq_len(nrow(themes_only)), function(i) {
          desc <- themes_only$description[i]
          desc_str <- if (!is.na(desc) && nchar(desc) > 0) {
            paste0(": ", substr(desc, 1, 200))
          } else ""
          paste0("  - ", themes_only$code_name[i], desc_str)
        }, character(1))
        review_parts <- c(review_parts, paste0(
          "### ", study_label, " - Theme Names (Human Researcher):\n",
          "Use these as a SPECIFICITY BENCHMARK:\n",
          paste(theme_lines, collapse = "\n")
        ))
      }

      # --- Discarded codes context ---
      if (has_deep_data) {
        discarded <- cb[cb$is_discarded & cb$is_codable, ]
        if (nrow(discarded) > 0) {
          discard_desc <- cb[cb$is_discarded & !cb$is_codable, ]
          discard_reason <- if (nrow(discard_desc) > 0 && !is.na(discard_desc$description[1])) {
            discard_desc$description[1]
          } else "considered too vague or not directly relevant"
          coding_discard_parts <- c(coding_discard_parts, paste0(
            "### ", study_label, " - Codes the Researcher DISCARDED:\n",
            "Reason: ", discard_reason, "\n",
            "Discarded codes: ", paste(discarded$code_name, collapse = ", "), "\n",
            "If you encounter similar patterns, do NOT create codes for them."
          ))
        }
      }

      # --- Coding style: segment-level vs whole-entry coding ---
      if (!is.null(study$coding_references) && nrow(study$coding_references) > 0) {
        refs <- study$coding_references
        coded_texts <- refs$coded_text[!is.na(refs$coded_text)]
        if (length(coded_texts) > 0) {
          seg_lengths <- nchar(coded_texts)
          avg_seg <- round(mean(seg_lengths))
          median_seg <- round(median(seg_lengths))

          # How many codes per source?
          codes_per_source <- table(refs$source_guid)
          avg_codes <- round(mean(codes_per_source), 1)

          coding_style_parts <- c(coding_style_parts, paste0(
            "### ", study_label, " - Coding Style:\n",
            "The researcher coded SPECIFIC TEXT SEGMENTS, not entire entries.\n",
            "Average coded segment length: ", avg_seg, " characters (median: ", median_seg, ")\n",
            "Average codes per entry: ", avg_codes, "\n",
            "Follow this approach: code the specific relevant text, not the whole entry."
          ))

          # Add entry-level coding examples (actual coded segments)
          sample_refs <- refs[!is.na(refs$coded_text), ]
          sample_refs <- withr::with_seed(42, {
            sample_refs[sample(seq_len(nrow(sample_refs)),
                               min(10, nrow(sample_refs))), ]
          })
          for (j in seq_len(nrow(sample_refs))) {
            entry_level_examples[[length(entry_level_examples) + 1L]] <- list(
              study = study$name,
              code = sample_refs$code_name[j],
              text = substr(sample_refs$coded_text[j], 1, 300),
              source = sample_refs$source_name[j]
            )
          }
        }
      }

      # Store hierarchy for downstream use
      if (!is.null(study$codebook_hierarchy)) {
        codebook_hierarchies[[study$name]] <- study$codebook_hierarchy
      }
    }

    # ================================================================
    # SUPPLEMENTARY SOURCE: Manuscripts (only when codebook lacks detail)
    # ================================================================
    if (has_manuscript) {
      ms <- study$manuscript

      # Only use methodology if codebook doesn't provide enough context
      if (!is.null(ms$sections$methodology)) {
        method_excerpt <- substr(ms$sections$methodology, 1, per_study_ms_budget * 0.5)
        coding_parts <- c(coding_parts, paste0(
          "### ", study_label, " - Analytical Methodology (supplement):\n",
          "The researcher's methodology provides additional context:\n",
          method_excerpt
        ))
      }

      # Only use discussion if codebook theme descriptions are lacking
      codebook_has_descriptions <- has_deep_data &&
        sum(!is.na(cb$description) & nchar(cb$description) > 10, na.rm = TRUE) > 3
      if (!codebook_has_descriptions && !is.null(ms$sections$discussion)) {
        discussion_excerpt <- substr(ms$sections$discussion, 1, per_study_ms_budget * 0.5)
        theming_parts <- c(theming_parts, paste0(
          "### ", study_label, " - Researcher's Interpretive Lens (supplement):\n",
          "The codebook lacked detailed descriptions. The researcher's discussion ",
          "provides additional context on how themes relate:\n",
          discussion_excerpt
        ))
      }
    }

    # Raw data summary (unchanged)
    if (!is.null(study$raw_data) && nrow(study$raw_data) > 0) {
      rd <- study$raw_data
      rd_with_text <- rd |> filter(!is.na(text), nchar(text) > 20)
      n_with_text <- nrow(rd_with_text)

      has_ratings <- any(!is.na(rd$rating))
      summary_line <- if (has_ratings) {
        sprintf("%s: %d files (%d with content), rating range: %.1f-%.1f",
                study_label, nrow(rd), n_with_text,
                min(rd$rating, na.rm = TRUE), max(rd$rating, na.rm = TRUE))
      } else {
        sprintf("%s: %d files (%d with content)", study_label, nrow(rd), n_with_text)
      }
      raw_summary <- c(raw_summary, summary_line)

      if (n_with_text > 0) {
        samples <- rd_with_text |>
          slice_sample(n = min(max_raw_samples, n_with_text))
        for (i in seq_len(nrow(samples))) {
          sample_text <- substr(samples$text[i], 1, 300)
          coding_parts <- c(coding_parts, paste0(
            "Example entry from ", study_label, ": \"", sample_text, "\""
          ))
        }
      }
    }
  }

  # ================================================================
  # Compute empirical coding benchmarks from codebooks
  # ================================================================
  benchmarks <- compute_coding_benchmarks(studies)
  calibration_parts <- character(0)

  if (!is.null(benchmarks)) {
    cal_text <- paste0(
      "## EMPIRICAL CODING BENCHMARKS (from ", benchmarks$n_codebooks,
      " prior human-coded studies)\n",
      "These benchmarks reflect how experienced human researchers code similar data:\n\n"
    )

    if (!is.null(benchmarks$max_code_coverage_pct)) {
      cal_text <- paste0(cal_text,
        "- Maximum code coverage: ", benchmarks$max_code_coverage_pct,
        "% of entries (no single code should exceed this)\n")
    }
    if (!is.null(benchmarks$typical_code_count)) {
      cal_text <- paste0(cal_text,
        "- Typical codebook size: ~", benchmarks$typical_code_count, " leaf codes per study\n")
    }
    if (!is.null(benchmarks$codes_per_theme)) {
      cal_text <- paste0(cal_text,
        "- Codes per theme: ~", benchmarks$codes_per_theme, " on average\n")
    }
    if (!is.null(benchmarks$code_word_count)) {
      cal_text <- paste0(cal_text,
        "- Code name length: ~", benchmarks$code_word_count, " words on average\n")
    }
    if (!is.null(benchmarks$avg_segment_length)) {
      cal_text <- paste0(cal_text,
        "- Typical coded segment length: ~", benchmarks$avg_segment_length, " characters\n")
    }
    if (!is.null(benchmarks$codes_per_entry)) {
      cal_text <- paste0(cal_text,
        "- Codes per entry: ~", benchmarks$codes_per_entry, " on average\n")
    }
    if (!is.null(benchmarks$hierarchy_depth)) {
      depth_label <- switch(as.character(benchmarks$hierarchy_depth),
        "1" = "flat (codes only)",
        "2" = "two-level (themes + codes)",
        "3" = "three-level (themes + subthemes + codes)",
        paste0(benchmarks$hierarchy_depth, " levels"))
      cal_text <- paste0(cal_text,
        "- Typical hierarchy depth: ", depth_label, "\n")
    }
    if (!is.null(benchmarks$discarded_code_pct)) {
      cal_text <- paste0(cal_text,
        "- Codes discarded by researcher: ~", benchmarks$discarded_code_pct, "%\n")
    }

    # Add example codes
    if (length(benchmarks$example_codes) > 0) {
      cal_text <- paste0(cal_text, "\nExample codes from prior human analyses:\n")
      for (ec in benchmarks$example_codes[seq_len(min(15, length(benchmarks$example_codes)))]) {
        cal_text <- paste0(cal_text,
          "  - \"", ec$code, "\" (freq: ", ec$frequency, ", study: ", ec$study, ")\n")
      }
    }

    calibration_parts <- cal_text
  }

  # ================================================================
  # Cross-study qualitative synthesis
  # ================================================================
  # Instead of just presenting studies sequentially, synthesize structural
  # patterns ACROSS all available codebooks. This helps the AI understand
  # how human researchers consistently organize qualitative data.
  synthesis_text <- ""
  if (length(codebook_hierarchies) >= 2) {
    synthesis_text <- .synthesize_cross_study_patterns(
      codebook_hierarchies, studies, benchmarks
    )
    if (nchar(synthesis_text) > 0) {
      theming_parts <- c(theming_parts, synthesis_text)
      coding_parts <- c(coding_parts, paste0(
        "## CROSS-STUDY PATTERNS\n",
        "Across ", length(codebook_hierarchies),
        " prior manual analyses, researchers consistently:\n",
        synthesis_text
      ))
    }
  }

  # Build entry-level examples text
  entry_examples_text <- ""
  if (length(entry_level_examples) > 0) {
    lines <- vapply(entry_level_examples, function(ex) {
      sprintf("  Code: \"%s\" | Text: \"%s\"", ex$code, substr(ex$text, 1, 150))
    }, character(1))
    entry_examples_text <- paste0(
      "## ENTRY-LEVEL CODING EXAMPLES (from prior human analyses)\n",
      "These show how researchers coded specific text segments:\n\n",
      paste(lines, collapse = "\n")
    )
  }

  ctx <- list(
    for_coding = paste(c(coding_parts, coding_style_parts), collapse = "\n\n"),
    for_coding_calibration = paste(calibration_parts, collapse = "\n\n"),
    for_coding_style = paste(coding_style_parts, collapse = "\n\n"),
    for_coding_discards = paste(coding_discard_parts, collapse = "\n\n"),
    for_coding_examples = entry_examples_text,
    for_theming = paste(theming_parts, collapse = "\n\n"),
    for_review = paste(review_parts, collapse = "\n\n"),
    for_report = "",
    benchmarks = benchmarks,
    entry_level_examples = entry_level_examples,
    codebook_hierarchies = codebook_hierarchies,
    raw_data_summary = paste(raw_summary, collapse = "\n"),
    n_studies = studies$n_studies,
    study_names = names(studies$studies)
  )
  class(ctx) <- "LearningContext"

  total_chars <- nchar(ctx$for_coding) + nchar(ctx$for_theming) +
    nchar(ctx$for_review) + nchar(ctx$for_coding_calibration)
  log_info("Generated learning context (codebook-first): {total_chars} total chars across {studies$n_studies} studies")
  log_info("  Coding context:     {nchar(ctx$for_coding)} chars")
  log_info("  Coding style:       {nchar(ctx$for_coding_style)} chars")
  log_info("  Coding discards:    {nchar(ctx$for_coding_discards)} chars")
  log_info("  Coding examples:    {nchar(ctx$for_coding_examples)} chars")
  log_info("  Theming context:    {nchar(ctx$for_theming)} chars")
  log_info("  Review context:     {nchar(ctx$for_review)} chars")
  log_info("  Calibration:        {nchar(ctx$for_coding_calibration)} chars")
  log_info("  Entry-level examples: {length(entry_level_examples)}")
  log_info("  Codebook hierarchies: {length(codebook_hierarchies)}")

  ctx
}

# ==============================================================================
# Internal helpers
# ==============================================================================

#' Empty learning context for when no studies are available
#' @keywords internal
.empty_learning_context <- function() {
  ctx <- list(
    for_coding = "",
    for_theming = "",
    for_review = "",
    for_coding_calibration = "",
    for_coding_style = "",
    for_coding_discards = "",
    for_coding_examples = "",
    for_report = "",
    benchmarks = NULL,
    raw_data_summary = "",
    n_studies = 0L,
    study_names = character(0),
    entry_level_examples = list(),
    codebook_hierarchies = list()
  )
  class(ctx) <- "LearningContext"
  ctx
}

#' Generate AI reflection on what was learned from previous studies
#'
#' Asks the AI to summarize what patterns, themes, and analytical approaches
#' it extracted from the previous manual analyses, and how that knowledge
#' will guide the current analysis. The reflection is stored in the
#' learning context for inclusion in the final report.
#'
#' @param learning_context LearningContext object
#' @param provider AIProvider object (or NULL for plain-text fallback)
#' @param audit_log An optional AuditLog object (from
#'   \code{\link{init_audit_log}}). When provided, the AI reflection call
#'   is recorded as an \code{ai_request} audit decision (T1.4) with full
#'   provenance (model, usage, prompt_hash). Pass \code{NULL} to skip.
#' @param response_cache An optional ResponseCache object (from
#'   \code{\link{init_response_cache}}). When provided, the raw API
#'   response is written to the cache and referenced from the audit log.
#' @return Updated LearningContext with \code{for_report} populated
#' @export
generate_learning_reflection <- function(learning_context, provider = NULL,
                                          audit_log = NULL,
                                          response_cache = NULL) {
  if (!inherits(learning_context, "LearningContext") || learning_context$n_studies == 0) {
    return(learning_context)
  }

  # Plain-text fallback listing study names
  fallback <- paste0(
    "The AI analysis was informed by ", learning_context$n_studies,
    " previous studies: ", paste(learning_context$study_names, collapse = ", "), ".\n\n",
    "Learning context was generated from these studies and injected into:\n",
    "- Progressive coding (", nchar(learning_context$for_coding), " chars)\n",
    "- Theme generation (", nchar(learning_context$for_theming), " chars)\n",
    "- Theme review (", nchar(learning_context$for_review), " chars)\n"
  )

  if (is.null(provider)) {
    learning_context$for_report <- fallback
    return(learning_context)
  }

  # Build AI prompt with all context slices
  combined_context <- paste0(
    "## CODING CONTEXT (used during initial coding):\n",
    substr(learning_context$for_coding, 1, 3000), "\n\n",
    "## THEMING CONTEXT (used during theme generation):\n",
    substr(learning_context$for_theming, 1, 3000), "\n"
  )

  system_prompt <- paste0(
    "You are an expert qualitative researcher. You have been given excerpts from ",
    "previous manual analyses that were used to inform an AI-assisted thematic ",
    "analysis. Summarize SPECIFICALLY:\n\n",
    "1. What key themes, patterns, and analytical insights were extracted from ",
    "the previous studies?\n",
    "2. How does this prior knowledge guide the current analysis? (e.g., expected ",
    "theme granularity, types of patterns to look for, analytical depth)\n",
    "3. What specific examples from the previous analyses calibrate the AI's ",
    "coding and theming behavior?\n\n",
    "Be concrete -- name specific themes, subthemes, and patterns from the prior ",
    "studies. Write 2-4 paragraphs suitable for a methodology section."
  )

  prompt <- paste0(
    "Studies loaded: ", paste(learning_context$study_names, collapse = ", "), "\n\n",
    combined_context, "\n\n",
    "Summarize what was learned and how it guides the analysis."
  )

  reflection <- tryCatch({
    ai_result <- ai_complete(provider, prompt, system_prompt, task = "review")
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "synthesis", ai_result, response_cache,
                      purpose = "learning_reflection",
                      n_studies = learning_context$n_studies)
    }
    ai_result$content
  }, error = function(e) {
    log_warn("AI learning reflection failed: {e$message}")
    NULL
  })

  learning_context$for_report <- if (!is.null(reflection)) reflection else fallback
  log_info("Learning reflection generated: {nchar(learning_context$for_report)} chars")

  learning_context
}

#' Parse a QDA software codebook export (NVivo, ATLAS.ti, MAXQDA, or generic)
#'
#' Auto-detects the QDA tool format from column headers and sheet structure,
#' then extracts code names, hierarchy, frequencies, and descriptions into
#' a standardized tibble.
#'
#' @param path Path to codebook file (.xlsx, .xls, or .csv)
#' @return tibble with columns: code_name, parent_code, frequency, n_sources,
#'   description, hierarchy_level. Returns NULL if parsing fails.
#' @export
parse_codebook <- function(path) {
  if (!file.exists(path)) {
    log_warn("Codebook file not found: {path}")
    return(NULL)
  }

  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      log_warn("readxl package required for Excel codebook parsing. Install with: install.packages('readxl')")
      return(NULL)
    }
    sheets <- readxl::excel_sheets(path)
    # Read all sheets to detect format
    sheet_data <- lapply(sheets, function(s) {
      tryCatch(readxl::read_excel(path, sheet = s), error = function(e) NULL)
    })
    names(sheet_data) <- sheets
    result <- .detect_and_parse_codebook(sheet_data, path)
  } else if (ext == "csv") {
    df <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE),
                   error = function(e) NULL)
    if (is.null(df)) {
      log_warn("Failed to read CSV codebook: {path}")
      return(NULL)
    }
    sheet_data <- list(main = tibble::as_tibble(df))
    result <- .detect_and_parse_codebook(sheet_data, path)
  } else if (ext == "qdpx") {
    deep_result <- .parse_qdpx_deep(path)
    if (is.null(deep_result)) return(NULL)
    # Return backward-compatible tibble, but attach deep result as attribute
    result <- deep_result$codebook
    attr(result, "qdpx_deep") <- deep_result
    if (!is.null(result) && nrow(result) > 0) {
      log_info("Parsed codebook: {nrow(result)} codes from {basename(path)}")
    }
    return(result)
  } else {
    log_warn("Unsupported codebook format: {ext}")
    return(NULL)
  }

  if (!is.null(result) && nrow(result) > 0) {
    log_info("Parsed codebook: {nrow(result)} codes from {basename(path)}")
  }
  result
}

#' Parse NVivo QDPX project file with deep hierarchy and entry-level coding
#'
#' QDPX files are ZIP archives containing a project.qde XML file with
#' the complete code structure, all coding references, and source texts.
#' This recursively extracts the full theme->subtheme->code hierarchy,
#' entry-level coding references (which text segments were coded), and
#' source document texts.
#'
#' @param path Path to .qdpx file
#' @return List with: $codebook (tibble), $codebook_full (tibble),
#'   $coding_references (tibble), $sources (tibble), $hierarchy (nested list)
#' @keywords internal
.parse_qdpx_deep <- function(path) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  tryCatch({
    # Extract entire QDPX (project.qde + source text files)
    utils::unzip(path, exdir = temp_dir)
    qde_path <- file.path(temp_dir, "project.qde")

    if (!file.exists(qde_path)) {
      log_warn("No project.qde found in QDPX file: {path}")
      return(NULL)
    }

    doc <- xml2::read_xml(qde_path)
    ns <- c(d1 = "urn:QDA-XML:project:1.0")

    # ---- 1. Recursively extract ALL codes at ALL depths ----
    codes_root <- xml2::xml_find_first(doc, ".//d1:CodeBook/d1:Codes", ns)
    if (is.na(codes_root)) {
      # Fallback: try without CodeBook wrapper
      codes_root <- xml2::xml_find_first(doc, ".//d1:Codes", ns)
    }
    if (is.na(codes_root)) {
      log_warn("No CodeBook/Codes found in QDPX file: {path}")
      return(NULL)
    }

    code_rows <- list()
    hierarchy <- list()

    .recurse_codes <- function(parent_node, parent_guid, parent_name, depth,
                               path_parts, discard_ancestor) {
      children <- xml2::xml_find_all(parent_node, "d1:Code", ns)
      child_list <- list()

      for (child in children) {
        guid <- xml2::xml_attr(child, "guid")
        name <- xml2::xml_attr(child, "name")
        is_codable_attr <- xml2::xml_attr(child, "isCodable")
        if (is.na(is_codable_attr)) is_codable_attr <- "true"
        desc_node <- xml2::xml_find_first(child, "d1:Description", ns)
        description <- tryCatch(xml2::xml_text(desc_node),
                                error = function(e) NA_character_)
        if (length(description) == 0) description <- NA_character_

        # Detect discarded codes section
        is_discard_root <- grepl(
          "not organized|not included|unassigned|discarded",
          tolower(name)
        )
        is_discarded <- discard_ancestor || is_discard_root

        current_path <- c(path_parts, name)
        hierarchy_path <- paste(current_path, collapse = " / ")

        code_rows[[length(code_rows) + 1L]] <<- list(
          code_guid = guid,
          code_name = name,
          parent_guid = if (is.na(parent_guid)) NA_character_ else parent_guid,
          parent_name = if (is.na(parent_name)) NA_character_ else parent_name,
          description = description,
          hierarchy_level = as.integer(depth),
          hierarchy_path = hierarchy_path,
          is_codable = identical(is_codable_attr, "true"),
          is_discarded = is_discarded
        )

        # Recurse into children
        subtree <- .recurse_codes(child, guid, name, depth + 1L,
                                   current_path, is_discarded)

        child_list[[length(child_list) + 1L]] <- list(
          guid = guid, name = name, description = description,
          is_codable = identical(is_codable_attr, "true"),
          is_discarded = is_discarded, depth = depth,
          children = subtree
        )
      }
      child_list
    }

    hierarchy <- .recurse_codes(codes_root, NA_character_, NA_character_,
                                 0L, character(0), FALSE)

    if (length(code_rows) == 0) {
      log_warn("No codes found in QDPX file: {path}")
      return(NULL)
    }

    # Build codebook tibble from recursive extraction
    codebook <- tibble(
      code_guid = vapply(code_rows, `[[`, character(1), "code_guid"),
      code_name = vapply(code_rows, `[[`, character(1), "code_name"),
      parent_guid = vapply(code_rows, `[[`, character(1), "parent_guid"),
      parent_name = vapply(code_rows, `[[`, character(1), "parent_name"),
      description = vapply(code_rows, `[[`, character(1), "description"),
      hierarchy_level = vapply(code_rows, `[[`, integer(1), "hierarchy_level"),
      hierarchy_path = vapply(code_rows, `[[`, character(1), "hierarchy_path"),
      is_codable = vapply(code_rows, `[[`, logical(1), "is_codable"),
      is_discarded = vapply(code_rows, `[[`, logical(1), "is_discarded"),
      frequency = 0L,
      n_sources = 0L
    )

    # Build GUID -> name lookup for all codes
    code_lookup <- setNames(codebook$code_name, codebook$code_guid)

    # ---- 2. Extract source texts from ZIP ----
    source_nodes <- xml2::xml_find_all(doc, ".//d1:TextSource", ns)
    source_rows <- list()

    for (src_node in source_nodes) {
      src_guid <- xml2::xml_attr(src_node, "guid")
      src_name <- xml2::xml_attr(src_node, "name")
      plain_text_path <- xml2::xml_attr(src_node, "plainTextPath")

      src_text <- NA_character_
      if (!is.null(plain_text_path) && !is.na(plain_text_path)) {
        txt_filename <- sub("^internal://", "", plain_text_path)
        txt_path <- file.path(temp_dir, "sources", txt_filename)
        if (!file.exists(txt_path)) {
          # Try capitalized "Sources" directory (case-sensitive filesystems)
          txt_path <- file.path(temp_dir, "Sources", txt_filename)
        }
        if (file.exists(txt_path)) {
          src_text <- tryCatch(
            paste(readLines(txt_path, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n"),
            error = function(e) NA_character_
          )
        }
      }

      source_rows[[length(source_rows) + 1L]] <- list(
        source_guid = src_guid,
        source_name = src_name,
        plain_text_content = src_text
      )
    }

    sources_tbl <- tibble(
      source_guid = vapply(source_rows, `[[`, character(1), "source_guid"),
      source_name = vapply(source_rows, `[[`, character(1), "source_name"),
      plain_text_content = vapply(source_rows, `[[`, character(1), "plain_text_content")
    )

    # ---- 3. Extract coding references (entry-level coding) ----
    coding_ref_rows <- list()
    code_source_sets <- list()

    for (src_node in source_nodes) {
      src_guid <- xml2::xml_attr(src_node, "guid")
      src_name <- xml2::xml_attr(src_node, "name")
      src_idx <- which(sources_tbl$source_guid == src_guid)
      src_text <- if (length(src_idx) > 0) sources_tbl$plain_text_content[src_idx[1]] else NA_character_

      selections <- xml2::xml_find_all(src_node, ".//d1:PlainTextSelection", ns)

      for (sel in selections) {
        start_pos <- suppressWarnings(
          as.integer(xml2::xml_attr(sel, "startPosition"))
        )
        end_pos <- suppressWarnings(
          as.integer(xml2::xml_attr(sel, "endPosition"))
        )

        codings <- xml2::xml_find_all(sel, "d1:Coding", ns)
        for (coding in codings) {
          cref <- xml2::xml_find_first(coding, "d1:CodeRef", ns)
          if (is.na(cref)) next
          code_guid <- xml2::xml_attr(cref, "targetGUID")
          code_name_val <- code_lookup[code_guid]
          if (is.na(code_name_val)) code_name_val <- NA_character_

          # Extract coded text using character positions
          coded_text <- NA_character_
          if (!is.na(src_text) && !is.na(start_pos) && !is.na(end_pos) &&
              start_pos >= 0 && end_pos > start_pos &&
              end_pos <= nchar(src_text)) {
            coded_text <- substr(src_text, start_pos + 1L, end_pos)
          }

          coding_ref_rows[[length(coding_ref_rows) + 1L]] <- list(
            code_guid = code_guid,
            code_name = code_name_val,
            source_guid = src_guid,
            source_name = src_name,
            start_pos = if (is.na(start_pos)) NA_integer_ else start_pos,
            end_pos = if (is.na(end_pos)) NA_integer_ else end_pos,
            coded_text = coded_text
          )

          # Track sources per code
          if (is.null(code_source_sets[[code_guid]])) {
            code_source_sets[[code_guid]] <- character(0)
          }
          code_source_sets[[code_guid]] <- unique(
            c(code_source_sets[[code_guid]], src_guid)
          )
        }
      }
    }

    coding_refs_tbl <- if (length(coding_ref_rows) > 0) {
      tibble(
        code_guid = vapply(coding_ref_rows, `[[`, character(1), "code_guid"),
        code_name = vapply(coding_ref_rows, `[[`, character(1), "code_name"),
        source_guid = vapply(coding_ref_rows, `[[`, character(1), "source_guid"),
        source_name = vapply(coding_ref_rows, `[[`, character(1), "source_name"),
        start_pos = vapply(coding_ref_rows, `[[`, integer(1), "start_pos"),
        end_pos = vapply(coding_ref_rows, `[[`, integer(1), "end_pos"),
        coded_text = vapply(coding_ref_rows, `[[`, character(1), "coded_text")
      )
    } else {
      tibble(
        code_guid = character(), code_name = character(),
        source_guid = character(), source_name = character(),
        start_pos = integer(), end_pos = integer(),
        coded_text = character()
      )
    }

    # ---- 4. Compute frequency and n_sources per code ----
    if (nrow(coding_refs_tbl) > 0) {
      freq_table <- table(coding_refs_tbl$code_guid)
      for (i in seq_len(nrow(codebook))) {
        guid <- codebook$code_guid[i]
        ct <- freq_table[guid]
        codebook$frequency[i] <- if (!is.na(ct)) as.integer(ct) else 0L
        codebook$n_sources[i] <- length(code_source_sets[[guid]] %||% character(0))
      }
    }

    # ---- 4b. Roll up frequencies from leaf codes to parent nodes ----
    # Non-leaf nodes (themes, subthemes) have frequency=0 because coding
    # references only target leaf codes. Aggregate child frequencies upward
    # so the AI understands the scale of each theme/subtheme.
    max_depth <- max(codebook$hierarchy_level, na.rm = TRUE)
    if (max_depth > 0) {
      # Walk from deepest level upward, summing child frequencies into parents
      for (depth in seq(max_depth - 1L, 0L)) {
        parent_guids_at_depth <- codebook$code_guid[codebook$hierarchy_level == depth]
        for (pg in parent_guids_at_depth) {
          child_mask <- !is.na(codebook$parent_guid) & codebook$parent_guid == pg
          if (any(child_mask)) {
            codebook$frequency[codebook$code_guid == pg] <-
              sum(codebook$frequency[child_mask], na.rm = TRUE)
            # Also roll up n_sources (unique sources across children)
            child_source_sets <- lapply(
              codebook$code_guid[child_mask],
              function(cg) code_source_sets[[cg]] %||% character(0)
            )
            codebook$n_sources[codebook$code_guid == pg] <-
              length(unique(unlist(child_source_sets)))
          }
        }
      }
      log_info("Rolled up frequencies: top-level themes now have aggregated counts")
    }

    # ---- 5. Backward-compatible codebook tibble ----
    compat_codebook <- tibble(
      code_name = codebook$code_name,
      parent_code = codebook$parent_name,
      frequency = codebook$frequency,
      n_sources = codebook$n_sources,
      description = codebook$description,
      hierarchy_level = codebook$hierarchy_level
    )

    n_leaf <- sum(codebook$is_codable)
    n_discarded <- sum(codebook$is_discarded)
    n_total <- nrow(codebook)

    log_info("Parsed QDPX (deep): {n_total} codes ({n_leaf} leaf, {n_discarded} discarded), ",
             "{nrow(sources_tbl)} sources, {nrow(coding_refs_tbl)} coding references")

    list(
      codebook = compat_codebook,
      codebook_full = codebook,
      coding_references = coding_refs_tbl,
      sources = sources_tbl,
      hierarchy = hierarchy
    )

  }, error = function(e) {
    log_warn("Failed to parse QDPX file: {e$message}")
    NULL
  })
}

#' Detect QDA tool format and parse accordingly
#' @keywords internal
.detect_and_parse_codebook <- function(sheet_data, path) {
  # Flatten all column names across sheets for detection
  all_cols <- tolower(unlist(lapply(sheet_data, function(d) {
    if (!is.null(d)) names(d) else character(0)
  })))
  all_sheets <- tolower(names(sheet_data))

  # --- NVivo detection ---
  # NVivo exports: columns typically include Name, Description, Sources, References
  nvivo_indicators <- c("sources", "references")
  if (sum(nvivo_indicators %in% all_cols) >= 2) {
    log_info("Detected NVivo codebook format")
    return(.parse_nvivo_codebook(sheet_data))
  }

  # --- MAXQDA detection ---
  # MAXQDA exports: sheet names like "Code System", "Document System"
  # Or columns like "Code", "Frequency", "Memo"
  maxqda_sheets <- c("code system", "document system")
  if (any(maxqda_sheets %in% all_sheets)) {
    log_info("Detected MAXQDA codebook format (multi-sheet)")
    return(.parse_maxqda_codebook(sheet_data))
  }
  maxqda_cols <- c("frequency", "memo")
  if (sum(maxqda_cols %in% all_cols) >= 1 && "code" %in% all_cols) {
    log_info("Detected MAXQDA codebook format (list of codes)")
    return(.parse_maxqda_codebook(sheet_data))
  }

  # --- ATLAS.ti detection ---
  # ATLAS.ti exports: positional columns (Code, Code Definition, Code Group 1...)
  # or columns with "code group" or "groundedness"
  atlasti_indicators <- c("code group", "groundedness", "code definition")
  if (any(vapply(atlasti_indicators, function(ind) any(grepl(ind, all_cols)), logical(1)))) {
    log_info("Detected ATLAS.ti codebook format")
    return(.parse_atlasti_codebook(sheet_data))
  }

  # --- Generic fallback ---
  log_info("Using generic codebook parser for {basename(path)}")
  .parse_generic_codebook(sheet_data)
}

#' Parse NVivo Excel codebook
#' @keywords internal
.parse_nvivo_codebook <- function(sheet_data) {
  # Use first sheet (or sheet with Name/Sources/References columns)
  df <- NULL
  for (d in sheet_data) {
    if (!is.null(d) && all(c("Name") %in% names(d))) {
      df <- d
      break
    }
    # Case-insensitive fallback
    if (!is.null(d)) {
      lower_names <- tolower(names(d))
      if ("name" %in% lower_names) {
        df <- d
        break
      }
    }
  }
  if (is.null(df)) df <- sheet_data[[1]]
  if (is.null(df)) return(NULL)

  cols <- tolower(names(df))

  # Map columns
  name_col <- which(cols %in% c("name", "node", "code"))[1]
  desc_col <- which(cols %in% c("description", "comment", "memo"))[1]
  sources_col <- which(cols %in% c("sources", "files", "documents"))[1]
  refs_col <- which(cols %in% c("references", "coding references", "ref"))[1]

  if (is.na(name_col)) return(NULL)

  result <- tibble(
    code_name = as.character(df[[name_col]]),
    parent_code = NA_character_,
    frequency = if (!is.na(refs_col)) as.integer(df[[refs_col]]) else NA_integer_,
    n_sources = if (!is.na(sources_col)) as.integer(df[[sources_col]]) else NA_integer_,
    description = if (!is.na(desc_col)) as.character(df[[desc_col]]) else NA_character_,
    hierarchy_level = 0L
  )

  # Detect hierarchy from indentation (leading spaces in name) or parent column
  result <- .infer_hierarchy(result)

  # Filter out empty/NA code names

  result <- result[!is.na(result$code_name) & nchar(trimws(result$code_name)) > 0, ]
  result$code_name <- trimws(result$code_name)

  result
}

#' Parse ATLAS.ti Excel codebook
#' @keywords internal
.parse_atlasti_codebook <- function(sheet_data) {
  # ATLAS.ti Web: second tab has codes/frequencies/groups
  # ATLAS.ti Desktop: positional columns (Code, Definition, Group1, Group2...)
  df <- NULL

  # Check for second sheet with code data (ATLAS.ti Web format)
  if (length(sheet_data) >= 2 && !is.null(sheet_data[[2]])) {
    cols2 <- tolower(names(sheet_data[[2]]))
    if (any(grepl("code|name", cols2)) && any(grepl("frequency|groundedness", cols2))) {
      df <- sheet_data[[2]]
    }
  }

  # Fall back to first sheet
  if (is.null(df)) df <- sheet_data[[1]]
  if (is.null(df)) return(NULL)

  cols <- tolower(names(df))

  # Map columns
  name_col <- which(cols %in% c("code", "name", "codes"))[1]
  desc_col <- which(cols %in% c("code definition", "comment", "description", "memo"))[1]
  freq_col <- which(cols %in% c("groundedness", "frequency", "references", "quotations"))[1]
  group_col <- which(grepl("code group|group", cols))[1]

  # If no column names match, assume positional: col1=Code, col2=Definition
  if (is.na(name_col)) name_col <- 1

  result <- tibble(
    code_name = as.character(df[[name_col]]),
    parent_code = if (!is.na(group_col)) as.character(df[[group_col]]) else NA_character_,
    frequency = if (!is.na(freq_col)) suppressWarnings(as.integer(df[[freq_col]])) else NA_integer_,
    n_sources = NA_integer_,
    description = if (!is.na(desc_col)) as.character(df[[desc_col]]) else NA_character_,
    hierarchy_level = 0L
  )

  # ATLAS.ti uses prefix notation for subcodes (e.g., "benefit: creative")
  prefix_codes <- grepl(":\\s", result$code_name)
  if (any(prefix_codes)) {
    for (i in which(prefix_codes)) {
      parts <- strsplit(result$code_name[i], ":\\s*", perl = TRUE)[[1]]
      if (length(parts) >= 2) {
        result$parent_code[i] <- trimws(parts[1])
        result$hierarchy_level[i] <- 1L
      }
    }
  }

  result <- result[!is.na(result$code_name) & nchar(trimws(result$code_name)) > 0, ]
  result$code_name <- trimws(result$code_name)

  result
}

#' Parse MAXQDA Excel codebook
#' @keywords internal
.parse_maxqda_codebook <- function(sheet_data) {
  # Try "Code System" sheet first (Project Components export)
  df <- NULL
  for (sheet_name in names(sheet_data)) {
    if (grepl("code system", tolower(sheet_name))) {
      df <- sheet_data[[sheet_name]]
      break
    }
  }

  # Fall back to first sheet
  if (is.null(df)) df <- sheet_data[[1]]
  if (is.null(df)) return(NULL)

  cols <- tolower(names(df))

  # Map columns
  name_col <- which(cols %in% c("code", "name", "code name"))[1]
  desc_col <- which(cols %in% c("memo", "description", "code memo"))[1]
  freq_col <- which(cols %in% c("frequency", "coded segments", "references"))[1]
  parent_col <- which(cols %in% c("parent", "parent code", "category"))[1]

  if (is.na(name_col)) name_col <- 1

  result <- tibble(
    code_name = as.character(df[[name_col]]),
    parent_code = if (!is.na(parent_col)) as.character(df[[parent_col]]) else NA_character_,
    frequency = if (!is.na(freq_col)) suppressWarnings(as.integer(df[[freq_col]])) else NA_integer_,
    n_sources = NA_integer_,
    description = if (!is.na(desc_col)) as.character(df[[desc_col]]) else NA_character_,
    hierarchy_level = 0L
  )

  result <- .infer_hierarchy(result)

  result <- result[!is.na(result$code_name) & nchar(trimws(result$code_name)) > 0, ]
  result$code_name <- trimws(result$code_name)

  result
}

#' Parse generic codebook (CSV or Excel with standard columns)
#' @keywords internal
.parse_generic_codebook <- function(sheet_data) {
  df <- sheet_data[[1]]
  if (is.null(df)) return(NULL)

  cols <- tolower(names(df))

  # Try to map standard column names
  name_col <- which(cols %in% c("code_name", "code", "name", "node", "label"))[1]
  parent_col <- which(cols %in% c("parent_code", "parent", "category", "theme", "group"))[1]
  freq_col <- which(cols %in% c("frequency", "references", "count", "n_references"))[1]
  sources_col <- which(cols %in% c("n_sources", "sources", "n_entries", "documents", "files"))[1]
  desc_col <- which(cols %in% c("description", "definition", "memo", "comment"))[1]

  if (is.na(name_col)) {
    log_warn("Could not identify code name column. Expected one of: code_name, code, name, node, label")
    return(NULL)
  }

  result <- tibble(
    code_name = as.character(df[[name_col]]),
    parent_code = if (!is.na(parent_col)) as.character(df[[parent_col]]) else NA_character_,
    frequency = if (!is.na(freq_col)) suppressWarnings(as.integer(df[[freq_col]])) else NA_integer_,
    n_sources = if (!is.na(sources_col)) suppressWarnings(as.integer(df[[sources_col]])) else NA_integer_,
    description = if (!is.na(desc_col)) as.character(df[[desc_col]]) else NA_character_,
    hierarchy_level = 0L
  )

  result <- .infer_hierarchy(result)

  result <- result[!is.na(result$code_name) & nchar(trimws(result$code_name)) > 0, ]
  result$code_name <- trimws(result$code_name)

  result
}

#' Infer hierarchy from indentation or parent_code column
#' @keywords internal
.infer_hierarchy <- function(codebook) {
  # If parent_code is populated, use it to set hierarchy_level
  if (any(!is.na(codebook$parent_code) & nchar(trimws(codebook$parent_code)) > 0)) {
    has_parent <- !is.na(codebook$parent_code) & nchar(trimws(codebook$parent_code)) > 0
    codebook$hierarchy_level[has_parent] <- 1L
    return(codebook)
  }

  # Try to detect indentation in code names (NVivo-style)
  leading_spaces <- nchar(codebook$code_name) - nchar(trimws(codebook$code_name, which = "left"))
  if (max(leading_spaces) > 0) {
    # Use indentation levels
    unique_indents <- sort(unique(leading_spaces))
    indent_map <- setNames(seq_along(unique_indents) - 1L, unique_indents)
    codebook$hierarchy_level <- as.integer(indent_map[as.character(leading_spaces)])

    # Infer parent codes from hierarchy
    current_parents <- character(max(codebook$hierarchy_level) + 1)
    for (i in seq_len(nrow(codebook))) {
      level <- codebook$hierarchy_level[i]
      name <- trimws(codebook$code_name[i])
      if (level > 0 && nchar(current_parents[level]) > 0) {
        codebook$parent_code[i] <- current_parents[level]
      }
      current_parents[level + 1] <- name
      # Clear deeper parents
      if (level + 2 <= length(current_parents)) {
        current_parents[(level + 2):length(current_parents)] <- ""
      }
    }
  }

  codebook
}

#' Compute empirical coding benchmarks from parsed QDA codebooks
#'
#' Aggregates statistics across all parsed codebooks to establish data-driven
#' thresholds for code specificity, consolidation targets, and theme structure.
#'
#' @param studies PreviousStudies object with $codebook fields populated
#' @return List of benchmarks, or NULL if no codebooks available
#' @export
compute_coding_benchmarks <- function(studies) {
  if (!inherits(studies, "PreviousStudies")) return(NULL)

  codebooks <- list()
  deep_studies <- list()
  for (s in studies$studies) {
    if (!is.null(s$codebook) && nrow(s$codebook) > 0) {
      codebooks[[s$name]] <- s$codebook
    }
    if (!is.null(s$codebook_full)) {
      deep_studies[[s$name]] <- s
    }
  }

  if (length(codebooks) == 0) {
    log_info("No codebooks found in previous studies -- skipping benchmark computation")
    return(NULL)
  }

  log_info("Computing coding benchmarks from {length(codebooks)} codebook(s) ({length(deep_studies)} with deep data)")

  all_coverage <- numeric(0)
  all_code_counts <- integer(0)
  all_word_counts <- integer(0)
  all_frequencies <- integer(0)
  all_codes_per_theme <- numeric(0)
  all_segment_lengths <- integer(0)
  all_codes_per_entry <- numeric(0)
  all_hierarchy_depths <- integer(0)
  all_discarded_pcts <- numeric(0)
  all_pct_entries_coded <- numeric(0)
  example_codes <- list()

  for (study_name in names(codebooks)) {
    cb <- codebooks[[study_name]]
    has_deep <- study_name %in% names(deep_studies)

    # Use deep codebook if available for better leaf detection
    if (has_deep) {
      cb_full <- deep_studies[[study_name]]$codebook_full
      leaf_codes <- cb_full[cb_full$is_codable & !cb_full$is_discarded, ]
      all_hierarchy_depths <- c(all_hierarchy_depths, max(cb_full$hierarchy_level))

      # Discarded code percentage
      n_discarded <- sum(cb_full$is_discarded & cb_full$is_codable)
      n_total_leaf <- sum(cb_full$is_codable)
      if (n_total_leaf > 0) {
        all_discarded_pcts <- c(all_discarded_pcts,
                                 round(100 * n_discarded / n_total_leaf, 1))
      }
    } else {
      # Simpler codebook: leaf codes have a parent or are standalone
      leaf_codes <- cb[!is.na(cb$parent_code) | cb$hierarchy_level > 0, ]
      if (nrow(leaf_codes) == 0) leaf_codes <- cb
    }

    all_code_counts <- c(all_code_counts, nrow(leaf_codes))

    # Code coverage (n_sources or frequency as proxy)
    if ("n_sources" %in% names(cb) && any(!is.na(cb$n_sources))) {
      total_sources <- max(cb$n_sources, na.rm = TRUE)
      if (total_sources > 0) {
        coverage <- cb$n_sources / total_sources
        all_coverage <- c(all_coverage, coverage[!is.na(coverage) & coverage > 0])
      }
    }
    if (any(!is.na(cb$frequency) & cb$frequency > 0)) {
      all_frequencies <- c(all_frequencies,
                            cb$frequency[!is.na(cb$frequency) & cb$frequency > 0])
    }

    # Code name word counts (leaf codes only)
    leaf_names <- if (has_deep) leaf_codes$code_name else cb$code_name
    words <- vapply(leaf_names, function(n) {
      length(strsplit(trimws(n), "\\s+")[[1]])
    }, integer(1))
    all_word_counts <- c(all_word_counts, words)

    # Codes per theme (leaf codes per parent)
    if (has_deep) {
      cb_full <- deep_studies[[study_name]]$codebook_full
      themes <- cb_full[cb_full$hierarchy_level == 0 & !cb_full$is_discarded, ]
      for (t_idx in seq_len(nrow(themes))) {
        theme_name <- themes$code_name[t_idx]
        # Count leaf codes whose hierarchy_path starts with this theme name
        prefix <- paste0(theme_name, " / ")
        codes_under <- cb_full[startsWith(cb_full$hierarchy_path, prefix) &
                                 cb_full$is_codable, ]
        if (nrow(codes_under) > 0) {
          all_codes_per_theme <- c(all_codes_per_theme, nrow(codes_under))
        }
      }
    } else if (any(!is.na(cb$parent_code) & nchar(trimws(cb$parent_code)) > 0)) {
      parent_counts <- table(cb$parent_code[!is.na(cb$parent_code) &
                                              nchar(trimws(cb$parent_code)) > 0])
      all_codes_per_theme <- c(all_codes_per_theme, as.numeric(parent_counts))
    }

    # Entry-level coding benchmarks (from deep QDPX data)
    if (has_deep && !is.null(deep_studies[[study_name]]$coding_references)) {
      refs <- deep_studies[[study_name]]$coding_references
      coded_texts <- refs$coded_text[!is.na(refs$coded_text)]
      if (length(coded_texts) > 0) {
        all_segment_lengths <- c(all_segment_lengths, nchar(coded_texts))
      }

      # Codes per entry
      codes_per_src <- table(refs$source_guid)
      all_codes_per_entry <- c(all_codes_per_entry, as.numeric(codes_per_src))

      # Percent of entries coded
      if (!is.null(deep_studies[[study_name]]$source_texts)) {
        n_sources <- nrow(deep_studies[[study_name]]$source_texts)
        n_coded <- length(unique(refs$source_guid))
        if (n_sources > 0) {
          all_pct_entries_coded <- c(all_pct_entries_coded,
                                     round(100 * n_coded / n_sources, 1))
        }
      }
    }

    # Top codes as examples (leaf codes with highest frequency)
    if (has_deep) {
      top_cb <- leaf_codes[order(-leaf_codes$frequency, na.last = TRUE), ]
    } else {
      top_cb <- cb[order(-cb$frequency, na.last = TRUE), ]
    }
    top_cb <- top_cb[!is.na(top_cb$frequency) & top_cb$frequency > 0, ]
    top_n <- head(top_cb, 10)
    for (j in seq_len(nrow(top_n))) {
      example_codes[[length(example_codes) + 1]] <- list(
        study = study_name,
        code = top_n$code_name[j],
        frequency = top_n$frequency[j]
      )
    }
  }

  benchmarks <- list(
    max_code_coverage_pct = if (length(all_coverage) > 0) round(max(all_coverage) * 100, 1) else NULL,
    median_code_coverage_pct = if (length(all_coverage) > 0) round(median(all_coverage) * 100, 1) else NULL,
    typical_code_count = if (length(all_code_counts) > 0) round(mean(all_code_counts)) else NULL,
    codes_per_theme = if (length(all_codes_per_theme) > 0) round(mean(all_codes_per_theme), 1) else NULL,
    code_word_count = if (length(all_word_counts) > 0) round(mean(all_word_counts), 1) else NULL,
    code_frequency_distribution = if (length(all_frequencies) > 0) {
      list(
        p25 = as.integer(quantile(all_frequencies, 0.25)),
        p50 = as.integer(quantile(all_frequencies, 0.50)),
        p75 = as.integer(quantile(all_frequencies, 0.75)),
        p90 = as.integer(quantile(all_frequencies, 0.90))
      )
    } else NULL,
    # New benchmarks from deep QDPX parsing
    avg_segment_length = if (length(all_segment_lengths) > 0) round(mean(all_segment_lengths)) else NULL,
    median_segment_length = if (length(all_segment_lengths) > 0) round(median(all_segment_lengths)) else NULL,
    codes_per_entry = if (length(all_codes_per_entry) > 0) round(mean(all_codes_per_entry), 1) else NULL,
    hierarchy_depth = if (length(all_hierarchy_depths) > 0) round(mean(all_hierarchy_depths)) else NULL,
    discarded_code_pct = if (length(all_discarded_pcts) > 0) round(mean(all_discarded_pcts), 1) else NULL,
    pct_entries_coded = if (length(all_pct_entries_coded) > 0) round(mean(all_pct_entries_coded), 1) else NULL,
    example_codes = example_codes,
    n_codebooks = length(codebooks)
  )

  log_info("Coding benchmarks: max_coverage={benchmarks$max_code_coverage_pct}%, ",
           "typical_codes={benchmarks$typical_code_count}, ",
           "codes_per_theme={benchmarks$codes_per_theme}, ",
           "word_count={benchmarks$code_word_count}, ",
           "segment_length={benchmarks$avg_segment_length}, ",
           "codes_per_entry={benchmarks$codes_per_entry}, ",
           "hierarchy_depth={benchmarks$hierarchy_depth}, ",
           "discarded={benchmarks$discarded_code_pct}%")

  benchmarks
}

#' Find manuscript file in a study folder
#' @keywords internal
.find_manuscript <- function(folder, filenames) {
  # Search for matching files with common extensions
  extensions <- c("docx", "pdf", "txt", "md")

  for (name_pattern in filenames) {
    for (ext in extensions) {
      # Try exact pattern match
      candidates <- list.files(folder, pattern = paste0(name_pattern, ".*\\.", ext, "$"),
                                full.names = TRUE, ignore.case = TRUE)
      if (length(candidates) > 0) {
        log_info("  Found manuscript: {basename(candidates[1])}")
        return(parse_manuscript(candidates[1]))
      }
    }
  }

  log_warn("  No manuscript found in {folder}")
  NULL
}

#' Extract text from DOCX using xml2
#' @keywords internal
.extract_docx_text <- function(file_path) {
  # Unzip the DOCX and read word/document.xml
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  tryCatch({
    utils::unzip(file_path, exdir = temp_dir)
    doc_xml_path <- file.path(temp_dir, "word", "document.xml")

    if (!file.exists(doc_xml_path)) {
      log_warn("No document.xml found in DOCX: {file_path}")
      return(NULL)
    }

    doc <- xml2::read_xml(doc_xml_path)

    # Define Word namespace
    ns <- xml2::xml_ns(doc)

    # Extract all text nodes
    text_nodes <- xml2::xml_find_all(doc, ".//w:t", ns)
    texts <- xml2::xml_text(text_nodes)

    # Join with appropriate spacing
    # Group by paragraph: find w:p elements, extract text from each
    paragraphs <- xml2::xml_find_all(doc, ".//w:p", ns)
    para_texts <- vapply(paragraphs, function(p) {
      t_nodes <- xml2::xml_find_all(p, ".//w:t", ns)
      paste(xml2::xml_text(t_nodes), collapse = "")
    }, character(1))

    # Filter empty paragraphs
    para_texts <- para_texts[nchar(trimws(para_texts)) > 0]

    full_text <- paste(para_texts, collapse = "\n")
    full_text

  }, error = function(e) {
    log_warn("Failed to extract text from DOCX: {e$message}")
    NULL
  })
}

#' Extract text from PDF
#' @keywords internal
.extract_pdf_text <- function(file_path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    log_warn("pdftools package not installed, cannot read PDF: {file_path}")
    return(NULL)
  }

  tryCatch({
    pages <- pdftools::pdf_text(file_path)
    paste(pages, collapse = "\n")
  }, error = function(e) {
    log_warn("Failed to extract text from PDF: {e$message}")
    NULL
  })
}

#' Parse filename metadata from raw data DOCX files
#'
#' Expected pattern: YYYY-MM-DD_YYYY-MM-DD_Username XXX_Rating X.X_Likes XXX.docx
#'
#' @keywords internal
.parse_filename_metadata <- function(filename) {
  # Remove extension
  base <- tools::file_path_sans_ext(filename)

  # Try to parse the structured format
  date_pattern <- "(\\d{4}-\\d{2}-\\d{2})_(\\d{4}-\\d{2}-\\d{2})"
  username_pattern <- "Username\\s+([^_]+)"
  rating_pattern <- "Rating\\s+([\\d.NA]+)"
  likes_pattern <- "Likes\\s+(\\d+)"

  date_scraped <- NA_character_
  date_posted <- NA_character_
  username <- NA_character_
  rating <- NA_real_
  likes <- NA_integer_

  date_match <- regmatches(base, regexec(date_pattern, base))[[1]]
  if (length(date_match) == 3) {
    date_scraped <- date_match[2]
    date_posted <- date_match[3]
  }

  user_match <- regmatches(base, regexec(username_pattern, base))[[1]]
  if (length(user_match) == 2) {
    username <- trimws(user_match[2])
  }

  rating_match <- regmatches(base, regexec(rating_pattern, base))[[1]]
  if (length(rating_match) == 2) {
    rating_str <- rating_match[2]
    if (!grepl("N\\.?A", rating_str, ignore.case = TRUE)) {
      rating <- suppressWarnings(as.numeric(rating_str))
    }
  }

  likes_match <- regmatches(base, regexec(likes_pattern, base))[[1]]
  if (length(likes_match) == 2) {
    likes <- suppressWarnings(as.integer(likes_match[2]))
  }

  list(
    date_scraped = date_scraped,
    date_posted = date_posted,
    username = username,
    rating = rating,
    likes = likes
  )
}

# ==============================================================================
# Cross-study qualitative synthesis
# ==============================================================================

#' Synthesize structural facts across multiple codebooks
#'
#' Produces a domain-neutral, evidence-based summary of the prior codebooks'
#' actual contents (theme names, hierarchy depth, coding style benchmarks).
#'
#' Design notes
#' ------------
#' An earlier version of this function (pre-1.0.0) injected hardcoded
#' medication/health-research opinions into the AI's learning context: a
#' regex list that matched theme names to predefined "recurring categories"
#' (side effects, treatment efficacy, dosage timing, etc.) and an
#' unconditional narrative-arc claim ("themes were organized to tell a
#' coherent story: starting with direct treatment effects, moving to side
#' effects and complications, then broader implications...") that fired
#' whenever any prior codebook had theme descriptions. Both biased the AI
#' toward medication-research framings regardless of the user's actual
#' research domain. They have been removed.
#'
#' What this function does now: list the actual top-level theme names from
#' each prior codebook so the AI can see what was studied without being told
#' what the patterns "are". The numerical benchmarks (segment length, codes
#' per entry, discarded-code percentage) are kept because they're
#' domain-independent calibration data.
#'
#' @param hierarchies Named list of codebook hierarchy data
#' @param studies PreviousStudies object
#' @param benchmarks Computed coding benchmarks (or NULL)
#' @return Character string describing structural facts (no opinions)
#' @keywords internal
.synthesize_cross_study_patterns <- function(hierarchies, studies, benchmarks) {
  if (length(hierarchies) < 2) return("")

  parts <- character(0)

  # ---- 1. Actual top-level themes per study (factual listing, no opinions) ----
  per_study_themes <- list()
  hierarchy_depths <- integer(0)
  for (s_name in names(studies$studies)) {
    s <- studies$studies[[s_name]]
    if (!is.null(s$codebook_full)) {
      cb <- s$codebook_full
      top_themes <- cb[cb$hierarchy_level == 0 & !cb$is_discarded, ]
      if (nrow(top_themes) > 0) {
        per_study_themes[[s_name]] <- top_themes$code_name
      }
      max_depth <- suppressWarnings(max(cb$hierarchy_level[!cb$is_discarded], na.rm = TRUE))
      if (is.finite(max_depth)) hierarchy_depths <- c(hierarchy_depths, max_depth)
    }
  }

  if (length(per_study_themes) >= 2) {
    theme_listing <- vapply(names(per_study_themes), function(s_name) {
      paste0("    ", s_name, " (", length(per_study_themes[[s_name]]),
             " top-level themes): ",
             paste(per_study_themes[[s_name]], collapse = "; "))
    }, character(1))
    parts <- c(parts, paste0(
      "- Top-level theme names from each prior analysis (these are the actual ",
      "themes the human researchers identified; use them only as illustrations ",
      "of granularity, not as templates to reproduce):\n",
      paste(theme_listing, collapse = "\n")
    ))
  }

  # ---- 2. Hierarchy depth (structural fact) ----
  if (length(hierarchy_depths) > 0) {
    median_depth <- median(hierarchy_depths)
    depth_label <- switch(as.character(median_depth),
      "0" = "flat (themes only, no subthemes or sub-subthemes)",
      "1" = "two levels deep (themes -> subthemes)",
      "2" = "three levels deep (themes -> subthemes -> codes)",
      paste0(median_depth + 1L, " levels deep")
    )
    parts <- c(parts, paste0(
      "- Median codebook hierarchy depth across the prior analyses: ",
      depth_label
    ))
  }

  # ---- 3. Discarded-code percentage (data-driven, retained from prior version) ----
  if (!is.null(benchmarks$discarded_code_pct) && benchmarks$discarded_code_pct > 0) {
    parts <- c(parts, paste0(
      "- Researchers discarded ~", benchmarks$discarded_code_pct,
      "% of provisional codes from the final codebook (typically codes that ",
      "turned out to be too vague to support distinct themes)"
    ))
  }

  # ---- 4. Coding-style benchmarks (data-driven, retained from prior version) ----
  if (!is.null(benchmarks$avg_segment_length) && !is.null(benchmarks$codes_per_entry)) {
    parts <- c(parts, paste0(
      "- Coded segments averaged ~", benchmarks$avg_segment_length,
      " characters each (i.e. specific phrases or sentences, not whole entries); ",
      "~", benchmarks$codes_per_entry, " codes were applied per entry on average"
    ))
  }

  if (length(parts) == 0) return("")

  paste0(
    "## CROSS-STUDY STRUCTURAL FACTS\n",
    "Observations from ", length(hierarchies),
    " prior manually-coded analyses (presented as facts, not as patterns to ",
    "reproduce; use them to calibrate code granularity and theme structure ",
    "for the current research question):\n",
    paste(parts, collapse = "\n")
  )
}
