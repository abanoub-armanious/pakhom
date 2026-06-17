#!/usr/bin/env Rscript
# ==============================================================================
# pakhom Maintenance Health Check
# Author: Abanoub J. Armanious, MS
# ==============================================================================
#
# Run this script periodically (e.g., after R updates, dependency updates,
# or before a new release) to verify the package is in good shape.
#
# Usage:
#   Rscript inst/maintenance/health_check.R
#   -- or from R console --
#   source(system.file("maintenance", "health_check.R", package = "pakhom"))
#
# ==============================================================================

cat("==============================================================================\n")
cat("pakhom Maintenance Health Check\n")
cat("==============================================================================\n\n")

passed <- 0L
warned <- 0L
failed <- 0L

check <- function(name, expr) {
  result <- tryCatch(expr, error = function(e) e)
  if (inherits(result, "error")) {
    cat(sprintf("  FAIL  %s\n        %s\n", name, conditionMessage(result)))
    failed <<- failed + 1L
  } else if (isTRUE(result)) {
    cat(sprintf("  PASS  %s\n", name))
    passed <<- passed + 1L
  } else if (is.character(result)) {
    cat(sprintf("  WARN  %s\n        %s\n", name, result))
    warned <<- warned + 1L
  } else {
    cat(sprintf("  FAIL  %s\n        Returned FALSE\n", name))
    failed <<- failed + 1L
  }
}

# --------------------------------------------------------------------------
# 1. R version compatibility
# --------------------------------------------------------------------------
cat("1. R Environment\n")
check("R version >= 4.1.0", {
  getRversion() >= "4.1.0"
})
check("Running in correct working directory", {
  file.exists("DESCRIPTION") && grepl("pakhom", readLines("DESCRIPTION", n = 1))
})

# --------------------------------------------------------------------------
# 2. Dependencies installed and loadable
# --------------------------------------------------------------------------
cat("\n2. Dependencies\n")
desc <- read.dcf("DESCRIPTION")
imports <- trimws(strsplit(desc[, "Imports"], ",")[[1]])
imports <- gsub("\\s*\\(.*\\)", "", imports)  # strip version constraints

for (pkg in imports) {
  check(sprintf("Import: %s", pkg), {
    if (requireNamespace(pkg, quietly = TRUE)) TRUE
    else sprintf("Package '%s' is not installed", pkg)
  })
}

suggests <- trimws(strsplit(desc[, "Suggests"], ",")[[1]])
suggests <- gsub("\\s*\\(.*\\)", "", suggests)
for (pkg in suggests) {
  check(sprintf("Suggest: %s", pkg), {
    if (requireNamespace(pkg, quietly = TRUE)) TRUE
    else sprintf("Optional package '%s' is not installed (non-critical)", pkg)
  })
}

# --------------------------------------------------------------------------
# 3. Package loads without errors
# --------------------------------------------------------------------------
cat("\n3. Package Loading\n")
check("devtools::load_all() succeeds", {
  devtools::load_all(".", quiet = TRUE)
  TRUE
})

# --------------------------------------------------------------------------
# 4. Documentation builds
# --------------------------------------------------------------------------
cat("\n4. Documentation\n")
check("roxygen2 documentation generates cleanly", {
  msgs <- capture.output(devtools::document(quiet = TRUE), type = "message")
  warnings_found <- grep("warning|Warning", msgs, value = TRUE)
  if (length(warnings_found) > 0) {
    paste("Warnings:", paste(warnings_found, collapse = "; "))
  } else {
    TRUE
  }
})

# --------------------------------------------------------------------------
# 5. Tests pass
# --------------------------------------------------------------------------
cat("\n5. Test Suite\n")
check("All tests pass", {
  results <- devtools::test(quiet = TRUE, stop_on_failure = FALSE)
  n_fail <- sum(as.data.frame(results)$failed)
  n_error <- sum(as.data.frame(results)$error)
  if (n_fail + n_error > 0) {
    sprintf("%d failures, %d errors", n_fail, n_error)
  } else {
    TRUE
  }
})

# --------------------------------------------------------------------------
# 6. R CMD check (lightweight)
# --------------------------------------------------------------------------
cat("\n6. R CMD check (notes/warnings/errors)\n")
check("R CMD check passes", {
  result <- devtools::check(quiet = TRUE, args = "--no-examples --no-tests --no-vignettes")
  n_err <- length(result$errors)
  n_warn <- length(result$warnings)
  n_note <- length(result$notes)
  if (n_err > 0) {
    sprintf("%d errors: %s", n_err, paste(result$errors, collapse = "; "))
  } else if (n_warn > 0) {
    sprintf("%d warnings (0 errors, %d notes)", n_warn, n_note)
  } else if (n_note > 0) {
    sprintf("%d notes (0 errors, 0 warnings)", n_note)
  } else {
    TRUE
  }
})

# --------------------------------------------------------------------------
# 7. API connectivity (optional, non-failing)
# --------------------------------------------------------------------------
cat("\n7. API Connectivity (optional)\n")
check("OpenAI API key configured", {
  key <- Sys.getenv("OPENAI_API_KEY", "")
  if (nchar(key) > 0 && !grepl("^fake|^test|^sk-test", key)) TRUE
  else "No valid OPENAI_API_KEY found in environment (set in .Renviron)"
})
check("Anthropic API key configured", {
  key <- Sys.getenv("ANTHROPIC_API_KEY", "")
  if (nchar(key) > 0 && !grepl("^fake|^test", key)) TRUE
  else "No valid ANTHROPIC_API_KEY found in environment (optional)"
})

# --------------------------------------------------------------------------
# 8. Pandoc available (for reports)
# --------------------------------------------------------------------------
cat("\n8. External Tools\n")
check("Pandoc available", {
  pandoc <- Sys.which("pandoc")
  if (nchar(pandoc) > 0) {
    TRUE
  } else {
    # Check RStudio bundled pandoc
    rstudio_pandoc <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64/pandoc"
    if (file.exists(rstudio_pandoc)) TRUE
    else "Pandoc not found. Install it or use RStudio (which bundles it)."
  }
})

# --------------------------------------------------------------------------
# 9. Dependency freshness
# --------------------------------------------------------------------------
cat("\n9. Dependency Freshness\n")
check("No outdated critical dependencies", {
  old <- tryCatch(old.packages(), error = function(e) NULL)
  if (is.null(old)) {
    "Could not check for outdated packages (no internet?)"
  } else {
    our_deps <- c(imports, suggests)
    outdated <- old[rownames(old) %in% our_deps, , drop = FALSE]
    if (nrow(outdated) > 0) {
      sprintf("%d outdated: %s", nrow(outdated), paste(rownames(outdated), collapse = ", "))
    } else {
      TRUE
    }
  }
})

# --------------------------------------------------------------------------
# Summary
# --------------------------------------------------------------------------
cat("\n==============================================================================\n")
cat(sprintf("SUMMARY: %d passed, %d warnings, %d failed\n", passed, warned, failed))
if (failed > 0) {
  cat("STATUS: NEEDS ATTENTION\n")
} else if (warned > 0) {
  cat("STATUS: OK (with warnings)\n")
} else {
  cat("STATUS: ALL CLEAR\n")
}
cat("==============================================================================\n")
