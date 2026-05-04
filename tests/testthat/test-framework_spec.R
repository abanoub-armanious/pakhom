# Tests for framework specification loading + validation (Sprint-4 M3.1,
# R/framework_spec.R). Mode 3 (Framework Applied) requires a researcher-
# supplied theoretical framework that the AI applies verbatim. The schema
# validator is the gatekeeper -- malformed specs must fail loud, not
# silently produce a Mode 3 run that uses garbage constructs.

# ---- load_framework_spec: built-in frameworks -------------------------------

test_that("load_framework_spec loads each built-in framework", {
  for (alias in list_builtin_frameworks()) {
    spec <- load_framework_spec(alias)
    expect_s3_class(spec, "FrameworkSpec")
    expect_true(nzchar(spec$name))
    expect_gte(length(spec$constructs), 3L)
  }
})

test_that("load_framework_spec(tpb): construct ids match TPB canon", {
  spec <- load_framework_spec("tpb")
  expect_setequal(
    spec$construct_ids,
    c("attitude", "subjective_norm", "perceived_behavioral_control",
      "intention", "behavior")
  )
})

test_that("load_framework_spec(comb): COM-B has 7 constructs (4 sub-domains + 1 behavior)", {
  spec <- load_framework_spec("comb")
  # capability_physical, capability_psychological, opportunity_physical,
  # opportunity_social, motivation_reflective, motivation_automatic, behavior
  expect_equal(length(spec$constructs), 7L)
})

test_that("load_framework_spec(tdf): TDF has 14 domains", {
  spec <- load_framework_spec("tdf")
  expect_equal(length(spec$constructs), 14L)
})

test_that("list_builtin_frameworks returns the documented set", {
  expect_setequal(list_builtin_frameworks(), c("tpb", "comb", "tdf"))
})

# ---- load_framework_spec: case-insensitive aliases ---------------------------

test_that("built-in framework aliases are case-insensitive", {
  spec1 <- load_framework_spec("TPB")
  spec2 <- load_framework_spec("Tpb")
  spec3 <- load_framework_spec("tpb")
  expect_identical(spec1$construct_ids, spec3$construct_ids)
  expect_identical(spec2$construct_ids, spec3$construct_ids)
})

# ---- load_framework_spec: file-based (user-supplied) -------------------------

.write_yaml <- function(content, path) {
  writeLines(content, path)
  invisible(path)
}

test_that("load_framework_spec accepts user-supplied YAML file", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "custom.yaml")
  .write_yaml(c(
    "framework:",
    "  name: 'Custom Test Framework'",
    "  epistemic_stance: positivist",
    "  anomaly_handling: bracket",
    "  constructs:",
    "    - id: c1",
    "      name: 'Construct One'",
    "      description: 'First test construct'",
    "    - id: c2",
    "      name: 'Construct Two'",
    "      description: 'Second test construct'"
  ), yaml_path)

  spec <- load_framework_spec(yaml_path)
  expect_equal(spec$name, "Custom Test Framework")
  expect_equal(length(spec$constructs), 2L)
  expect_equal(spec$epistemic_stance, "positivist")
  expect_equal(spec$anomaly_handling, "bracket")
})

test_that("load_framework_spec accepts user-supplied JSON file", {
  d <- withr::local_tempdir()
  json_path <- file.path(d, "custom.json")
  jsonlite::write_json(list(framework = list(
    name = "JSON Framework",
    epistemic_stance = "constructionist",
    anomaly_handling = "extend",
    constructs = list(
      list(id = "j1", name = "J One", description = "First"),
      list(id = "j2", name = "J Two", description = "Second")
    )
  )), json_path, auto_unbox = TRUE, pretty = TRUE)

  spec <- load_framework_spec(json_path)
  expect_equal(spec$name, "JSON Framework")
  expect_equal(length(spec$constructs), 2L)
})

# ---- load_framework_spec: validation failures --------------------------------

test_that("load_framework_spec rejects non-string path", {
  expect_error(load_framework_spec(NULL), "non-empty string")
  expect_error(load_framework_spec(123), "non-empty string")
  expect_error(load_framework_spec(c("a", "b")), "non-empty string")
})

test_that("load_framework_spec errors on missing file", {
  expect_error(load_framework_spec("/no/such/file.yaml"), "file not found")
})

test_that("load_framework_spec rejects unsupported extensions", {
  d <- withr::local_tempdir()
  txt_path <- file.path(d, "framework.txt")
  writeLines("framework: foo", txt_path)
  expect_error(load_framework_spec(txt_path), "Unsupported framework spec extension")
})

test_that("load_framework_spec errors when top-level framework: block missing", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "bad.yaml")
  .write_yaml("not_framework:\n  name: x", yaml_path)
  expect_error(load_framework_spec(yaml_path), "missing top-level")
})

test_that("load_framework_spec errors when constructs missing or empty", {
  d <- withr::local_tempdir()

  no_constructs <- file.path(d, "nc.yaml")
  .write_yaml(c("framework:", "  name: F"), no_constructs)
  expect_error(load_framework_spec(no_constructs), "non-empty list")

  empty_constructs <- file.path(d, "ec.yaml")
  .write_yaml(c("framework:", "  name: F", "  constructs: []"), empty_constructs)
  expect_error(load_framework_spec(empty_constructs), "non-empty list")
})

test_that("load_framework_spec rejects construct with missing id", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "noid.yaml")
  .write_yaml(c(
    "framework:",
    "  name: F",
    "  constructs:",
    "    - name: Missing-ID Construct",
    "      description: oops"
  ), yaml_path)
  expect_error(load_framework_spec(yaml_path), "id must be a non-empty string")
})

test_that("load_framework_spec rejects construct with missing description", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "nodesc.yaml")
  .write_yaml(c(
    "framework:",
    "  name: F",
    "  constructs:",
    "    - id: c1",
    "      name: First"
  ), yaml_path)
  expect_error(load_framework_spec(yaml_path), "description")
})

test_that("load_framework_spec rejects duplicate construct ids", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "dup.yaml")
  .write_yaml(c(
    "framework:",
    "  name: F",
    "  constructs:",
    "    - id: c1",
    "      name: First",
    "      description: ''",
    "    - id: c1",
    "      name: Second",
    "      description: ''"
  ), yaml_path)
  expect_error(load_framework_spec(yaml_path), "Duplicate construct id")
})

test_that("load_framework_spec rejects invalid epistemic_stance", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "stance.yaml")
  .write_yaml(c(
    "framework:",
    "  name: F",
    "  epistemic_stance: martian",
    "  constructs:",
    "    - id: c1",
    "      name: First",
    "      description: ''"
  ), yaml_path)
  expect_error(load_framework_spec(yaml_path), "epistemic_stance.*invalid")
})

test_that("load_framework_spec rejects invalid anomaly_handling", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "anom.yaml")
  .write_yaml(c(
    "framework:",
    "  name: F",
    "  anomaly_handling: ignore",
    "  constructs:",
    "    - id: c1",
    "      name: First",
    "      description: ''"
  ), yaml_path)
  expect_error(load_framework_spec(yaml_path), "anomaly_handling.*invalid")
})

test_that("load_framework_spec defaults epistemic_stance + anomaly_handling sensibly", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "defaults.yaml")
  .write_yaml(c(
    "framework:",
    "  name: F",
    "  constructs:",
    "    - id: c1",
    "      name: First",
    "      description: 'one'"
  ), yaml_path)
  spec <- load_framework_spec(yaml_path)
  # Defaults are conservative: constructionist (allows interpretation),
  # bracket (don't auto-extend the framework)
  expect_equal(spec$epistemic_stance, "constructionist")
  expect_equal(spec$anomaly_handling, "bracket")
})

test_that("load_framework_spec accepts and stores citations as character vector", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "cite.yaml")
  .write_yaml(c(
    "framework:",
    "  name: F",
    "  citations:",
    "    - 'First citation'",
    "    - 'Second citation'",
    "  constructs:",
    "    - id: c1",
    "      name: First",
    "      description: ''"
  ), yaml_path)
  spec <- load_framework_spec(yaml_path)
  expect_equal(spec$citations, c("First citation", "Second citation"))
})

test_that("load_framework_spec preserves example_indicators per construct", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "ind.yaml")
  .write_yaml(c(
    "framework:",
    "  name: F",
    "  constructs:",
    "    - id: c1",
    "      name: First",
    "      description: ''",
    "      example_indicators: ['I plan', 'I will', 'going to']"
  ), yaml_path)
  spec <- load_framework_spec(yaml_path)
  expect_equal(spec$constructs[[1]]$example_indicators,
               c("I plan", "I will", "going to"))
})

# ---- print method ---------------------------------------------------------

test_that("print.FrameworkSpec produces multi-line summary", {
  spec <- load_framework_spec("tpb")
  out <- capture.output(print(spec))
  expect_true(any(grepl("FrameworkSpec: Theory of Planned Behavior", out)))
  expect_true(any(grepl("Constructs:.*5", out)))
  expect_true(any(grepl("attitude", out)))
})

# ---- framework_prompt_block -------------------------------------------------

test_that("framework_prompt_block emits the canonical Mode 3 system prefix", {
  spec <- load_framework_spec("tpb")
  block <- framework_prompt_block(spec)
  expect_match(block, "THEORETICAL FRAMEWORK")
  expect_match(block, "Theory of Planned Behavior")
  expect_match(block, "epistemic stance: positivist")
  expect_match(block, "anomaly handling: bracket")
  # Each TPB construct id should appear in the block
  for (id in spec$construct_ids) {
    expect_match(block, sprintf("\\[%s\\]", id))
  }
  # Anti-fabrication-style instruction present
  expect_match(block, "ONLY permitted code names")
  expect_match(block, "anomaly")
})

test_that("framework_prompt_block validates input class", {
  expect_error(framework_prompt_block(list(name = "fake")), "FrameworkSpec")
})

test_that("framework_prompt_block omits indicators line when none supplied", {
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "noinds.yaml")
  .write_yaml(c(
    "framework:",
    "  name: F",
    "  constructs:",
    "    - id: c1",
    "      name: First",
    "      description: 'one'"
  ), yaml_path)
  spec <- load_framework_spec(yaml_path)
  block <- framework_prompt_block(spec)
  expect_false(grepl("Example indicators", block))
})

# ---- archive_framework_spec (phase 32 / audit H1 + H2) -------------------

test_that("archive_framework_spec writes archive + sha256 hash anchored to source", {
  spec <- load_framework_spec("tpb")
  d <- withr::local_tempdir()
  arch <- archive_framework_spec(spec, d)
  expect_true(file.exists(arch$path))
  # Phase 37 audit (AC4 stamp): arch$hash anchors replay-equivalence
  # to the SOURCE spec the user supplied, not the post-stamp archive
  # (which has a methodology comment line prepended). The hash field
  # is therefore equal to digest(source) regardless of whether the
  # archive has been stamped.
  expect_equal(
    digest::digest(file = spec$source_path, algo = "sha256",
                    serialize = FALSE),
    arch$hash
  )
  expect_equal(arch$relative_path, "framework_applied.yaml")
  expect_equal(arch$name, "Theory of Planned Behavior")
  expect_equal(arch$epistemic_stance, "positivist")
  expect_equal(arch$anomaly_handling, "bracket")
  expect_equal(arch$n_constructs, 5L)
})

test_that("archive_framework_spec stamps methodology + run_id + source-sha256 into the YAML header (AC4)", {
  # Phase 37 audit (AC MEDIUM): archived framework_applied.yaml must
  # carry the methodology stamp at the artifact level so a reviewer
  # auditing a single file sees the mode declaration even out of run-
  # dir context. The stamp is a `#` YAML comment, which yaml::yaml.load
  # strips -- so the parsed FrameworkSpec is unchanged.
  spec <- load_framework_spec("tpb")
  d <- withr::local_tempdir()
  arch <- archive_framework_spec(spec, d, run_id = "test-run-id")
  body <- readLines(arch$path, warn = FALSE)
  expect_match(body[1L], "^# methodology:")
  expect_match(body[1L], "M3 - Framework Applied", fixed = TRUE)
  expect_match(body[1L], "test-run-id", fixed = TRUE)
  expect_match(body[2L], "^# source-sha256:")
  expect_match(body[2L], substr(arch$hash, 1, 16), fixed = TRUE)
  # YAML parser strips comments -- archive content is still loadable
  reloaded <- load_framework_spec(arch$path)
  expect_equal(reloaded$name, "Theory of Planned Behavior")
  expect_equal(reloaded$construct_ids, spec$construct_ids)
})

test_that("archive_framework_spec preserves source extension (.json)", {
  d <- withr::local_tempdir()
  src <- file.path(d, "spec.json")
  jsonlite::write_json(list(framework = list(
    name = "JsonFramework",
    constructs = list(
      list(id = "c1", name = "C One", description = "first construct")
    ),
    epistemic_stance = "constructionist",
    anomaly_handling = "extend"
  )), src, pretty = TRUE, auto_unbox = TRUE)
  spec <- load_framework_spec(src)
  out <- withr::local_tempdir()
  arch <- archive_framework_spec(spec, out)
  expect_equal(arch$relative_path, "framework_applied.json")
  expect_true(file.exists(file.path(out, "framework_applied.json")))
})

test_that("archive_framework_spec rejects spec with missing source_path", {
  # Construct a spec with a NA source_path (shouldn't happen via
  # load_framework_spec, but defending against direct construction)
  spec <- structure(list(
    name = "Fake", citations = character(0),
    epistemic_stance = "mixed", anomaly_handling = "bracket",
    constructs = list(list(id = "c", name = "C", description = "d",
                              example_indicators = character(0))),
    construct_ids = "c", source_path = NA_character_,
    schema_version = "1.0.0"
  ), class = "FrameworkSpec")
  d <- withr::local_tempdir()
  expect_error(archive_framework_spec(spec, d),
               "source_path.*missing")
})

test_that("archive_framework_spec rejects non-FrameworkSpec input", {
  d <- withr::local_tempdir()
  expect_error(archive_framework_spec(list(name = "fake"), d),
               "FrameworkSpec")
})

test_that("archive_framework_spec creates run_dir if missing", {
  spec <- load_framework_spec("tpb")
  d <- file.path(withr::local_tempdir(), "fresh", "subdir")
  expect_false(dir.exists(d))
  arch <- archive_framework_spec(spec, d)
  expect_true(dir.exists(d))
  expect_true(file.exists(arch$path))
})

test_that("archive_framework_spec hash is deterministic across calls", {
  spec <- load_framework_spec("tpb")
  d1 <- withr::local_tempdir()
  d2 <- withr::local_tempdir()
  a1 <- archive_framework_spec(spec, d1)
  a2 <- archive_framework_spec(spec, d2)
  expect_equal(a1$hash, a2$hash)
})

test_that("archive_framework_spec works for all three built-in frameworks", {
  for (name in list_builtin_frameworks()) {
    spec <- load_framework_spec(name)
    d <- withr::local_tempdir()
    arch <- archive_framework_spec(spec, d)
    expect_true(file.exists(arch$path),
                info = sprintf("framework: %s", name))
    expect_match(arch$hash, "^[0-9a-f]{64}$",
                  info = sprintf("framework: %s sha256 must be 64 hex chars",
                                 name))
  }
})

test_that("framework_construct_ids serializes as a JSON array even for single-construct frameworks", {
  # Audit L2 (phase 32): jsonlite::write_json with auto_unbox=TRUE
  # would collapse a length-1 character vector into a JSON scalar,
  # so a 1-construct framework's construct_ids would round-trip as
  # "c1" instead of ["c1"]. The phase 32 fix is to splat
  # as.list(construct_ids) into init_run_state's extras so the JSON
  # array shape is preserved regardless of length.
  d <- withr::local_tempdir()
  src <- file.path(d, "single.yaml")
  writeLines(c(
    "framework:",
    "  name: 'Single Construct'",
    "  epistemic_stance: 'mixed'",
    "  anomaly_handling: 'extend'",
    "  constructs:",
    "    - id: only_one",
    "      name: 'Only construct'",
    "      description: 'one'"
  ), src)
  spec <- load_framework_spec(src)
  arch <- archive_framework_spec(spec, d)

  # Simulate the do.call(init_run_state, c(list(...), framework_extras))
  # path: build framework_extras the way 18_pipeline.R does, then
  # write through .write_run_metadata, then read back.
  meta_extras <- list(
    framework_name             = arch$name,
    framework_hash             = arch$hash,
    framework_relative_path    = arch$relative_path,
    framework_epistemic_stance = arch$epistemic_stance,
    framework_anomaly_handling = arch$anomaly_handling,
    framework_n_constructs     = arch$n_constructs,
    framework_construct_ids    = as.list(arch$construct_ids),  # the fix
    framework_schema_version   = arch$schema_version
  )
  meta <- do.call(init_run_state, c(list(
    run_dir = d,
    run_id = "test-run",
    methodology_mode = "framework_applied"
  ), meta_extras))

  # Verify by reading back the JSON: construct_ids should be an array,
  # not a scalar string, even though it has length 1.
  raw <- jsonlite::read_json(file.path(d, "run_metadata.json"),
                                simplifyVector = FALSE)
  expect_type(raw$framework_construct_ids, "list")
  expect_length(raw$framework_construct_ids, 1L)
  expect_equal(raw$framework_construct_ids[[1L]], "only_one")
})
