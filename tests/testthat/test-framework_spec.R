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
