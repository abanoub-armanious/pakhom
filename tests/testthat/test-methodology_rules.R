# Tests for methodology rules generation + injection (T1.6,
# R/methodology_rules.R). The rules block is prepended to the system
# prompt on every ai_complete() call (per AC9 -- rules in the model's
# context-window every turn).

# ---- generate_methodology_rules: empty / missing config --------------------

test_that("generate_methodology_rules returns '' on NULL config", {
  expect_equal(generate_methodology_rules(NULL), "")
})

test_that("generate_methodology_rules returns universal rules even when mode is missing", {
  cfg <- list(methodology = list(mode = NULL))
  rules <- generate_methodology_rules(cfg)
  expect_match(rules, "Universal Tier-0")
  expect_match(rules, "Quote provenance is mandatory")
  expect_match(rules, "Participant spread")
  expect_match(rules, "Full-corpus coverage")
})

# ---- generate_methodology_rules: per-mode behavior -------------------------

test_that("generate_methodology_rules: reflexive_scaffold rules forbid theme proposal", {
  cfg <- list(methodology = list(mode = "reflexive_scaffold"))
  rules <- generate_methodology_rules(cfg)
  expect_match(rules, "Mode 1 \\(Reflexive Scaffold\\)")
  expect_match(rules, "NEVER propose theme names")
  expect_match(rules, "NEVER interpret meaning")
  expect_match(rules, "Refusal is a first-class output")
})

test_that("generate_methodology_rules: codebook_collaborative allows codes + theme proposals", {
  # An earlier rule said "model does NOT name themes"; the AI now
  # proposes theme + subtheme names in the post-clustering labeling pass.
  # The refreshed rule reflects this while keeping researcher-as-final-author.
  cfg <- list(methodology = list(mode = "codebook_collaborative"))
  rules <- generate_methodology_rules(cfg)
  expect_match(rules, "Mode 2 \\(Codebook Collaborative\\)")
  expect_match(rules, "MAY propose codes")
  # the rules ask the AI to articulate each theme's central organizing concept
  expect_match(rules, "central organizing concept")
  # Researcher can rename/merge/split/delete at the end
  expect_match(rules, "rename, merge, split, or delete")
})

test_that("generate_methodology_rules: framework_applied constrains to framework verbatim", {
  cfg <- list(methodology = list(mode = "framework_applied"))
  rules <- generate_methodology_rules(cfg)
  expect_match(rules, "Mode 3 \\(Framework Applied\\)")
  expect_match(rules, "verbatim")
  expect_match(rules, "Flag entries that resist the framework")
})

test_that("generate_methodology_rules: unknown mode emits universal rules with warning", {
  cfg <- list(methodology = list(mode = "free_for_all"))
  rules <- generate_methodology_rules(cfg)
  # Mode block is empty for unknown modes; universal rules still present
  expect_false(grepl("Mode 1|Mode 2|Mode 3", rules))
  expect_match(rules, "Universal Tier-0")
})

# ---- generate_methodology_rules: memos block -------------------------------

test_that("generate_methodology_rules: memos block included when memos enabled", {
  cfg <- list(
    methodology = list(mode = "reflexive_scaffold"),
    memos = list(enabled = TRUE,
                 mandatory_for_modes = "reflexive_scaffold",
                 prompt_at = c("after_coding", "after_themes"))
  )
  rules <- generate_methodology_rules(cfg)
  expect_match(rules, "Reflexive memos")
  expect_match(rules, "after_coding")
})

test_that("generate_methodology_rules: memos auto-derived from mandatory_for_modes when enabled=NULL", {
  # NULL means "derive from mandatory_for_modes". If the current mode is
  # in mandatory_for_modes, memos are enabled.
  cfg_yes <- list(
    methodology = list(mode = "reflexive_scaffold"),
    memos = list(enabled = NULL,
                 mandatory_for_modes = "reflexive_scaffold",
                 prompt_at = "after_coding")
  )
  expect_match(generate_methodology_rules(cfg_yes), "Reflexive memos")

  cfg_no <- list(
    methodology = list(mode = "framework_applied"),
    memos = list(enabled = NULL,
                 mandatory_for_modes = "reflexive_scaffold",
                 prompt_at = "after_coding")
  )
  rules_no <- generate_methodology_rules(cfg_no)
  expect_false(grepl("Reflexive memos", rules_no))
})

test_that("generate_methodology_rules: memos block omitted when enabled=FALSE", {
  cfg <- list(
    methodology = list(mode = "reflexive_scaffold"),
    memos = list(enabled = FALSE,
                 mandatory_for_modes = "reflexive_scaffold",
                 prompt_at = "after_coding")
  )
  rules <- generate_methodology_rules(cfg)
  expect_false(grepl("Reflexive memos", rules))
})

# ---- generate_methodology_rules: reflexivity block from study cfg ----------

test_that("generate_methodology_rules: positionality / paradigm / notes surface in rules", {
  cfg <- list(
    methodology = list(mode = "reflexive_scaffold"),
    study = list(
      researcher_positionality = "Organizational researcher with remote-work expertise",
      research_paradigm        = "critical realist",
      reflexive_notes          = "Aware of my outsider status to Reddit communities."
    )
  )
  rules <- generate_methodology_rules(cfg)
  expect_match(rules, "Researcher reflexivity")
  expect_match(rules, "Organizational researcher")
  expect_match(rules, "critical realist")
  expect_match(rules, "outsider status")
})

test_that("generate_methodology_rules: reflexivity section omitted when no fields supplied", {
  cfg <- list(methodology = list(mode = "reflexive_scaffold"))
  rules <- generate_methodology_rules(cfg)
  expect_false(grepl("Researcher reflexivity", rules))
})

# ---- write_methodology_rules ----------------------------------------------

test_that("write_methodology_rules creates rules/methodology_rules.md", {
  d <- withr::local_tempdir()
  cfg <- list(methodology = list(mode = "reflexive_scaffold"))
  path <- write_methodology_rules(cfg, d)
  expect_true(file.exists(path))
  expect_match(readLines(path, warn = FALSE) |> paste(collapse = "\n"),
               "Mode 1 \\(Reflexive Scaffold\\)")
})

test_that("write_methodology_rules returns NULL when there are no rules to write", {
  d <- withr::local_tempdir()
  expect_null(write_methodology_rules(NULL, d))
})

# ---- create_ai_provider attaches methodology_rules from config -------------

test_that("create_ai_provider attaches methodology_rules when config has methodology block", {
  withr::local_envvar(OPENAI_API_KEY = "sk-test-key")
  cfg <- list(
    openai = list(api_key = "sk-test-key"),
    methodology = list(mode = "reflexive_scaffold")
  )
  p <- create_ai_provider("openai", cfg)
  expect_true(nzchar(p$methodology_rules))
  expect_match(p$methodology_rules, "Mode 1 \\(Reflexive Scaffold\\)")
})

test_that("create_ai_provider sets methodology_rules='' when config lacks methodology", {
  withr::local_envvar(OPENAI_API_KEY = "sk-test-key")
  p <- create_ai_provider("openai")
  expect_equal(p$methodology_rules, "")
})

# ---- ai_complete prepends provider$methodology_rules to system prompt ------

test_that("ai_complete prepends methodology_rules to system_prompt", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .openai_completion = function(provider, prompt, system_prompt, model,
                                    temperature, max_tokens, json_mode,
                                    response_schema = NULL, documents = NULL) {
      captured$system_prompt <- system_prompt
      list(
        content = "ok", model = "m",
        usage = list(prompt_tokens = 1L, completion_tokens = 1L, total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash = "h", request_id = "r", citations = list()
      )
    },
    .package = "pakhom"
  )

  prov <- mock_provider("openai")
  prov$methodology_rules <- "INJECTED RULES BLOCK"
  ai_complete(prov, prompt = "p", system_prompt = "task-specific prompt")

  expect_match(captured$system_prompt, "INJECTED RULES BLOCK")
  expect_match(captured$system_prompt, "task-specific prompt")
  # Rules come first
  expect_lt(regexpr("INJECTED RULES BLOCK", captured$system_prompt),
            regexpr("task-specific prompt", captured$system_prompt))
})

test_that("ai_complete leaves system_prompt untouched when methodology_rules is empty", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .openai_completion = function(provider, prompt, system_prompt, model,
                                    temperature, max_tokens, json_mode,
                                    response_schema = NULL, documents = NULL) {
      captured$system_prompt <- system_prompt
      list(
        content = "ok", model = "m",
        usage = list(prompt_tokens = 1L, completion_tokens = 1L, total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash = "h", request_id = "r", citations = list()
      )
    },
    .package = "pakhom"
  )

  prov <- mock_provider("openai")
  prov$methodology_rules <- ""  # explicit empty
  ai_complete(prov, prompt = "p", system_prompt = "task-specific")

  expect_equal(captured$system_prompt, "task-specific")
})

test_that("ai_complete handles NULL system_prompt + non-empty rules (rules become the prompt)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .openai_completion = function(provider, prompt, system_prompt, model,
                                    temperature, max_tokens, json_mode,
                                    response_schema = NULL, documents = NULL) {
      captured$system_prompt <- system_prompt
      list(
        content = "ok", model = "m",
        usage = list(prompt_tokens = 1L, completion_tokens = 1L, total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash = "h", request_id = "r", citations = list()
      )
    },
    .package = "pakhom"
  )

  prov <- mock_provider("openai")
  prov$methodology_rules <- "RULES"
  ai_complete(prov, prompt = "p", system_prompt = NULL)

  expect_equal(captured$system_prompt, "RULES")
})

# ---- AC9 contract test: rules injection cannot be disabled -----------------

test_that("AC9 enforcement: rules injection cannot be turned off via task or config knob", {
  # AC9 is "mode rules generated from config and injected to model context
  # every turn". A regression that adds a `disable_rules` flag would
  # weaken this. The contract: regardless of any task-specific or
  # call-specific override, provider$methodology_rules drives injection.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .openai_completion = function(provider, prompt, system_prompt, model,
                                    temperature, max_tokens, json_mode,
                                    response_schema = NULL, documents = NULL) {
      captured$system_prompt <- system_prompt
      list(content = "ok", model = "m",
           usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                        total_tokens = 2L),
           finish_reason = "stop", raw_response = list(),
           prompt_hash = "h", request_id = "r", citations = list())
    },
    .package = "pakhom"
  )

  prov <- mock_provider("openai")
  prov$methodology_rules <- "MUST APPEAR"
  # Try every task name we have
  for (task in c("coding", "theming", "sentiment", "synthesis", "review", "insight")) {
    captured$system_prompt <- ""
    ai_complete(prov, prompt = "p", system_prompt = NULL, task = task)
    expect_match(captured$system_prompt, "MUST APPEAR",
                 info = sprintf("Task=%s should not be able to suppress rules", task))
  }
})

# ============================================================================
# Inductive-pass variant of the Mode 3 rule + methodology_override
# ============================================================================

test_that("codebook_collaborative rule reflects the v2 grouping posture (no stale v1 mechanics)", {
  # Mode 2 lets the AI PROPOSE codes + cluster-level groupings while the
  # researcher remains the deliverable's author. The injected + archived rule
  # text describes the methodological POSTURE -- group codes, judge a shared
  # central organizing concept, AI-decided convergence, label after grouping --
  # NOT the retired v1 HAC-dendrogram implementation (the production v2
  # algorithm does not walk a dendrogram, so the rules must not claim it does).
  cfg <- list(methodology = list(mode = "codebook_collaborative"))
  rules <- generate_methodology_rules(cfg)
  # Affirmative: AI groups codes + judges a shared central organizing concept
  expect_match(rules, "groups codes into conceptual clusters")
  expect_match(rules, "central organizing concept")
  # C2: groups, never combines codes into new codes
  expect_match(rules, "never combines them into new codes")
  # Still researcher-final: review pass can rename / merge / split / delete
  expect_match(rules, "rename, merge, split, or delete")
  # Symmetric (anti-consolidation-bias) framing, not a merge-biased prompt
  expect_match(rules, "symmetric")
  # Regression guard (publication fix): NO stale v1 HAC/dendrogram
  # mechanics leak into the rules that are archived + injected on every call.
  expect_false(grepl("HAC|dendrogram|internal node|one-level-deeper|tree walk", rules))
})

test_that("framework_applied default rule still forbids new construct generation", {
  cfg <- list(methodology = list(mode = "framework_applied"))
  rules <- generate_methodology_rules(cfg, inductive_pass = FALSE)
  # Default Mode 3 must still tell the AI not to generate new framework
  # constructs during the deductive coding pass.
  expect_match(rules, "Do NOT generate new framework constructs")
  # And the rule header reads "framework_applied" without inductive suffix
  expect_match(rules, "## Mode rules \\(framework_applied\\)")
})

test_that("framework_applied inductive variant omits 'do NOT generate' + permits new codes", {
  cfg <- list(methodology = list(mode = "framework_applied"))
  rules <- generate_methodology_rules(cfg, inductive_pass = TRUE)
  # The inductive variant must NOT say "Do NOT generate new framework constructs"
  expect_false(grepl("Do NOT generate new framework constructs", rules))
  # It must affirmatively say to generate inductive codes
  expect_match(rules, "Generate inductive codes")
  # Header includes the inductive-pass suffix
  expect_match(rules, "## Mode rules \\(framework_applied -- inductive pass\\)")
  # AC2 preservation: framework spec NOT mutated
  expect_match(rules, "do NOT mutate the framework spec|framework definition is fixed")
})

test_that("inductive_pass = TRUE is a no-op for non-Mode-3 modes", {
  # For Mode 1 / Mode 2 the inductive flag is a no-op -- there is no
  # alternate rule body AND no header suffix (the suffix is suppressed
  # so the rule block is bit-identical to the default-pass output).
  # This contract matters because the header is the AI's first cue
  # about which rule variant is in force; a misleading suffix would
  # signal a non-existent semantic switch.
  for (mode in c("reflexive_scaffold", "codebook_collaborative")) {
    cfg <- list(methodology = list(mode = mode))
    default_rules   <- generate_methodology_rules(cfg, inductive_pass = FALSE)
    inductive_rules <- generate_methodology_rules(cfg, inductive_pass = TRUE)
    expect_equal(default_rules, inductive_rules,
                 info = sprintf("mode=%s should be bit-identical with inductive_pass=TRUE", mode))
  }
})

test_that("ai_complete uses methodology_override when supplied (overrides provider rules)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .openai_completion = function(provider, prompt, system_prompt, model,
                                    temperature, max_tokens, json_mode,
                                    response_schema = NULL, documents = NULL) {
      captured$system_prompt <- system_prompt
      list(content = "ok", model = "m",
           usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                        total_tokens = 2L),
           finish_reason = "stop", raw_response = list(),
           prompt_hash = "h", request_id = "r", citations = list())
    },
    .package = "pakhom"
  )
  prov <- mock_provider("openai")
  prov$methodology_rules <- "DEFAULT RULES BLOCK"

  # With override: the override text replaces the provider's default rules
  ai_complete(prov, prompt = "p", system_prompt = "task-prompt",
              methodology_override = "INDUCTIVE OVERRIDE")
  expect_match(captured$system_prompt, "INDUCTIVE OVERRIDE")
  expect_false(grepl("DEFAULT RULES BLOCK", captured$system_prompt))

  # Without override: provider rules apply as before (back-compat)
  captured$system_prompt <- ""
  ai_complete(prov, prompt = "p", system_prompt = "task-prompt")
  expect_match(captured$system_prompt, "DEFAULT RULES BLOCK")
  expect_false(grepl("INDUCTIVE OVERRIDE", captured$system_prompt))
})

test_that("methodology_override with empty string falls through cleanly", {
  # A caller passing methodology_override = "" should produce a
  # system_prompt without any rules prefix (matches the empty-default
  # behavior; not a back-door to the provider rules).
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .openai_completion = function(provider, prompt, system_prompt, model,
                                    temperature, max_tokens, json_mode,
                                    response_schema = NULL, documents = NULL) {
      captured$system_prompt <- system_prompt
      list(content = "ok", model = "m",
           usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                        total_tokens = 2L),
           finish_reason = "stop", raw_response = list(),
           prompt_hash = "h", request_id = "r", citations = list())
    },
    .package = "pakhom"
  )
  prov <- mock_provider("openai")
  prov$methodology_rules <- "PROVIDER DEFAULT"
  ai_complete(prov, prompt = "p", system_prompt = "task-prompt",
              methodology_override = "")
  expect_equal(captured$system_prompt, "task-prompt")
  expect_false(grepl("PROVIDER DEFAULT", captured$system_prompt))
})
