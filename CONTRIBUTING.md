# Contributing to pakhom

Thank you for your interest in contributing. Contributions of all kinds are
welcome: bug reports, feature requests, documentation improvements, and code.

## Reporting bugs and requesting features

Please open an issue at
<https://github.com/abanoub-armanious/pakhom/issues>. For a bug report, include:

- a minimal reproducible example (a `reprex::reprex()` is ideal);
- the output of `sessionInfo()`;
- which methodology mode (reflexive / codebook / framework) and which AI
  provider (OpenAI / Anthropic) you were using.

Because the analysis pipeline makes paid API calls, please redact any keys and,
where possible, reproduce the problem against the offline layers (statistics,
report rendering, quote provenance) that the test suite exercises without an
API key.

## Development workflow

1. Fork the repository and create a feature branch.
2. Install development dependencies: `devtools::install_dev_deps()`.
3. Make your change. Please keep the package's **architectural commitments**
   intact (see the "For methodologists / reviewers: architectural commitments" section of `README.md`):
   - the package **groups** codes into themes; it never merges them into new
     codes;
   - clustering depth is the AI's dynamic, per-dataset call, with no
     hardcoded theme or cluster counts or thresholds;
   - the report **explains** rather than **gates** (values and their n are
     shown; nothing is silently suppressed);
   - no user-facing content (taxonomies, menus, categories) is hardcoded.
4. Add or update tests under `tests/testthat/`. The suite uses testthat 3e and
   mocks every AI call, so it runs with no network access and no API key.
5. Run `devtools::document()`, `devtools::test()`, and
   `R CMD check --as-cran`. All three must be clean before you open a PR.
6. Update `NEWS.md`.
7. Open a pull request describing the change and referencing any related issue.

## Code style

The codebase follows the tidyverse style guide. All `man/` pages are generated
by roxygen2 (>= 7.3.3): edit the roxygen comments above each function, not the
`.Rd` files.

## Versioning

The package follows semantic versioning. Please do **not** change the version
number in a pull request; record user-facing changes as entries in `NEWS.md`
instead. The version is incremented and a release is tagged by the maintainer.

## Code of conduct

By participating in this project you agree to abide by its
[Code of Conduct](CODE_OF_CONDUCT.md).
