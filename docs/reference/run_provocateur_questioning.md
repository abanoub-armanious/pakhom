# Run provocateur questioning across themes (Mode 1 entry point)

Orchestrates the five (or selected) provocation categories per theme,
assembling a `ResearcherReflectionLog` that captures every provocation
issued. The log is the AI's contribution to a Mode 1 analysis – the
THEMES + CODES are the researcher's authorship, kept on the same log
object as `researcher_authored_codes / researcher_authored_themes`.

## Usage

``` r
run_provocateur_questioning(
  data,
  theme_set,
  provider,
  config = list(),
  categories = .VALID_PROVOCATION_CATEGORIES,
  audit_log = NULL,
  response_cache = NULL,
  fabrication_log = NULL,
  resume_log = NULL
)
```

## Arguments

- data:

  Tibble with std_id + std_text (standardized corpus).

- theme_set:

  ThemeSet object. Each theme drives one round of provocations. The
  researcher must have authored these themes; the provocateur does NOT
  name themes.

- provider:

  AIProvider object.

- config:

  Optional config list (used for logging/context).

- categories:

  Character vector of category names to run (default: all five).

- audit_log:

  Optional AuditLog from `init_audit_log`.

- response_cache:

  Optional ResponseCache.

- fabrication_log:

  Optional FabricationLog.

- resume_log:

  Optional `ResearcherReflectionLog` to append to (resume semantics).

## Value

A `ResearcherReflectionLog` with provocations populated.

## Details

Per AC7 (universal Tier-0): every provocation that cites verbatim
evidence runs through `verify_quote`; fabricated provocations are
dropped silently and recorded to the audit + fabrication logs.
