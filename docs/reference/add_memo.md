# Add a memo to a ResearcherReflectionLog

R is pass-by-value: callers must capture the return:
`log <- add_memo(log, "...")`. The function appends the memo to
`log$memos` (preserving order) and updates `log$last_updated`. When an
`audit_log` is supplied, a `memo_added` decision is recorded so the
methodology paper has a timestamp trail of when each memo was authored
relative to AI calls.

## Usage

``` r
add_memo(log, body = NULL, ..., memo = NULL, audit_log = NULL)
```

## Arguments

- log:

  A `ResearcherReflectionLog`.

- body:

  Character: the memo's Markdown body (or pass a pre-built `Memo` object
  via `memo` instead).

- ...:

  Forwarded to
  [`make_memo`](https://abanoub-armanious.github.io/pakhom/reference/make_memo.md)
  (type, author, linked_codes, linked_themes, linked_entries,
  linked_prior_memo, timestamp, id). Ignored when `memo` is supplied.

- memo:

  Optional pre-built `Memo` object; supplying this bypasses `make_memo`
  construction. Mutually exclusive with `body`.

- audit_log:

  Optional `AuditLog`; when supplied, a `memo_added` decision is
  recorded.

## Value

The updated `ResearcherReflectionLog`.

## Details

Memos are immutable once added: there is no `update_memo` or
`delete_memo` – if a researcher needs to revise a thought, they add a
NEW memo with `linked_prior_memo` pointing at the old one. The chain is
the audit trail. This is intentional per the Birks/ Chapman/Francis 2025
"Memoing in qualitative research: two decades on" guidance that memo
evolution itself is data.

## See also

[`make_memo`](https://abanoub-armanious.github.io/pakhom/reference/make_memo.md)
(constructor);
[`persist_memos`](https://abanoub-armanious.github.io/pakhom/reference/persist_memos.md)
(write all memos to disk as Markdown with YAML frontmatter);
[`load_memos`](https://abanoub-armanious.github.io/pakhom/reference/load_memos.md)
(read them back).

## Examples

``` r
log <- create_reflection_log()

# Add a theoretical memo linked to a theme
log <- add_memo(
  log,
  body = paste0(
    "Adherence themes are over-weighted by contributor X's posts.\n\n",
    "Need to interrogate this concentration before publishing."
  ),
  type = "theoretical",
  linked_themes = "Adherence"
)

# Add an operational memo as a revision of the prior
log <- add_memo(
  log,
  body = "Merged codes med_routine + daily_pills into med_adherence.",
  type = "operational",
  linked_codes = c("med_routine", "daily_pills"),
  linked_prior_memo = log$memos[[1]]$id
)

list_memos(log)
```
