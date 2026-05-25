# Defensive escape for the entry-text fence

Phase 58 Tier 7 audit followup H-T7-3: when the entry text literally
contains `</entry_text>`, the prompt fence is unbalanced and the AI may
compute offsets against a truncated view of the entry. This helper
replaces the closing-tag sentinel inside the entry text with an
unambiguous escape that parsers see as literal text. The offsets the AI
emits will then be against the ESCAPED text, which is what
`verify_quote` also sees (we don't un-escape before verification; the
escape is a deterministic 1:1 character mapping that preserves
character-position arithmetic for the relevant range). For typical
Reddit posts this is a no-op; only adversarial / tutorial inputs trigger
the substitution.

## Usage

``` r
.escape_entry_text_fence(text)
```
