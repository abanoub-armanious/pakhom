# Schema for AI-judged divisive cluster evaluation (Phase 52)

Used by .evaluate_cluster() during the top-down HAC tree walk in
generate_themes_iterative(). Replaces the pre-Phase-52 sequential- merge
schema (action = merge\|standalone). The new shape enforces three
load-bearing bias mitigations (Phase 49 audit + Phase 52 design):

## Usage

``` r
.theme_decision_schema()
```

## Details

\(a\) Articulation requirement – the AI must write the central
organizing concept BEFORE its decision. If forcing one feels artificial
it must say so explicitly there. This is the single load-bearing field
for avoiding kitchen-sink themes. (b) Decision is a closed three-valued
enum (coherent_theme / split_required / atomic_outlier) so the AI cannot
hedge with "maybe" or "yes with caveats". (c) The rationale field
requires the AI to address the most-distant code pair specifically – the
prompt always shows this pair, and the rationale must engage with
whether the articulated principle covers BOTH its endpoints.


      {
        "central_organizing_concept": string,    // mandatory articulation
        "decision": "coherent_theme" | "split_required" | "atomic_outlier",
        "proposed_name":        string | null,   // null unless coherent_theme
        "proposed_description": string | null,   // null unless coherent_theme
        "rationale":            string           // engages w/ most-distant pair
      }
