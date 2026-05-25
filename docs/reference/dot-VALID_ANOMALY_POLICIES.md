# Valid anomaly-handling policy enum values

Valid anomaly-handling policy enum values

## Usage

``` r
.VALID_ANOMALY_POLICIES
```

## Format

An object of class `character` of length 3.

## Details

The framework spec is FIXED at run start (AC2 / AC8) – pakhom does not
mutate constructs mid-run. These policies therefore describe what
happens to anomaly SEGMENTS in THIS run's output, not edits to the spec
itself. If a researcher wants to add or modify constructs for a future
run, they edit the framework YAML manually based on what the anomaly
clustering reveals + the framework_review.csv produced under the
"revise" policy.

"extend" – cluster the anomaly segments inductively into a section of
"emergent themes" parallel to the framework themes (Vila-Henninger 2024
"abductive coding"). The framework themes remain primary; the emergent
themes section surfaces patterns the framework did not anticipate. This
is the DEFAULT per Phase 54 – it gives the researcher visibility into
framework misfit instead of burying non-fitting segments in a single
"Anomaly" catch-all. "revise" – same as "extend" PLUS writes
framework_review.csv to the run directory (one row per anomaly segment
with suggested-edit + accepted columns for the researcher to fill).
Right for pilot studies / framework adaptation where the researcher
expects to update the spec for a future run after inspecting which
segments resisted the framework. The existing after_themes review pause
(config\$analysis\$review_points\$after_themes) is the integration point
where the researcher acts on the CSV. Phase 58 Tier 8 M-26 status: a
dedicated after_framework\_- coding pause point remains deferred – the
architectural requirement (resumable runs that re-load an in-flight spec
edit + re-route already-coded anomaly segments through the updated
framework) is substantial enough that shipping half-implementation would
be worse than continued deferral. Today after_themes serves as the
review point; M-26 is queued for the post-rewrite Phase 60+ checkpoint
redesign. "bracket" – preserve the pre-Phase-54 behavior: a single
"Anomaly (non-fitting)" catch-all theme containing every non-fitting
segment. Right when the framework is mature and the researcher genuinely
wants anomalies bracketed rather than re-analyzed.
