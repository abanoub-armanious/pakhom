# ==============================================================================
# Structured Output Schemas (Sprint-4 T1.2)
# ==============================================================================
# JSON Schemas for the six AI tasks that previously relied on prompt-based
# JSON-mode coercion. With T1.2 the providers enforce these schemas
# server-side: OpenAI via response_format = list(type = "json_schema",
# strict = TRUE, ...), Anthropic via a forced tool-use call whose
# input_schema is the same R list.
#
# Why server-side schemas matter:
# 1. Eliminates parse failures (the legacy parse_json_safely fallback in
#    R/03_json_utils.R was kept on the hot path because GPT-4o would
#    occasionally emit JSON with trailing commas, prose preambles, or
#    truncated objects). A 3% parse-failure rate on a 1000-call run is 30
#    silently-dropped decisions; structured outputs reduce this to 0.
# 2. Eliminates schema-drift bugs (the legacy code accepted whatever shape
#    the model returned, so a model-update could silently change the shape
#    and the caller would crash on missing fields downstream).
# 3. Makes auditing tractable (raw_responses cached under T1.4 can be
#    schema-validated at replay time; pre-T1.2 logs predate strict
#    schemas and can't be).
#
# Design constraints these schemas obey (driven by OpenAI strict mode --
# which is the stricter of the two providers; Anthropic tool-use accepts
# everything OpenAI strict accepts):
# - Every object has additionalProperties = FALSE.
# - Every property listed in `properties` is also in `required`. Optional
#   fields use nullable types via list("string", "null") rather than being
#   omitted from required.
# - No `oneOf`, `anyOf`, `not`, `pattern`, `format`, `minLength`, etc.
# - Enums are declared as list() (not c()) so jsonlite::toJSON with
#   auto_unbox = TRUE doesn't collapse single-element enums to scalars.
# - Required arrays use list() for the same reason.
#
# Each schema is a function (not a constant) so call-site code can
# customize emotion enums, code_type taxonomies, etc., as the package
# evolves without breaking back-compat with cached schemas in older runs.
# ==============================================================================

#' Schema for the per-entry coding response (.code_entry_progressive)
#'
#' Returned shape:
#' \preformatted{
#'   {
#'     "skipped": boolean,
#'     "skip_reason": string,                  // "" when not skipped
#'     "coded_segments": [
#'       {
#'         "text": string,                     // verbatim from entry
#'         "start_char": integer,
#'         "end_char": integer,
#'         "code": string,                     // existing code or "NEW: name"
#'         "code_description": string,         // required for NEW codes
#'         "code_type": "descriptive"|"emotional"|"process"|"in_vivo"
#'       }, ...
#'     ]
#'   }
#' }
#'
#' Strict-mode contract: when skipped = TRUE the model still returns
#' coded_segments (as []) and skip_reason (as a non-empty string).
#' When skipped = FALSE, skip_reason is "".
#' @keywords internal
.coding_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("skipped", "skip_reason", "coded_segments"),
    properties = list(
      skipped     = list(type = "boolean"),
      skip_reason = list(type = "string"),
      coded_segments = list(
        type  = "array",
        items = list(
          type                  = "object",
          additionalProperties  = FALSE,
          required = list("text", "start_char", "end_char",
                          "code", "code_description", "code_type"),
          properties = list(
            text             = list(type = "string"),
            start_char       = list(type = "integer"),
            end_char         = list(type = "integer"),
            code             = list(type = "string"),
            code_description = list(type = "string"),
            code_type        = list(
              type = "string",
              enum = list("descriptive", "emotional", "process", "in_vivo")
            )
          )
        )
      )
    )
  )
}

#' Schema for the per-entry coding response in Mode 3 (Framework Applied)
#'
#' Mirrors \code{.coding_schema()} but constrains \code{construct_id} to
#' the framework's allowed construct ids plus the literal "anomaly" --
#' the model can label a segment with a construct from the framework OR
#' flag it as resisting the framework, but cannot invent new constructs.
#' Per AC2 and AC8, Mode 3 is a CONFIGURATION of the same schema-based
#' coding architecture, not a separate code path: same shape, different
#' enum + an \code{anomaly_reason} field for non-fitting segments.
#'
#' Returned shape:
#' \preformatted{
#'   {
#'     "skipped": boolean,
#'     "skip_reason": string,
#'     "coded_segments": [
#'       {
#'         "text": string,
#'         "start_char": integer,
#'         "end_char": integer,
#'         "construct_id": string (one of framework constructs OR "anomaly"),
#'         "anomaly_reason": string (non-empty when construct_id="anomaly"; "" otherwise)
#'       }, ...
#'     ]
#'   }
#' }
#'
#' @param construct_ids Character vector of allowed construct ids from
#'   the loaded \code{FrameworkSpec}. The schema enforces that
#'   \code{construct_id} is one of these or the literal "anomaly".
#' @keywords internal
.coding_schema_framework <- function(construct_ids) {
  stopifnot(is.character(construct_ids), length(construct_ids) > 0L)
  enum <- as.list(c(construct_ids, "anomaly"))
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("skipped", "skip_reason", "coded_segments"),
    properties = list(
      skipped     = list(type = "boolean"),
      skip_reason = list(type = "string"),
      coded_segments = list(
        type  = "array",
        items = list(
          type                  = "object",
          additionalProperties  = FALSE,
          required = list("text", "start_char", "end_char",
                          "construct_id", "anomaly_reason"),
          properties = list(
            text           = list(type = "string"),
            start_char     = list(type = "integer"),
            end_char       = list(type = "integer"),
            construct_id   = list(type = "string", enum = enum),
            anomaly_reason = list(type = "string")
          )
        )
      )
    )
  )
}

#' Schema for the Phase 56 AI saturation arbiter response
#'
#' Used by \code{.ai_judge_saturation()} during progressive coding to ask
#' the model whether thematic saturation has been reached. Replaces the
#' pre-Phase-56 binary \code{.saturation_schema()} (novel_patterns_remaining
#' + reasoning). The new shape mirrors the Phase 52 theme-decision schema:
#'
#' (a) Articulation requirement -- the model must FIRST describe what it
#'     observes (code growth pattern, codebook composition, reuse density)
#'     before committing to a verdict. Vacuous articulations (<30 chars)
#'     force a downgrade from "reached" -> "not_yet" so the AI can't
#'     declare saturation without substantive reasoning. Same anti-vacuous
#'     pattern Phase 52 uses for theme decisions.
#'
#' (b) Three-valued verdict instead of boolean -- the pre-Phase-56 path
#'     forced a binary novel_patterns_remaining: yes/no. The new shape adds
#'     "uncertain" so the AI can decline to judge when the evidence is
#'     insufficient (e.g., very early in coding). Per C1 ("AI decides when
#'     to stop"), an "uncertain" verdict means "continue coding; re-check
#'     later" rather than forcing a hardcoded min-entries gate.
#'
#' (c) Rationale field -- short justification (1-2 sentences) that must
#'     reference the most distinctive evidence from the prompt.
#'
#' \preformatted{
#'   {
#'     "articulation": string,
#'     "verdict": "reached" | "not_yet" | "uncertain",
#'     "rationale": string
#'   }
#' }
#' @keywords internal
.saturation_decision_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("articulation", "verdict", "rationale"),
    properties = list(
      articulation = list(
        type        = "string",
        description = paste0(
          "BEFORE the verdict: in 2-4 sentences, describe what you observe ",
          "in the data. What is the current code creation rate? What is ",
          "the codebook composition? Are new entries surfacing genuinely ",
          "novel codes or reusing existing ones? Vacuous articulations ",
          "under 30 characters will force a 'not_yet' verdict."
        )
      ),
      verdict = list(
        type        = "string",
        enum        = list("reached", "not_yet", "uncertain"),
        description = paste0(
          "'reached' = the codebook is stable; new entries are reusing ",
          "existing codes rather than generating novel ones. ",
          "'not_yet' = the codebook is still meaningfully growing. ",
          "'uncertain' = the evidence is insufficient to judge (e.g., too ",
          "little coded yet); continue coding and re-check later. ",
          "Your articulation must justify the verdict."
        )
      ),
      rationale = list(
        type        = "string",
        description = paste0(
          "1-2 sentence justification. Must reference the most distinctive ",
          "piece of evidence from the prompt (e.g., the new-codes-per-window ",
          "trajectory, the reuse density, or a specific code-reuse pattern)."
        )
      )
    )
  )
}

#' Schema for the code-description refresh response (Phase 58 Tier 2 C-5)
#'
#' \preformatted{
#'   {
#'     "description": "<1-2 sentence refreshed description capturing the conceptual core>"
#'   }
#' }
#'
#' @keywords internal
.code_description_refresh_schema <- function() {
  list(
    type                 = "object",
    additionalProperties = FALSE,
    required             = list("description"),
    properties = list(
      description = list(
        type        = "string",
        description = paste0(
          "Refreshed description (1-2 sentences) that captures the CONCEPTUAL ",
          "CORE shared across the sample segments shown. The original ",
          "description was anchored to the first segment that created the ",
          "code; if scope has drifted, the refresh must reflect the broader ",
          "pattern. Specific enough to distinguish from sibling codes but ",
          "general enough to cover every segment in the sample. AVOID just ",
          "restating the code name."
        )
      )
    )
  )
}

#' Schema for the per-batch sentiment response (analyze_sentiment)
#'
#' \preformatted{
#'   {
#'     "results": [
#'       {
#'         "id": integer,                      // matches the batch entry index
#'         "sentiment_score": number,          // -1 to 1
#'         "confidence": number,               // 0 to 1
#'         "emotions": [string],               // multi-label, ordered strongest first
#'         "emotion_intensity": number         // 0 to 1
#'       }, ...
#'     ]
#'   }
#' }
#'
#' @param emotion_categories Character vector of allowed emotion labels.
#'   Defaults to the eight Plutchik primaries plus "neutral". Pass
#'   \code{config$analysis$sentiment$emotion_categories} when you want the
#'   schema's enum to match the prompt's enum.
#' @keywords internal
.sentiment_schema <- function(emotion_categories = c("joy", "sadness", "anger",
                                                      "fear", "surprise",
                                                      "disgust", "trust",
                                                      "anticipation", "neutral")) {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("results"),
    properties = list(
      results = list(
        type  = "array",
        items = list(
          type                  = "object",
          additionalProperties  = FALSE,
          required = list("id", "sentiment_score", "confidence",
                          "emotions", "emotion_intensity"),
          properties = list(
            id               = list(type = "integer"),
            sentiment_score  = list(type = "number"),
            confidence       = list(type = "number"),
            emotions = list(
              type  = "array",
              items = list(
                type = "string",
                enum = as.list(unique(emotion_categories))
              )
            ),
            emotion_intensity = list(type = "number")
          )
        )
      )
    )
  )
}

#' Schema for AI-judged divisive cluster evaluation (Phase 52)
#'
#' Used by .evaluate_cluster() during the top-down HAC tree walk in
#' generate_themes_iterative(). Replaces the pre-Phase-52 sequential-
#' merge schema (action = merge|standalone). The new shape enforces
#' three load-bearing bias mitigations (Phase 49 audit + Phase 52
#' design):
#'
#' (a) Articulation requirement -- the AI must write the central
#'     organizing concept BEFORE its decision. If forcing one feels
#'     artificial it must say so explicitly there. This is the single
#'     load-bearing field for avoiding kitchen-sink themes.
#' (b) Decision is a closed three-valued enum (coherent_theme /
#'     split_required / atomic_outlier) so the AI cannot hedge with
#'     "maybe" or "yes with caveats".
#' (c) The rationale field requires the AI to address the most-distant
#'     code pair specifically -- the prompt always shows this pair, and
#'     the rationale must engage with whether the articulated principle
#'     covers BOTH its endpoints.
#'
#' \preformatted{
#'   {
#'     "central_organizing_concept": string,    // mandatory articulation
#'     "decision": "coherent_theme" | "split_required" | "atomic_outlier",
#'     "proposed_name":        string | null,   // null unless coherent_theme
#'     "proposed_description": string | null,   // null unless coherent_theme
#'     "rationale":            string           // engages w/ most-distant pair
#'   }
#' }
#' @keywords internal
.theme_decision_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("central_organizing_concept", "decision",
                                  "proposed_name", "proposed_description",
                                  "rationale"),
    properties = list(
      central_organizing_concept = list(
        type        = "string",
        description = paste0(
          "Articulate the conceptual organizing principle that unifies ALL ",
          "the codes in this cluster. If forcing one feels artificial -- ",
          "if the most distant code pair stretches the principle -- say so ",
          "explicitly here. This articulation is the BASIS for the decision."
        )
      ),
      decision = list(
        type = "string",
        enum = list("coherent_theme", "split_required", "atomic_outlier")
      ),
      proposed_name = list(
        type        = list("string", "null"),
        description = paste0(
          "5-12 word theme/subtheme name (null unless decision is ",
          "coherent_theme). Should sound like a research finding."
        )
      ),
      proposed_description = list(
        type        = list("string", "null"),
        description = paste0(
          "1-2 sentence description (null unless decision is coherent_theme). ",
          "What the central organizing concept IS, in researcher voice."
        )
      ),
      rationale = list(
        type        = "string",
        description = paste0(
          "Why this decision? Address the most-distant code pair shown ",
          "above specifically: does the principle you articulated cover ",
          "BOTH endpoints? If split_required, what is the conceptual ",
          "fault line that runs through the cluster? If atomic_outlier, ",
          "why does this code (or tightly-bound set) not fit any larger ",
          "theme?"
        )
      )
    )
  )
}

#' Schema for the per-pass clustering proposal (Phase 60)
#'
#' Used by \code{ai_propose_clustering()} during the multi-pass theme-
#' clustering algorithm in \code{generate_themes_phase60()}. Replaces (for the
#' v2 algorithm) the single-call \code{.theme_decision_schema()} which fused
#' structural and labeling decisions in one shot.
#'
#' Design contract honored by this schema (binding under C-tenet 3 and 5,
#' \code{pakhom/notes/strategic_audit/REWRITE_PLAN_PHASE_50_TO_59.md:498-528}
#' and \code{PHASE_60_THEME_ALGORITHM_REWRITE.md}):
#'
#' (a) NO \code{name} / \code{description} fields. Labeling happens in a
#'     dedicated post-convergence pass (\code{.theme_labeling_schema()}).
#'     The AI cannot leak name pressure into the structural decision.
#'
#' (b) Per-cluster \code{rationale} field. The AI must justify EACH proposed
#'     grouping in its own words, naming the codes it's grouping and why.
#'     This is the "look at each code carefully" property without falling
#'     into the pre-Phase-52 sequential-pairwise cascade. The AI still sees
#'     ALL leaves at once -- the "full picture" -- but is forced to write a
#'     justification per cluster.
#'
#' (c) Closed two-valued \code{verdict} enum (\code{continue} or
#'     \code{converged}). The AI either proposes a new partition that
#'     groups the current leaves OR declares convergence. No hedging.
#'
#' (d) \code{cluster_assignments} is nullable. When verdict is
#'     \code{converged} the field is \code{null}. OpenAI strict mode
#'     forbids conditional schemas (no \code{oneOf}), so the validation
#'     contract is post-call: orchestrator checks that
#'     \code{continue + null} or \code{converged + non-null} are rejected
#'     and re-prompts.
#'
#' \preformatted{
#'   {
#'     "verdict": "continue" | "converged",
#'     "cluster_assignments": [
#'       { "leaf_indices": [int, ...], "cluster_rationale": str }, ...
#'     ] | null,
#'     "overall_rationale": str
#'   }
#' }
#'
#' @keywords internal
.clustering_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("verdict", "cluster_assignments",
                                  "overall_rationale"),
    properties = list(
      verdict = list(
        type        = "string",
        enum        = list("continue", "converged"),
        description = paste0(
          "'continue' = propose a new partition that groups the current ",
          "leaves into clusters. 'converged' = no further useful grouping ",
          "is possible; the current leaves are the final structure. ",
          "Setting verdict='converged' requires cluster_assignments=null."
        )
      ),
      cluster_assignments = list(
        type  = list("array", "null"),
        items = list(
          type                  = "object",
          additionalProperties  = FALSE,
          required              = list("leaf_indices", "cluster_rationale"),
          properties = list(
            leaf_indices = list(
              type  = "array",
              items = list(type = "integer"),
              description = paste0(
                "1-based indices into the LEAVES list shown in the prompt. ",
                "Every leaf MUST appear in exactly one cluster (partition ",
                "property). Singleton clusters (length 1) are acceptable ",
                "when a leaf does not belong with any others."
              )
            ),
            cluster_rationale = list(
              type        = "string",
              description = paste0(
                "Why THESE specific leaves belong together. Name each leaf ",
                "you are grouping by its code name (pass 1) or its member ",
                "code names (pass 2+), and explain the unifying principle. ",
                "Do NOT name the cluster -- naming is a separate post-",
                "convergence pass. 30-400 characters."
              )
            )
          )
        ),
        description = paste0(
          "Array of clusters partitioning the current leaves. NULL when ",
          "verdict='converged'. Each cluster must be non-empty; every leaf ",
          "must appear in exactly one cluster."
        )
      ),
      overall_rationale = list(
        type        = "string",
        description = paste0(
          "Top-level reasoning: why this partition (or why convergence)? ",
          "If continuing, explain the conceptual fault lines you found ",
          "across leaves. If converged, explain why no further grouping ",
          "would yield useful structure. 50-500 characters."
        )
      )
    )
  )
}

#' Schema for the post-convergence theme + subtheme labeling pass (Phase 60)
#'
#' Used by \code{ai_label_theme_set()} in a single AI call after multi-pass
#' clustering has converged. The AI sees the FULL converged tree
#' (themes -> subthemes -> codes) and assigns researcher-facing names +
#' descriptions to every node.
#'
#' Design contract (binding under C-tenet 5):
#'
#' (a) Labeling happens AFTER structural decisions. The AI cannot influence
#'     which codes belong to which theme during this call -- the structure
#'     is fixed.
#'
#' (b) The AI sees ALL themes + ALL subthemes + ALL codes in one prompt, so
#'     cross-theme name distinctness is enforceable (the AI is explicitly
#'     instructed not to use the same or near-duplicate names for two
#'     themes).
#'
#' (c) The response shape mirrors the structural skeleton: themes array,
#'     each theme has subthemes array. The orchestrator binds names back to
#'     the skeleton positionally via \code{theme_index} and
#'     \code{subtheme_index}.
#'
#' (d) The AI returns the same number of themes as the skeleton has, and the
#'     same number of subthemes per theme. Orchestrator post-validates this
#'     and re-prompts on mismatch.
#'
#' \preformatted{
#'   {
#'     "themes": [
#'       {
#'         "theme_index": int,             // 1-based, must match skeleton
#'         "name": str,                    // 3-12 words, substantive noun phrase
#'         "description": str,             // 1-2 sentences in researcher voice
#'         "subthemes": [
#'           { "subtheme_index": int, "name": str, "description": str }, ...
#'         ]
#'       }, ...
#'     ]
#'   }
#' }
#'
#' @keywords internal
.theme_labeling_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("themes"),
    properties = list(
      themes = list(
        type  = "array",
        items = list(
          type                  = "object",
          additionalProperties  = FALSE,
          required              = list("theme_index", "name", "description",
                                        "subthemes"),
          properties = list(
            theme_index = list(
              type        = "integer",
              description = paste0(
                "1-based index of this theme in the skeleton. Must match the ",
                "order shown in the prompt. The orchestrator binds names to ",
                "skeleton positions via this index."
              )
            ),
            name = list(
              type        = "string",
              description = paste0(
                "Substantive noun phrase (3-12 words) that names the theme's ",
                "central organizing concept. Sounds like a research finding ",
                "(e.g. 'Identity reconstruction after medication onset'). ",
                "NOT a list-of-things or bucket label (avoid 'Various aspects ",
                "of X', 'Mixed experiences with Y'). Must be DISTINCT from ",
                "every other theme name in this response."
              )
            ),
            description = list(
              type        = "string",
              description = paste0(
                "1-2 sentence theme description in researcher voice. States ",
                "what the central organizing concept IS and how it manifests ",
                "across the codes in this theme. Specific enough to ",
                "distinguish from sibling themes."
              )
            ),
            subthemes = list(
              type  = "array",
              items = list(
                type                  = "object",
                additionalProperties  = FALSE,
                required              = list("subtheme_index", "name",
                                              "description"),
                properties = list(
                  subtheme_index = list(
                    type        = "integer",
                    description = paste0(
                      "1-based index of this subtheme within its parent ",
                      "theme. Must match the order shown in the prompt."
                    )
                  ),
                  name = list(
                    type        = "string",
                    description = paste0(
                      "Subtheme name (3-10 words) that names what this ",
                      "subset of codes shares. Distinct from sibling ",
                      "subtheme names AND from the parent theme name."
                    )
                  ),
                  description = list(
                    type        = "string",
                    description = paste0(
                      "1 sentence description of the subtheme. States what ",
                      "the codes in this subtheme have in common that ",
                      "distinguishes them from sibling subthemes."
                    )
                  )
                )
              ),
              description = paste0(
                "Array of subtheme labels, one per subtheme in the skeleton ",
                "for this theme. Must be the same length as the skeleton's ",
                "subtheme list and in the same order. Empty array when the ",
                "theme has no subtheme structure (single-pass convergence ",
                "or theme contains codes directly)."
              )
            )
          )
        )
      )
    )
  )
}

#' Schema for batch inductive coding of Mode 3 anomaly segments (Phase 54)
#'
#' Used by .inductive_code_anomaly_segments() to turn the segments that
#' didn't fit a framework into named inductive codes. The AI sees a batch
#' of anomaly segment texts and generates a code_name + code_description
#' per segment. Codes can naturally repeat across segments (the AI is
#' prompted to reuse code names for segments expressing the same concept);
#' Phase 52's HAC + AI tree walk then consolidates near-duplicates into
#' emergent themes.
#'
#' This schema is intentionally per-segment (rather than cross-referenced
#' codes -> segments) so the structured output stays simple and the
#' provenance from segment back to its inductive code stays one-to-one.
#'
#' \preformatted{
#'   {
#'     "coded_segments": [
#'       { "segment_index": int, "code_name": string, "code_description": string }
#'     ]
#'   }
#' }
#' @keywords internal
.emergent_coding_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("coded_segments"),
    properties = list(
      coded_segments = list(
        type  = "array",
        items = list(
          type                  = "object",
          additionalProperties  = FALSE,
          required              = list("segment_index", "code_name",
                                        "code_description"),
          properties = list(
            segment_index    = list(
              type        = "integer",
              description = "1-based index into the segments array provided in the prompt."
            ),
            code_name        = list(
              type        = "string",
              description = paste0(
                "3-8 word inductive code name capturing what the segment is ",
                "ABOUT (not the verbatim words used). Reuse the same name across ",
                "segments expressing the same concept -- consolidation is welcome."
              )
            ),
            code_description = list(
              type        = "string",
              description = paste0(
                "1-2 sentence description of the conceptual content. Speaks ",
                "to the abductive 'what does this reveal that the framework did ",
                "not anticipate?' question."
              )
            )
          )
        )
      )
    )
  )
}

#' Schema for the insight-generation response (generate_insights)
#'
#' \preformatted{
#'   {
#'     "key_findings": [{ "insight": string, "explanation": string }],
#'     "theoretical_implications": string,
#'     "practical_implications": string,
#'     "limitations": [string],
#'     "future_directions": [string]
#'   }
#' }
#' @keywords internal
.insight_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("key_findings", "theoretical_implications",
                                  "practical_implications", "limitations",
                                  "future_directions"),
    properties = list(
      key_findings = list(
        type  = "array",
        items = list(
          type                 = "object",
          additionalProperties = FALSE,
          required             = list("insight", "explanation"),
          properties = list(
            insight     = list(type = "string"),
            explanation = list(type = "string")
          )
        )
      ),
      theoretical_implications = list(type = "string"),
      practical_implications   = list(type = "string"),
      limitations              = list(type = "array",
                                       items = list(type = "string")),
      future_directions        = list(type = "array",
                                       items = list(type = "string"))
    )
  )
}

#' Schema for the executive-summary synthesis response (generate_ai_synthesis)
#'
#' \preformatted{
#'   { "executive_summary": string, "conclusion": string }
#' }
#' @keywords internal
.synthesis_schema <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("executive_summary", "conclusion"),
    properties = list(
      executive_summary = list(type = "string"),
      conclusion        = list(type = "string")
    )
  )
}

# ==============================================================================
# Schema validation helper (lightweight; used in tests + sanity checks)
# ==============================================================================

#' Lightweight check that a list looks like a valid JSON Schema for our use
#'
#' Not a full JSON Schema validator -- just enough to catch the OpenAI
#' strict-mode pitfalls at package-load / test time so a malformed schema
#' is caught before it hits the API. Specifically:
#' \itemize{
#'   \item Top-level must be an object schema with type = "object".
#'   \item Every object must have additionalProperties = FALSE.
#'   \item Every object's `required` must list every key in `properties`.
#'   \item Required and enum arrays must be lists (not character vectors)
#'     to avoid jsonlite auto_unbox collapsing single-element arrays.
#' }
#'
#' @param schema A schema list (e.g., the output of \code{.coding_schema()}).
#' @param path Internal: schema path, used for error messages.
#' @return TRUE invisibly if the schema is well-formed; otherwise stops
#'   with a descriptive error pointing at the violating subschema.
#' @keywords internal
.validate_schema <- function(schema, path = "$") {
  if (!is.list(schema)) {
    stop(sprintf("Schema at %s must be a list", path), call. = FALSE)
  }

  # Type can be a string ("object", "string", ...) or a list of strings
  # for nullable types (e.g., list("integer", "null")). Check both shapes.
  type <- schema$type
  if (is.list(type)) {
    if (!all(vapply(type, is.character, logical(1)))) {
      stop(sprintf("Schema at %s: type list must contain only strings", path),
           call. = FALSE)
    }
  }

  is_object <- identical(type, "object") ||
               (is.list(type) && "object" %in% unlist(type))

  if (is_object) {
    if (!isFALSE(schema$additionalProperties)) {
      stop(sprintf("Schema at %s: additionalProperties must be FALSE for OpenAI strict mode", path),
           call. = FALSE)
    }
    if (is.null(schema$properties) || !is.list(schema$properties)) {
      stop(sprintf("Schema at %s: object schemas must declare properties", path),
           call. = FALSE)
    }
    if (!is.list(schema$required) ||
        any(vapply(schema$required, function(r) !is.character(r), logical(1)))) {
      stop(sprintf("Schema at %s: required must be a list() of property name strings (not c())", path),
           call. = FALSE)
    }
    prop_names <- names(schema$properties)
    req_names  <- vapply(schema$required, identity, character(1))
    missing_in_required <- setdiff(prop_names, req_names)
    if (length(missing_in_required) > 0) {
      stop(sprintf("Schema at %s: properties not in required (OpenAI strict mode forbids optional fields): %s",
                   path, paste(missing_in_required, collapse = ", ")),
           call. = FALSE)
    }
    extra_in_required <- setdiff(req_names, prop_names)
    if (length(extra_in_required) > 0) {
      stop(sprintf("Schema at %s: required references unknown properties: %s",
                   path, paste(extra_in_required, collapse = ", ")),
           call. = FALSE)
    }

    # Recurse into each property
    for (nm in prop_names) {
      .validate_schema(schema$properties[[nm]],
                       path = sprintf("%s.properties.%s", path, nm))
    }
  }

  # Recurse into array items
  if (identical(type, "array") && !is.null(schema$items)) {
    .validate_schema(schema$items, path = sprintf("%s.items", path))
  }

  # Recurse into enum: ensure it's a list (not c()) so single-element enums
  # don't collapse under jsonlite auto_unbox.
  if (!is.null(schema$enum)) {
    if (!is.list(schema$enum)) {
      stop(sprintf("Schema at %s: enum must be a list() (not c()) so single-element enums survive jsonlite auto_unbox", path),
           call. = FALSE)
    }
  }

  invisible(TRUE)
}
