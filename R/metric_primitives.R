# ==============================================================================
# Backend catalog of computational metric primitives
# ==============================================================================
#
# The architecture is "AI as analyst with calculator". This file is the
# CALCULATOR: a flat catalog of small, deterministic, well-defined statistics
# the AI can request by name. The AI -- not the researcher and not a hardcoded
# taxonomy -- inspects each metric column plus the research focus and decides
# WHICH primitives are honest for that column and HOW to interpret the result.
#
# This is BACKEND scaffolding: the researcher
# never sees, picks from, or configures this catalog. They supply only their
# data + research focus + mode. The catalog is invisible to them. That makes it
# the "backend hardcoding = fine" kind, never the "user-facing classification =
# forbidden" kind.
#
# Design properties (binding):
#  * SINGLE SOURCE OF TRUTH. .metric_primitive_registry() is the only place a
#    primitive is registered. metric_catalog() (the AI-facing listing),
#    compute_metric_stat() (the dispatcher), and the test suite all read from
#    it, so the catalog and the dispatcher can never drift apart.
#  * ALLOWLIST DISPATCH ONLY. compute_metric_stat() looks the requested name up
#    in the registry; it NEVER calls get()/match.fun()/eval() on a model-supplied
#    string. An AI (or a typo in a pinned replay config) cannot reach an
#    arbitrary R function. This is a deliberate security boundary -- AI-generated
#    R code is explicitly deferred (a future feature) behind sandboxing.
#  * FAIL HONESTLY (design requirement R4). When a requested primitive is not in
#    the catalog, the dispatcher returns a structured `available = FALSE` record
#    naming the gap -- it NEVER silently substitutes a different statistic. The
#    report surfaces the gap; maintainers/community can then contribute the
#    primitive (the catalog is extensible by construction).
#  * NA-SAFE BY CONTRACT. Every primitive strips NAs internally and returns NA
#    (scalar primitives) or an empty named structure (distribution primitives)
#    on empty / all-NA / too-few-observations input -- never an error. The
#    dispatcher reports n_observed alongside every value so downstream reporting
#    is honest about how much data a statistic rests on.
#  * NO NEW DEPENDENCIES. Skewness, kurtosis, Shannon entropy and circular
#    statistics are implemented directly from their definitions in base R
#    (verified numerically before implementation) rather than pulling in
#    moments/e1071/circular. Conventions are documented per-primitive.
#
# All functions here are @keywords internal: documented for maintainers and
# community contributors (per the extensibility goal), but not part of the
# package's minimal public surface.
# ==============================================================================

# ---- internal cleaning helpers ----------------------------------------------

# Coerce to numeric and drop every non-finite value (NA, NaN, +/-Inf) plus
# coercion failures (a character column the AI mis-identifies as numeric becomes
# NA and is dropped). is.finite() is the single gate: it excludes NA/NaN/Inf in
# one pass, so a primitive never sees Inf and therefore never errors on it
# (e.g. mean((v-m)^2) staying finite for the shape primitives) -- honoring the
# "never error, return NA" contract uniformly across the whole family.
.prim_clean <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  v[is.finite(v)]
}

# Coerce a timestamp vector to POSIXct (UTC) and drop NA. Accepts a POSIXct
# vector, epoch-seconds (numeric), OR a formatted datetime STRING -- the last is
# what the package's std_timestamp column actually holds (R/07_data_loading.R),
# so temporal primitives must handle it or they silently compute on nothing. The
# The AI does not have to declare which shape; all three are accepted. UTC is fixed so
# reports are reproducible across machine locales.
.prim_clean_time <- function(t) {
  if (inherits(t, "POSIXct")) {
    return(t[is.finite(as.numeric(t))])     # drops NA and any non-finite instant
  }
  if (is.numeric(t)) {                       # epoch seconds
    return(as.POSIXct(t[is.finite(t)], origin = "1970-01-01", tz = "UTC"))
  }
  # Character: epoch-seconds-as-strings only when EVERY value is numeric;
  # otherwise treat as datetime strings and parse with the package's robust,
  # non-throwing, multi-format parser (so "2024-01-01 18:00:00" works and one
  # garbage cell yields NA rather than an error).
  ch      <- as.character(t)
  present <- ch[!is.na(ch) & nzchar(ch)]
  num     <- suppressWarnings(as.numeric(present))
  if (length(present) > 0L && all(is.finite(num))) {
    return(as.POSIXct(num, origin = "1970-01-01", tz = "UTC"))
  }
  parsed <- .parse_timestamps(ch)
  parsed[is.finite(as.numeric(parsed))]
}

# ==============================================================================
# Primitive implementations
#
# Each primitive takes its natural argument(s) and returns either a length-1
# numeric (scalar primitives) or a named numeric vector (distribution
# primitives). The (x, args) calling convention used by the dispatcher is
# adapted in the registry, so these stay clean and individually testable.
# ==============================================================================

#' Backend metric primitives (catalog functions)
#'
#' A flat catalog of small, deterministic statistics the Methodology
#' Assistant can request by name (see [metric_catalog()] and
#' [compute_metric_stat()]). Each scalar primitive returns a length-1 numeric
#' (NA on empty / all-NA / insufficient input); each distribution primitive
#' returns a named numeric vector (empty named numeric on no data). All strip
#' NAs internally and never error on degenerate input.
#'
#' These are backend scaffolding, not a user-facing API: researchers never pick
#' from this catalog. They are documented for maintainers and community
#' contributors extending the catalog.
#'
#' @param x Numeric vector (the metric column). Non-numeric values are coerced
#'   and dropped.
#' @param t Timestamp vector: either POSIXct or epoch-seconds (numeric). Parsed
#'   as UTC.
#' @param q Quantile probability in \[0, 1\] (for [prim_quantile()]).
#' @param threshold Numeric cut point (for the proportion-above/below
#'   primitives).
#' @param lo,hi Inclusive numeric bounds (for [prim_proportion_in_range()]).
#' @param k Positive integer: number of most-frequent values to return (for
#'   [prim_top_k_values()]).
#' @param bin_width_days Positive numeric bin width in days (for
#'   [prim_entries_over_time()]).
#' @return A length-1 numeric for scalar primitives, or a named numeric vector
#'   for distribution primitives.
#' @name metric_primitives
#' @keywords internal
NULL

# ---- location ----------------------------------------------------------------

#' @describeIn metric_primitives Arithmetic mean. Honest for symmetric data;
#'   misleading for skewed / heavy-tailed data (prefer [prim_median()]).
prim_mean <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  mean(v)
}

#' @describeIn metric_primitives Median (50th percentile). Robust to outliers
#'   and skew; the honest center for heavy-tailed counts (e.g. upvotes).
prim_median <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  stats::median(v)
}

#' @describeIn metric_primitives Statistical mode (most frequent value; ties
#'   resolve to the smallest). Useful for discrete / coded / ordinal columns.
prim_mode_value <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  tb <- table(v)                 # names() are sorted ascending -> ties take min
  as.numeric(names(tb)[which.max(tb)])
}

# ---- spread ------------------------------------------------------------------

#' @describeIn metric_primitives Standard deviation (n-1 denominator). Pairs
#'   with [prim_mean()]; like the mean, assumes a roughly symmetric scale.
prim_sd <- function(x) {
  v <- .prim_clean(x)
  if (length(v) < 2L) return(NA_real_)
  stats::sd(v)
}

#' @describeIn metric_primitives Median absolute deviation, normal-consistent
#'   (constant 1.4826). The robust spread to pair with [prim_median()].
prim_mad <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  stats::mad(v)                  # default constant 1.4826 (normal-consistent)
}

#' @describeIn metric_primitives Interquartile range (Q3 - Q1, type-7). Robust
#'   spread; the basis of the 1.5*IQR outlier rule.
prim_iqr <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  stats::IQR(v, type = 7)
}

#' @describeIn metric_primitives Range width (max - min). Coarse spread; very
#'   sensitive to single extreme values.
prim_range_width <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  diff(range(v))
}

#' @describeIn metric_primitives Coefficient of variation (sd / mean). Unitless
#'   relative dispersion; NA when the mean is ~0 (ratio undefined).
prim_cv <- function(x) {
  v <- .prim_clean(x)
  if (length(v) < 2L) return(NA_real_)
  m <- mean(v)
  if (abs(m) < .Machine$double.eps^0.5) return(NA_real_)
  stats::sd(v) / m
}

# ---- extremes / position -----------------------------------------------------

#' @describeIn metric_primitives Minimum observed value.
prim_min <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  min(v)
}

#' @describeIn metric_primitives Maximum observed value (the single most extreme
#'   point; a heavy-tail signal).
prim_max <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  max(v)
}

#' @describeIn metric_primitives Arbitrary quantile at probability `q` (type-7).
#'   Parameterized variant used mainly by the pinned-replay path; the live
#'   schema offers the fixed-probability `prim_pNN` variants below.
prim_quantile <- function(x, q) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  if (is.null(q) || length(q) != 1L || is.na(q) || q < 0 || q > 1) return(NA_real_)
  unname(stats::quantile(v, probs = q, type = 7, names = FALSE))
}

#' @describeIn metric_primitives 10th percentile.
prim_p10 <- function(x) prim_quantile(x, 0.10)
#' @describeIn metric_primitives 25th percentile (lower quartile, Q1).
prim_p25 <- function(x) prim_quantile(x, 0.25)
#' @describeIn metric_primitives 75th percentile (upper quartile, Q3).
prim_p75 <- function(x) prim_quantile(x, 0.75)
#' @describeIn metric_primitives 90th percentile. Honest "high end" for
#'   heavy-tailed data where the mean is dominated by the tail.
prim_p90 <- function(x) prim_quantile(x, 0.90)
#' @describeIn metric_primitives 95th percentile.
prim_p95 <- function(x) prim_quantile(x, 0.95)
#' @describeIn metric_primitives 99th percentile (extreme tail).
prim_p99 <- function(x) prim_quantile(x, 0.99)

# ---- count / distribution shape ----------------------------------------------

#' @describeIn metric_primitives Count of non-missing observations the statistic
#'   rests on. (The dispatcher also reports this as `n_observed`.)
prim_n <- function(x) {
  as.numeric(length(.prim_clean(x)))
}

#' @describeIn metric_primitives Number of distinct non-missing values. Low
#'   relative to n suggests a discrete / categorical column.
prim_n_unique <- function(x) {
  as.numeric(length(unique(.prim_clean(x))))
}

#' @describeIn metric_primitives Skewness (moment coefficient g1 =
#'   m3 / m2^1.5, the same convention as moments::skewness). >0 right-skewed,
#'   <0 left-skewed, ~0 symmetric. NA for fewer than 3 observations or zero
#'   variance.
prim_skewness <- function(x) {
  v <- .prim_clean(x)
  if (length(v) < 3L) return(NA_real_)
  m  <- mean(v)
  m2 <- mean((v - m)^2)
  if (m2 <= 0) return(NA_real_)
  mean((v - m)^3) / m2^1.5
}

#' @describeIn metric_primitives Excess kurtosis (g2 = m4 / m2^2 - 3), so a
#'   normal distribution scores 0 and heavier-than-normal tails score >0. NA for
#'   fewer than 4 observations or zero variance.
prim_kurtosis_excess <- function(x) {
  v <- .prim_clean(x)
  if (length(v) < 4L) return(NA_real_)
  m  <- mean(v)
  m2 <- mean((v - m)^2)
  if (m2 <= 0) return(NA_real_)
  mean((v - m)^4) / m2^2 - 3
}

# ---- heavy-tail-aware --------------------------------------------------------

#' @describeIn metric_primitives Mean of log1p(x). A tail-dampened center for
#'   right-skewed non-negative counts. NA if any value is negative (log1p
#'   undefined there) -- a fail-honest signal that the column is not a
#'   non-negative count.
prim_log_mean <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L || any(v < 0)) return(NA_real_)
  mean(log1p(v))
}

#' @describeIn metric_primitives Standard deviation of log1p(x). Tail-dampened
#'   spread for non-negative counts; NA if any value is negative.
prim_log_sd <- function(x) {
  v <- .prim_clean(x)
  if (length(v) < 2L || any(v < 0)) return(NA_real_)
  stats::sd(log1p(v))
}

#' @describeIn metric_primitives Count of values beyond the 1.5*IQR Tukey
#'   fences. A direct count of how many outliers a heavy tail produces.
prim_outlier_count_iqr <- function(x) {
  v <- .prim_clean(x)
  if (length(v) < 4L) return(NA_real_)
  qs  <- stats::quantile(v, c(0.25, 0.75), type = 7, names = FALSE)
  iqr <- qs[2] - qs[1]
  lo  <- qs[1] - 1.5 * iqr
  hi  <- qs[2] + 1.5 * iqr
  as.numeric(sum(v < lo | v > hi))
}

#' @describeIn metric_primitives Ratio of max to median. A scale-free heavy-tail
#'   index: 1 means no tail, large values mean the maximum dwarfs the typical
#'   case. NA when the median is ~0.
prim_max_to_median_ratio <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  md <- stats::median(v)
  if (abs(md) < .Machine$double.eps^0.5) return(NA_real_)
  max(v) / md
}

# ---- proportional / bounded --------------------------------------------------

#' @describeIn metric_primitives Proportion of values exactly equal to 0. For
#'   zero-inflated counts (e.g. how many entries received no upvotes).
prim_proportion_zero <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  mean(v == 0)
}

#' @describeIn metric_primitives Proportion of values not equal to 0
#'   (1 - proportion_zero).
prim_proportion_nonzero <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  mean(v != 0)
}

#' @describeIn metric_primitives Proportion of values strictly above
#'   `threshold`. NA if `threshold` is missing.
prim_proportion_above <- function(x, threshold) {
  v <- .prim_clean(x)
  if (length(v) == 0L || is.null(threshold) || length(threshold) != 1L ||
      is.na(threshold)) return(NA_real_)
  mean(v > threshold)
}

#' @describeIn metric_primitives Proportion of values strictly below
#'   `threshold`. NA if `threshold` is missing.
prim_proportion_below <- function(x, threshold) {
  v <- .prim_clean(x)
  if (length(v) == 0L || is.null(threshold) || length(threshold) != 1L ||
      is.na(threshold)) return(NA_real_)
  mean(v < threshold)
}

#' @describeIn metric_primitives Proportion of values in the inclusive range
#'   \[lo, hi\]. NA if either bound is missing.
prim_proportion_in_range <- function(x, lo, hi) {
  v <- .prim_clean(x)
  if (length(v) == 0L || is.null(lo) || is.null(hi) ||
      length(lo) != 1L || length(hi) != 1L || is.na(lo) || is.na(hi)) {
    return(NA_real_)
  }
  mean(v >= lo & v <= hi)
}

# ---- categorical (numeric-encoded) -------------------------------------------

#' @describeIn metric_primitives Shannon entropy in bits
#'   (-sum p*log2 p over distinct values). 0 = one value only; log2(k) = uniform
#'   over k values. A concentration / diversity measure for coded columns.
prim_entropy <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  p <- as.numeric(table(v)) / length(v)
  p <- p[p > 0]
  -sum(p * log2(p))
}

#' @describeIn metric_primitives Full frequency distribution: a named numeric
#'   vector of counts per distinct value (names are the values). Distribution
#'   primitive.
prim_frequency_distribution <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(stats::setNames(numeric(0), character(0)))
  tb <- table(v)
  stats::setNames(as.numeric(tb), names(tb))
}

#' @describeIn metric_primitives The `k` most frequent values and their counts
#'   (named numeric vector, descending). Distribution primitive.
prim_top_k_values <- function(x, k) {
  v <- .prim_clean(x)
  if (length(v) == 0L || is.null(k) || length(k) != 1L || is.na(k) || k < 1) {
    return(stats::setNames(numeric(0), character(0)))
  }
  tb <- sort(table(v), decreasing = TRUE)
  tb <- tb[seq_len(min(as.integer(k), length(tb)))]
  stats::setNames(as.numeric(tb), names(tb))
}

#' @describeIn metric_primitives The 3 most frequent values and their counts
#'   (zero-arg convenience over [prim_top_k_values()]). Distribution primitive.
prim_top3_values <- function(x) prim_top_k_values(x, 3L)

# ---- temporal ----------------------------------------------------------------

#' @describeIn metric_primitives Counts of entries per clock hour (0-23, UTC).
#'   Distribution primitive; reveals time-of-day rhythm.
prim_hour_of_day_distribution <- function(t) {
  tt <- .prim_clean_time(t)
  if (length(tt) == 0L) return(stats::setNames(numeric(0), character(0)))
  h  <- factor(as.integer(format(tt, "%H", tz = "UTC")), levels = 0:23)
  stats::setNames(as.numeric(table(h)), sprintf("%02d", 0:23))
}

#' @describeIn metric_primitives Counts of entries per ISO weekday
#'   (Mon..Sun, locale-independent). Distribution primitive.
prim_day_of_week_distribution <- function(t) {
  tt <- .prim_clean_time(t)
  labs <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  if (length(tt) == 0L) return(stats::setNames(numeric(0), character(0)))
  d <- factor(as.integer(format(tt, "%u", tz = "UTC")), levels = 1:7)  # 1=Mon
  stats::setNames(as.numeric(table(d)), labs)
}

#' @describeIn metric_primitives Counts of entries per calendar month
#'   (Jan..Dec). Distribution primitive; reveals seasonality.
prim_month_of_year_distribution <- function(t) {
  tt <- .prim_clean_time(t)
  if (length(tt) == 0L) return(stats::setNames(numeric(0), character(0)))
  m <- factor(as.integer(format(tt, "%m", tz = "UTC")), levels = 1:12)
  stats::setNames(as.numeric(table(m)), month.abb)
}

#' @describeIn metric_primitives Span in days between the earliest and latest
#'   timestamp. NA for fewer than 2 timestamps.
prim_time_span_days <- function(t) {
  tt <- .prim_clean_time(t)
  if (length(tt) < 2L) return(NA_real_)
  as.numeric(difftime(max(tt), min(tt), units = "days"))
}

#' @describeIn metric_primitives Median gap, in seconds, between consecutive
#'   timestamps (sorted). Posting cadence; NA for fewer than 2 timestamps.
prim_median_seconds_between <- function(t) {
  tt <- sort(.prim_clean_time(t))
  if (length(tt) < 2L) return(NA_real_)
  stats::median(as.numeric(diff(tt), units = "secs"))
}

#' @describeIn metric_primitives Counts of entries per calendar month
#'   (YYYY-MM, chronological). Distribution primitive; the volume timeline.
prim_entries_by_month <- function(t) {
  tt <- .prim_clean_time(t)
  if (length(tt) == 0L) return(stats::setNames(numeric(0), character(0)))
  ym <- format(tt, "%Y-%m", tz = "UTC")
  tb <- table(ym)                          # table() sorts names -> chronological
  stats::setNames(as.numeric(tb), names(tb))
}

#' @describeIn metric_primitives Counts of entries in fixed-width time bins of
#'   `bin_width_days` days, measured from the earliest timestamp. Distribution
#'   primitive (bin start dates as names). NA-shaped when `bin_width_days` is
#'   missing/invalid.
prim_entries_over_time <- function(t, bin_width_days) {
  tt <- .prim_clean_time(t)
  if (length(tt) == 0L || is.null(bin_width_days) || length(bin_width_days) != 1L ||
      !is.finite(bin_width_days) || bin_width_days <= 0) {  # !is.finite covers NA/NaN/Inf
    return(stats::setNames(numeric(0), character(0)))
  }
  origin <- min(tt)
  width_secs <- bin_width_days * 86400
  bin_idx <- floor(as.numeric(difftime(tt, origin, units = "secs")) / width_secs)
  bin_start <- origin + bin_idx * width_secs
  labs <- format(bin_start, "%Y-%m-%d", tz = "UTC")
  tb <- table(factor(labs, levels = unique(format(sort(bin_start), "%Y-%m-%d", tz = "UTC"))))
  stats::setNames(as.numeric(tb), names(tb))
}

# ---- circular ----------------------------------------------------------------

#' @describeIn metric_primitives Circular (directional) mean of angles in
#'   radians, normalized to \[0, 2*pi). Implemented as
#'   atan2(mean sin, mean cos). Use for periodic quantities where a plain mean
#'   is wrong (e.g. averaging 23:00 and 01:00 should give ~midnight, not noon).
prim_circular_mean <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  a <- atan2(mean(sin(v)), mean(cos(v)))
  if (a < 0) a <- a + 2 * pi
  a
}

#' @describeIn metric_primitives Circular variance of angles in radians: 1 - R,
#'   where R is the mean resultant length. 0 = perfectly concentrated, 1 =
#'   uniformly dispersed around the circle.
prim_circular_variance <- function(x) {
  v <- .prim_clean(x)
  if (length(v) == 0L) return(NA_real_)
  1 - sqrt(mean(sin(v))^2 + mean(cos(v))^2)
}

#' @describeIn metric_primitives Peak (mean) hour of day on a 24h clock, in
#'   \[0, 24). Maps each timestamp's time-of-day to an angle, takes the circular
#'   mean, and converts back to clock hours -- the methodologically correct
#'   "typical time of day", unlike a linear average of hour numbers.
prim_peak_hour_circular <- function(t) {
  tt <- .prim_clean_time(t)
  if (length(tt) == 0L) return(NA_real_)
  frac_hour <- as.numeric(format(tt, "%H", tz = "UTC")) +
               as.numeric(format(tt, "%M", tz = "UTC")) / 60
  ang <- frac_hour / 24 * 2 * pi
  m <- atan2(mean(sin(ang)), mean(cos(ang)))
  if (m < 0) m <- m + 2 * pi
  # %% 24 keeps the result in [0, 24): when the mean direction lands exactly on
  # the wrap point (e.g. times symmetric about midnight), m can round to 2*pi
  # and the conversion would otherwise yield exactly 24.0.
  (m / (2 * pi) * 24) %% 24
}

# ---- distribution-shape test -------------------------------------------------

#' @describeIn metric_primitives Shapiro-Wilk normality test p-value. Small p
#'   (e.g. <0.05) indicates non-normality, justifying robust/rank methods. NA
#'   for fewer than 3 observations or fewer than 3 distinct values. For n > 5000
#'   a seeded 5000-point subsample is tested (Shapiro-Wilk's supported range).
prim_shapiro_p <- function(x) {
  v <- .prim_clean(x)
  n <- length(v)
  if (n < 3L || length(unique(v)) < 3L) return(NA_real_)
  if (n > 5000L) {
    # .with_seed gives the SAME seeded random subsample whether or not withr is
    # installed (it has a withr-free RNG fallback); the previous else-branch
    # used a different (deterministic stride) sample, so results diverged across
    # environments.
    idx <- .with_seed(42, sample(n, 5000))
    v <- v[idx]
  }
  tryCatch(stats::shapiro.test(v)$p.value, error = function(e) NA_real_)
}

# ==============================================================================
# Registry, catalog, and dispatcher
# ==============================================================================

# Build a registry entry. `fn` adapts the uniform (x, args) dispatch convention
# to a primitive's natural signature.
.prim_entry <- function(fn, family, description, shape = "scalar",
                        input_kind = "numeric", needs_args = FALSE) {
  list(fn = fn, family = family, description = description,
       shape = shape, input_kind = input_kind, needs_args = needs_args)
}

#' The metric-primitive registry (single source of truth)
#'
#' Returns a named list mapping every catalog primitive name to a record of
#' \code{fn} (an \code{(x, args)} adapter calling the natural-signature
#' primitive), \code{family}, \code{description}, \code{shape}
#' ("scalar"/"distribution"), \code{input_kind} ("numeric"/"temporal"/
#' "circular"), and \code{needs_args}. Both [metric_catalog()] and
#' [compute_metric_stat()] read from here, so the AI-facing listing and the
#' dispatcher can never disagree about what exists.
#'
#' @return Named list of registry records.
#' @keywords internal
#' @noRd
.metric_primitive_registry <- function() {
  list(
    # location
    prim_mean        = .prim_entry(function(x, args) prim_mean(x), "location",
                                    "Arithmetic mean. Honest for symmetric data; misleading for skewed/heavy-tailed (prefer prim_median)."),
    prim_median      = .prim_entry(function(x, args) prim_median(x), "location",
                                    "Median (50th percentile). Robust to outliers/skew; the honest center for heavy-tailed counts."),
    prim_mode_value  = .prim_entry(function(x, args) prim_mode_value(x), "location",
                                    "Statistical mode (most frequent value; ties take the smallest). For discrete/ordinal columns."),
    # spread
    prim_sd          = .prim_entry(function(x, args) prim_sd(x), "spread",
                                    "Standard deviation. Pairs with prim_mean; assumes a roughly symmetric scale."),
    prim_mad         = .prim_entry(function(x, args) prim_mad(x), "spread",
                                    "Median absolute deviation (normal-consistent). The robust spread to pair with prim_median."),
    prim_iqr         = .prim_entry(function(x, args) prim_iqr(x), "spread",
                                    "Interquartile range (Q3-Q1). Robust spread; basis of the 1.5*IQR outlier rule."),
    prim_range_width = .prim_entry(function(x, args) prim_range_width(x), "spread",
                                    "Range width (max-min). Coarse; very sensitive to single extremes."),
    prim_cv          = .prim_entry(function(x, args) prim_cv(x), "spread",
                                    "Coefficient of variation (sd/mean). Unitless relative dispersion; NA when mean ~0."),
    # extremes / position
    prim_min         = .prim_entry(function(x, args) prim_min(x), "position",
                                    "Minimum observed value."),
    prim_max         = .prim_entry(function(x, args) prim_max(x), "position",
                                    "Maximum observed value (single most extreme point; heavy-tail signal)."),
    prim_quantile    = .prim_entry(function(x, args) prim_quantile(x, args$q), "position",
                                    "Quantile at probability q (type-7). Parameterized; mainly for pinned-replay configs.",
                                    needs_args = TRUE),
    prim_p10         = .prim_entry(function(x, args) prim_p10(x), "position", "10th percentile."),
    prim_p25         = .prim_entry(function(x, args) prim_p25(x), "position", "25th percentile (Q1)."),
    prim_p75         = .prim_entry(function(x, args) prim_p75(x), "position", "75th percentile (Q3)."),
    prim_p90         = .prim_entry(function(x, args) prim_p90(x), "position",
                                    "90th percentile. Honest 'high end' for heavy-tailed data."),
    prim_p95         = .prim_entry(function(x, args) prim_p95(x), "position", "95th percentile."),
    prim_p99         = .prim_entry(function(x, args) prim_p99(x), "position", "99th percentile (extreme tail)."),
    # count / shape
    prim_n           = .prim_entry(function(x, args) prim_n(x), "shape",
                                    "Count of non-missing observations the statistic rests on."),
    prim_n_unique    = .prim_entry(function(x, args) prim_n_unique(x), "shape",
                                    "Number of distinct values. Low vs n suggests a discrete/categorical column."),
    prim_skewness    = .prim_entry(function(x, args) prim_skewness(x), "shape",
                                    "Skewness (moment g1). >0 right-skewed, <0 left, ~0 symmetric."),
    prim_kurtosis_excess = .prim_entry(function(x, args) prim_kurtosis_excess(x), "shape",
                                    "Excess kurtosis (g2; normal=0). >0 means heavier-than-normal tails."),
    # heavy-tail
    prim_log_mean    = .prim_entry(function(x, args) prim_log_mean(x), "heavy_tail",
                                    "Mean of log1p(x). Tail-dampened center for non-negative counts; NA if any value <0."),
    prim_log_sd      = .prim_entry(function(x, args) prim_log_sd(x), "heavy_tail",
                                    "SD of log1p(x). Tail-dampened spread for non-negative counts; NA if any value <0."),
    prim_outlier_count_iqr = .prim_entry(function(x, args) prim_outlier_count_iqr(x), "heavy_tail",
                                    "Count of values beyond the 1.5*IQR Tukey fences (how many outliers the tail produces)."),
    prim_max_to_median_ratio = .prim_entry(function(x, args) prim_max_to_median_ratio(x), "heavy_tail",
                                    "Max/median: scale-free heavy-tail index (1 = no tail). NA when median ~0."),
    # proportional / bounded
    prim_proportion_zero     = .prim_entry(function(x, args) prim_proportion_zero(x), "proportional",
                                    "Proportion of values exactly 0 (zero-inflation)."),
    prim_proportion_nonzero  = .prim_entry(function(x, args) prim_proportion_nonzero(x), "proportional",
                                    "Proportion of values not equal to 0."),
    prim_proportion_above    = .prim_entry(function(x, args) prim_proportion_above(x, args$threshold), "proportional",
                                    "Proportion strictly above a threshold. Needs args$threshold.",
                                    needs_args = TRUE),
    prim_proportion_below    = .prim_entry(function(x, args) prim_proportion_below(x, args$threshold), "proportional",
                                    "Proportion strictly below a threshold. Needs args$threshold.",
                                    needs_args = TRUE),
    prim_proportion_in_range = .prim_entry(function(x, args) prim_proportion_in_range(x, args$lo, args$hi), "proportional",
                                    "Proportion within inclusive [lo, hi]. Needs args$lo and args$hi.",
                                    needs_args = TRUE),
    # categorical
    prim_entropy     = .prim_entry(function(x, args) prim_entropy(x), "categorical",
                                    "Shannon entropy (bits). 0 = single value; log2(k) = uniform over k values. Diversity/concentration."),
    prim_frequency_distribution = .prim_entry(function(x, args) prim_frequency_distribution(x), "categorical",
                                    "Full count-per-distinct-value distribution.", shape = "distribution"),
    prim_top_k_values = .prim_entry(function(x, args) prim_top_k_values(x, args$k), "categorical",
                                    "The k most frequent values and counts. Needs args$k.",
                                    shape = "distribution", needs_args = TRUE),
    prim_top3_values  = .prim_entry(function(x, args) prim_top3_values(x), "categorical",
                                    "The 3 most frequent values and counts.", shape = "distribution"),
    # temporal
    prim_hour_of_day_distribution = .prim_entry(function(x, args) prim_hour_of_day_distribution(x), "temporal",
                                    "Counts of entries per clock hour (0-23, UTC). Time-of-day rhythm.",
                                    shape = "distribution", input_kind = "temporal"),
    prim_day_of_week_distribution = .prim_entry(function(x, args) prim_day_of_week_distribution(x), "temporal",
                                    "Counts of entries per ISO weekday (Mon-Sun, locale-independent).",
                                    shape = "distribution", input_kind = "temporal"),
    prim_month_of_year_distribution = .prim_entry(function(x, args) prim_month_of_year_distribution(x), "temporal",
                                    "Counts of entries per calendar month (Jan-Dec). Seasonality.",
                                    shape = "distribution", input_kind = "temporal"),
    prim_time_span_days = .prim_entry(function(x, args) prim_time_span_days(x), "temporal",
                                    "Span in days from earliest to latest timestamp.",
                                    input_kind = "temporal"),
    prim_median_seconds_between = .prim_entry(function(x, args) prim_median_seconds_between(x), "temporal",
                                    "Median gap (seconds) between consecutive timestamps. Posting cadence.",
                                    input_kind = "temporal"),
    prim_entries_by_month = .prim_entry(function(x, args) prim_entries_by_month(x), "temporal",
                                    "Counts of entries per calendar month (YYYY-MM, chronological). Volume timeline.",
                                    shape = "distribution", input_kind = "temporal"),
    prim_entries_over_time = .prim_entry(function(x, args) prim_entries_over_time(x, args$bin_width_days), "temporal",
                                    "Counts of entries in fixed-width bins of bin_width_days days. Needs args$bin_width_days.",
                                    shape = "distribution", input_kind = "temporal", needs_args = TRUE),
    # circular
    prim_circular_mean     = .prim_entry(function(x, args) prim_circular_mean(x), "circular",
                                    "Directional mean of angles in radians, in [0, 2pi). For periodic quantities where a plain mean is wrong.",
                                    input_kind = "circular"),
    prim_circular_variance = .prim_entry(function(x, args) prim_circular_variance(x), "circular",
                                    "Circular variance (1 - resultant length). 0 = concentrated, 1 = uniformly dispersed.",
                                    input_kind = "circular"),
    prim_peak_hour_circular = .prim_entry(function(x, args) prim_peak_hour_circular(x), "circular",
                                    "Typical time of day on a 24h clock, in [0, 24), via circular mean of times-of-day (correct clock averaging).",
                                    input_kind = "temporal"),
    # distribution-shape test
    prim_shapiro_p   = .prim_entry(function(x, args) prim_shapiro_p(x), "shape_test",
                                    "Shapiro-Wilk normality p-value (small p = non-normal -> justify robust/rank methods).")
  )
}

# ------------------------------------------------------------------------------
# Small-n reliability classification (backend; NOT user config)
# ------------------------------------------------------------------------------
# Which primitives are dispersion / distribution-shape ESTIMATORS whose
# reliability degrades at small n. This is a STATISTICAL property of the
# estimator (a spread or higher-moment / tail statistic on a handful of points is
# fragile), NOT a content classification the researcher supplies -- the same kind
# of backend scaffolding as the registry's `family` field. Robust centers
# (median / mean / mode, log_mean) and plain counts (n, n_unique) are deliberately
# EXCLUDED. The per-column n THRESHOLD is never hardcoded: it is the AI analyst's
# numeric judgement (min_reliable_n in .metric_intelligence_schema), and the
# report only MARKS such cells (it never hides a value). A test asserts every
# name here exists in the registry, so a rename can't silently orphan the set.
.SMALL_N_SENSITIVE_PRIMITIVES <- c(
  "prim_sd", "prim_mad", "prim_iqr", "prim_range_width", "prim_cv",    # spread
  "prim_skewness", "prim_kurtosis_excess",                             # shape (not counts)
  "prim_log_sd", "prim_outlier_count_iqr", "prim_max_to_median_ratio", # heavy-tail spread
  "prim_shapiro_p"                                                     # normality test
)

#' Is a primitive a small-n-sensitive spread/shape estimator?
#'
#' Backend predicate for the per-subtheme reliability flag: TRUE for the
#' dispersion / distribution-shape estimators in
#' \code{.SMALL_N_SENSITIVE_PRIMITIVES}, FALSE for robust centers, counts,
#' positions, proportions, temporal primitives, and any unknown name. Pure
#' lookup; carries no threshold (the threshold is the AI's per-column
#' \code{min_reliable_n}).
#'
#' @param primitive Character primitive name.
#' @return Logical scalar.
#' @keywords internal
.metric_primitive_small_n_sensitive <- function(primitive) {
  isTRUE(as.character(primitive)[1] %in% .SMALL_N_SENSITIVE_PRIMITIVES)
}

#' List the available metric primitives (the AI's catalog)
#'
#' The machine-readable catalog of computational primitives the
#' Methodology Assistant may request. Returned as a tibble (one row per
#' primitive) so it can be inspected, tested, and rendered. Use
#' [format_metric_catalog()] for the human/AI-prompt text form and
#' [metric_catalog_names()] for just the names.
#'
#' This is backend scaffolding: it is not configuration the researcher supplies.
#'
#' @return A tibble with columns \code{primitive}, \code{family},
#'   \code{input_kind}, \code{shape}, \code{needs_args}, \code{description}.
#' @keywords internal
metric_catalog <- function() {
  reg <- .metric_primitive_registry()
  # unname(): vapply carries over the registry's element names; left in place
  # those become per-element column names that surprise downstream `identical()`
  # checks and joins. Columns should be plain unnamed vectors.
  tibble::tibble(
    primitive   = unname(names(reg)),
    family      = unname(vapply(reg, `[[`, character(1), "family")),
    input_kind  = unname(vapply(reg, `[[`, character(1), "input_kind")),
    shape       = unname(vapply(reg, `[[`, character(1), "shape")),
    needs_args  = unname(vapply(reg, `[[`, logical(1), "needs_args")),
    description = unname(vapply(reg, `[[`, character(1), "description"))
  )
}

#' Names of the available metric primitives
#'
#' The character vector of catalog primitive names. Intended for schema
#' construction (e.g. constraining a model request) and for validating
#' pinned-replay configurations against the catalog.
#'
#' @return Character vector of primitive names.
#' @keywords internal
metric_catalog_names <- function() {
  names(.metric_primitive_registry())
}

#' Render the metric catalog as prompt text
#'
#' Groups the catalog by family and renders each primitive as a labeled bullet,
#' for injection into the Methodology Assistant's prompt. Pure
#' formatting over [metric_catalog()].
#'
#' @param catalog Optional pre-fetched catalog tibble (defaults to
#'   [metric_catalog()]).
#' @return A single character string.
#' @keywords internal
format_metric_catalog <- function(catalog = metric_catalog()) {
  fam_order <- c("location", "spread", "position", "shape", "heavy_tail",
                 "proportional", "categorical", "temporal", "circular",
                 "shape_test")
  fams <- intersect(fam_order, unique(catalog$family))
  fams <- c(fams, setdiff(unique(catalog$family), fams))  # any new family last
  lines <- "AVAILABLE METRIC PRIMITIVES (request each by its exact name):"
  for (fam in fams) {
    rows <- catalog[catalog$family == fam, , drop = FALSE]
    lines <- c(lines, "", sprintf("== %s ==", fam))
    for (i in seq_len(nrow(rows))) {
      args_note <- if (isTRUE(rows$needs_args[i])) " [requires args]" else ""
      shape_note <- if (isTRUE(rows$shape[i] == "distribution")) " (distribution)" else ""
      lines <- c(lines, sprintf("- %s%s%s: %s",
                                rows$primitive[i], shape_note, args_note,
                                rows$description[i]))
    }
  }
  paste(lines, collapse = "\n")
}

#' Compute one metric statistic by primitive name (allowlist dispatcher)
#'
#' Looks the requested \code{primitive} up in the registry and computes it over
#' \code{x}. Dispatch is allowlist-only: an unknown name is NEVER resolved to an
#' arbitrary R function. When the name is not in the catalog the function FAILS
#' HONESTLY -- it returns a record with \code{available = FALSE} and a reason
#' naming the gap, and substitutes no alternative statistic (design requirement
#' R4). This lets the report transparently surface "the AI asked for X but X is
#' not available" and signals maintainers to contribute the missing primitive.
#'
#' @param primitive Character(1) name of a catalog primitive
#'   (see [metric_catalog_names()]).
#' @param x The data vector: numeric for most primitives, or a timestamp vector
#'   (POSIXct or epoch-seconds) for temporal primitives.
#' @param args Named list of extra arguments for parameterized primitives
#'   (e.g. \code{list(q = 0.9)}, \code{list(threshold = 0)}). Ignored by
#'   zero-arg primitives.
#' @return A list with \code{primitive}, \code{available} (logical),
#'   \code{family}, \code{shape}, \code{value}, \code{n_observed}, and
#'   \code{reason} (always present for a uniform record shape: \code{NA} when
#'   available, a gap explanation when not). \code{value} is a length-1 numeric
#'   for scalar primitives or a NAMED numeric vector for distribution
#'   primitives, and is \code{NA} / an empty named numeric on degenerate input.
#'   \code{n_observed} is the count of values the primitive actually used (after
#'   dropping NA/NaN/Inf and coercion failures), not the raw input length.
#'
#'   Serialization note: a distribution \code{value} is a named
#'   numeric vector, and \code{jsonlite::toJSON(..., auto_unbox = TRUE)} drops
#'   the names (turning \code{c("09" = 2)} into \code{[2]}). Wrap distribution
#'   values in \code{as.list()} before JSON encoding to preserve the labels (the
#'   same treatment \code{write_corpus_coverage()} already uses).
#' @keywords internal
compute_metric_stat <- function(primitive, x, args = list()) {
  reg <- .metric_primitive_registry()
  nm  <- if (is.character(primitive) && length(primitive) == 1L) primitive else NA_character_

  if (is.na(nm) || !nm %in% names(reg)) {
    return(list(
      primitive  = nm,
      available  = FALSE,
      family     = NA_character_,
      shape      = NA_character_,
      value      = NA_real_,
      n_observed = 0L,
      reason     = sprintf(
        paste0("Primitive '%s' is not in the metric catalog. No statistic was ",
               "substituted (fail-honest). Call metric_catalog() to list the ",
               "available primitives, or contribute the missing primitive."),
        if (is.na(nm)) "<invalid>" else nm)
    ))
  }

  entry <- reg[[nm]]
  if (is.null(args) || !is.list(args)) args <- list()
  # n_observed must reflect the values the primitive actually USES, not the raw
  # input length: the cleaners drop NA/NaN/Inf and coercion failures, so counting
  # via the matching cleaner keeps n_observed honest (it is a load-bearing
  # transparency field the rest of the pipeline reports). "circular" inputs are
  # numeric radians, so they clean as numeric like everything except "temporal".
  n_obs <- if (identical(entry$input_kind, "temporal")) {
    length(.prim_clean_time(x))
  } else {
    length(.prim_clean(x))
  }

  value <- tryCatch(
    entry$fn(x, args),
    error = function(e) {
      logger::log_warn(sprintf("Primitive '%s' errored: %s", nm, conditionMessage(e)))
      if (identical(entry$shape, "distribution")) {
        stats::setNames(numeric(0), character(0))
      } else {
        NA_real_
      }
    }
  )

  list(
    primitive  = nm,
    available  = TRUE,
    family     = entry$family,
    shape      = entry$shape,
    value      = value,
    n_observed = as.integer(n_obs),
    reason     = NA_character_          # present in both branches for a uniform record shape
  )
}
