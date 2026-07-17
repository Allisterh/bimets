# Extend exogenous variables forward into a forecast horizon.
#
# Companion to extend_residual_with_decay() — that handler covers
# the replay add-factors, but bimets::SIMULATE() also needs every variable
# in TSRANGE to have a defined value. For genuinely future horizons that
# means we need to populate exogenous variables (dummies, anchors, the
# tail of any variable we treat as given) past their data end.
#
# Three modes per variable:
#
#   "carry"    — hold the last observed value forward (good for dummies
#                 at zero, anchored series like PI_TARGET).
#   "constant" — fill with a specified scalar.
#   "linear"   — extrapolate the linear trend of the last `n` periods
#                 (good for time-trend variables like PIBN_TREND_1).

# Series where the generic "hold the last observed value" (carry) rule is
# not the right long-run assumption, so "carry_all" layers these overrides
# on top rather than leaving them to whatever the last historical print
# happened to be.
#
#   RAIN — the EViews-vendored rainfall deviation series (see
#   series_catalogue.csv) has no live public source and is fixture-only.
#   Rainfall deviations average zero over the long run, so XAG =
#   XAGEXRAIN + RAIN should collapse to XAG = XAGEXRAIN beyond history —
#   held at a constant 0, not carried from the last historical deviation.
#   This mirrors the EViews reference (`modify_data.prg`: `RAIN=0` for
#   every quarter past the last real print). RAIN's own fixture history
#   already happens to end at 0 (last nonzero print 2008Q2), so carry and
#   constant-0 coincide today, but the explicit rule keeps that guaranteed
#   rather than incidental.
DEFAULT_RULE_OVERRIDES <- list(
  RAIN = list(mode = "constant", value = 0)
)

#' Extend variables in a database forward to a target quarter
#'
#' For each named entry in `rules`, extends the matching series in
#' `database` so it covers up to `end_quarter`. Quarters that already have
#' data are not overwritten. Series not in `rules` are left untouched —
#' the caller chooses which variables get extended.
#'
#' @param database Named list of bimets/base TIMESERIES.
#' @param end_quarter Character `"yyyyQq"` — the inclusive end of the
#'   extension range.
#' @param rules Named list keyed by martin_var. Each entry is a list with
#'   at least `mode` ∈ \{`"carry"`, `"constant"`, `"linear"`\}; mode
#'   `"constant"` also takes `value`; mode `"linear"` takes optional
#'   `lookback` (default 4 quarters). If `rules = "carry_all"`, every
#'   series in `database` is carried forward, except for the known
#'   overrides in `DEFAULT_RULE_OVERRIDES` (currently just `RAIN`).
#' @return The database with extended series.
#' @export
extend_exogenous <- function(database, end_quarter,
                              rules = "carry_all") {
  stopifnot(is.character(end_quarter), length(end_quarter) == 1L)
  yq <- parse_yyyyQq(end_quarter)
  target_dec <- yq$year + (yq$quarter - 1) / 4

  if (identical(rules, "carry_all")) {
    rules <- stats::setNames(
      lapply(names(database), function(.) list(mode = "carry")),
      names(database)
    )
    overrides <- DEFAULT_RULE_OVERRIDES[intersect(names(DEFAULT_RULE_OVERRIDES),
                                                    names(database))]
    rules[names(overrides)] <- overrides
  }
  if (!is.list(rules) || is.null(names(rules))) {
    stop("`rules` must be 'carry_all' or a named list keyed by martin_var.",
         call. = FALSE)
  }

  for (mv in names(rules)) {
    ts <- database[[mv]]
    if (is.null(ts)) next
    rule <- rules[[mv]]
    mode <- rule$mode %||% "carry"
    database[[mv]] <- extend_one(ts, target_dec, rule, mode)
  }
  database
}

# Extend one ts to `target_dec` per `rule`. Returns ts unchanged if the last
# genuine observation already reaches both the target and the ts span end.
extend_one <- function(ts, target_dec, rule, mode) {
  tsp  <- stats::tsp(ts)
  vals <- as.numeric(ts)
  # Seed from the last GENUINE observation, not the ts span end: a series can
  # carry trailing NAs inside its declared span (e.g. last print 2025Q2 but tsp
  # ends 2025Q4). Extending only past the span end would leave those interior
  # NAs for SIMULATE to hit in-range. Fill every cell after the last non-NA
  # value, up to the later of the target and the span end (so we never truncate
  # the original span).
  last_idx <- suppressWarnings(max(which(!is.na(vals))))
  if (!is.finite(last_idx)) {
    last_idx      <- length(vals)
    last_real_dec <- tsp[2]
    seed          <- 0
  } else {
    last_real_dec <- tsp[1] + (last_idx - 1L) / 4
    seed          <- vals[last_idx]
  }
  fill_to_dec <- max(target_dec, tsp[2])
  if (fill_to_dec <= last_real_dec + 1e-9) return(ts)
  n_fill <- round((fill_to_dec - last_real_dec) * 4)

  ext <- switch(mode,
    carry    = rep(seed, n_fill),
    constant = rep(rule$value %||% seed, n_fill),
    linear   = linear_extrapolate(vals, n_fill, rule$lookback %||% 4L),
    stop("Unknown extension mode: ", mode, call. = FALSE)
  )

  start_year    <- floor(tsp[1] + 1e-9)
  start_quarter <- round((tsp[1] - start_year) * 4 + 1)
  bimets::TIMESERIES(c(vals[seq_len(last_idx)], ext),
                     START = c(start_year, start_quarter), FREQ = 4)
}

linear_extrapolate <- function(vals, n_new, lookback) {
  last_idx <- suppressWarnings(max(which(!is.na(vals))))
  if (!is.finite(last_idx) || last_idx < 2L) {
    return(rep(if (is.finite(last_idx)) vals[last_idx] else 0, n_new))
  }
  hist_start <- max(1L, last_idx - lookback + 1L)
  hist_vals  <- vals[hist_start:last_idx]
  # Average per-period change
  slope <- mean(diff(hist_vals), na.rm = TRUE)
  if (!is.finite(slope)) slope <- 0
  vals[last_idx] + slope * seq_len(n_new)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
