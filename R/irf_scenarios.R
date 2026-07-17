#' Standard impulse responses for testing MARTIN's model properties
#'
#' A small library of *named, economically-sized* shocks for inspecting the
#' model's dynamic properties (the kind of impulse responses RBA technical
#' notes report). Unlike [sensitivity_matrix()] — which sweeps every adjustable
#' equation with a tiny standardized add-factor — these are a fixed set of
#' interpretable shocks delivered as add-factors via [adjustment_list()] and
#' solved with [solve_martin()].
#'
#' The four standard shocks (see [standard_irf_specs()]):
#'   * `monetary_100bp` — +100bp cash rate (NCR) for one quarter, then the
#'     Taylor rule resumes (a true monetary-policy *shock*).
#'   * `govcons_1pc`    — +1% government consumption (GC) for one quarter.
#'   * `commodity_10pc` — a *permanent* +10% lift to world commodity prices
#'     (WPCOM): an initial +10% level jump, then a small sustained add-factor
#'     that offsets the WPCOM equation's own mean-reversion so the level is held
#'     near +10% over the whole projection.
#'   * `rer_10pc`       — a ~+10% real-exchange-rate (RTWI) appreciation for one
#'     quarter, then it mean-reverts under its own equation.
#'
#' @section Add-factor shocks vs. pinned paths:
#'   Every standard shock here is an add-factor on the relevant equation's
#'   residual, leaving the variable *endogenous* so its own lags carry the
#'   shock forward under its own equation from the first post-shock quarter.
#'   The alternative mechanism — `profiles=` / `exogenize=` in [solve_martin()]
#'   — instead *pins* the variable to an explicit path over a window and then
#'   releases it back to its equation. Both mechanisms propagate to every
#'   downstream equation that reads the shocked variable while it is
#'   perturbed: an earlier version of this note claimed `exogenize` "does not
#'   propagate downstream" — that was wrong (found in audit). A pinned RTWI,
#'   say, still feeds every RTWI-dependent equation (PM, exports, ...) each
#'   solved quarter exactly as an add-factor-shocked RTWI would. The real
#'   distinction is mechanical, not about propagation:
#'     * an add-factor perturbs the equation's *residual*, so the variable's
#'       own dynamics (lag structure, mean-reversion, interaction with the
#'       rest of the model) still shape the path even during the shock window
#'       — the right tool for "what if this equation gets an unexplained
#'       kick" (a genuine impulse response).
#'     * a pinned path (`profiles=`/`exogenize=`) *overrides* the variable
#'       entirely over the window with a value the caller supplies — the
#'       right tool for "hold this variable at this exact level for N
#'       quarters", e.g. the RBA IRF convention of holding RSTAR/GC/GI (and,
#'       in the channel-decomposition variants, RTWI/NTWI/PH) at baseline
#'       while another variable is shocked (see [rba_irf_specs()] /
#'       [run_rba_irf()], and scripts/04's plain "hold at baseline").
#'   Both are legitimate mechanisms; pick the one that matches what you are
#'   conditioning on.
#'
#' @section The "permanent" commodity shock:
#'   This bimets port models real commodity prices (WPCOM) as a mean-reverting
#'   AR process, so a literally-permanent level shift is not a native concept.
#'   The `commodity_10pc` shock therefore counters the equation's own
#'   error-correction (coefficient 0.05) with a small sustained add-factor,
#'   holding WPCOM within roughly +10% to +12% across the horizon. A one-off
#'   +10% jump (set `hold = NA`) instead decays at the model's reversion rate.
#'
#' @section Linearity / scaling:
#'   MARTIN is nonlinear. The +100bp / +1% / +10% sizes here are the
#'   conventional reporting magnitudes, not freely rescalable — re-run with a
#'   custom spec at a different size rather than scaling the table.
NULL

# WPCOM equation error-correction coefficient (see the WPCOM block in
# extdata/model_af/11_world.txt): the level reverts toward its long-run real
# value at this rate per quarter, so a sustained add-factor of
# `rate * log(1+shock)` holds the level shift.
.wpcom_ec_rate <- 0.05

# A loosened add-factor ceiling for the FX / world-price shocks: log(1.10) ~=
# 0.095 exceeds the default 0.02 log_diff ceiling (those defaults guard against
# runaway judgemental tunes, not a deliberate, documented +10% IRF).
.irf_log_ceiling <- function() {
  list(log_diff = 0.12, level = 1.0, percent = 5.0, unknown = 5.0)
}

# Variables reported as a rate (percentage points): an additive deviation, not
# a percent deviation. These are the interest rates, the unemployment rate, and
# the percent-unit trends/spreads. Everything else — including the exchange-rate
# indices RTWI/NTWI/NUSD/REWI (log_diff levels) — is a percent deviation.
.irf_rate_vars <- function() {
  c(
    # unemployment rate + NAIRU
    "LUR", "TLUR",
    # nominal domestic interest rates + spreads
    "NCR", "N2R", "N10R", "NBR", "NMR", "NSP", "NBRSP",
    # real domestic interest rates + neutral rate
    "RCR", "R2R", "RBR", "RMR", "RMRE", "RSTAR",
    # inflation expectations (a percent rate)
    "PI_E",
    # world interest rates + spreads
    "WRR", "WR2R", "WRSP", "WR2SP"
  )
}

#' The four standard impulse-response shock specifications
#'
#' @return A named list of shock specs. Each is delivered as an add-factor and
#'   has: `label` (human text), `equation`, `value` (the per-quarter add-factor,
#'   i.e. the impact-quarter jump), `n_quarters` (length of the explicit shock
#'   window; `NA` = run to the horizon end for a permanent shock), `tail` (see
#'   [adjustment()]), `hold` (`NA`, or a sustained per-quarter add-factor
#'   applied after the first quarter to hold a permanent level shift), `ceiling`
#'   (`NULL`, or a `sibyl.af_ceiling` override for shocks larger than the
#'   default per-units bound).
#' @export
standard_irf_specs <- function() {
  list(
    monetary_100bp = list(
      label      = "Monetary policy: +100bp cash rate (1 quarter)",
      equation   = "NCR",
      value      = 1.00,             # percent units: +100 basis points
      n_quarters = 1L,
      tail       = "zero",
      hold       = NA_real_,
      ceiling    = NULL              # 1.00 < default percent ceiling (5.0)
    ),
    govcons_1pc = list(
      label      = "Government consumption: +1% (1 quarter)",
      equation   = "GC",
      value      = 0.01,             # log_diff: ~ +1% on the quarterly level
      n_quarters = 1L,
      tail       = "zero",
      hold       = NA_real_,
      ceiling    = NULL              # 0.01 < default log_diff ceiling (0.02)
    ),
    commodity_10pc = list(
      label      = "World commodity prices: permanent +10%",
      equation   = "WPCOM",
      value      = log(1.10),        # +10% level jump on impact
      n_quarters = NA_integer_,      # permanent: run to the horizon end
      tail       = "carry",
      hold       = .wpcom_ec_rate * log(1.10),  # offset WPCOM mean-reversion
      ceiling    = .irf_log_ceiling()
    ),
    rer_10pc = list(
      label      = "Real exchange rate: +10% appreciation (1 quarter)",
      equation   = "RTWI",
      value      = log(1.10),        # ~ +10% appreciation on impact
      n_quarters = 1L,
      tail       = "zero",
      hold       = NA_real_,
      ceiling    = .irf_log_ceiling()
    )
  )
}

# Quarter string `offset` quarters after `shock_start` (0 == shock_start).
.quarter_at_offset <- function(shock_start, offset) {
  quarter_offsets(shock_start, n_after = offset)[offset + 1L]
}

# Build the per-quarter add-factor value vector for a spec over `shock_q`:
# the first quarter takes `value`; if `hold` is set, every later quarter takes
# `hold` (a sustained offset that holds a permanent level shift).
.irf_value_vector <- function(spec, n) {
  vals <- rep(spec$value, n)
  if (!is.na(spec$hold) && n > 1L) vals[-1L] <- spec$hold
  vals
}

#' Solve one standard-IRF shock
#'
#' @param database The same database passed to [solve_martin()].
#' @param horizon Length-2 `c("yyyyQq", "yyyyQq")`.
#' @param shock_start `"yyyyQq"` of the first shocked quarter.
#' @param spec One element of [standard_irf_specs()].
#' @param coefficients,estimation_end Passed straight to [solve_martin()].
#' @return The scenario projection tibble (with `solve_martin()`'s attributes,
#'   including `convergence`).
#' @export
run_irf_scenario <- function(database, horizon, shock_start, spec,
                             coefficients = c("frozen", "reestimated"),
                             estimation_end = NULL) {
  coefficients <- match.arg(coefficients)

  # Explicit shock window: a fixed length, or the whole horizon when permanent.
  if (is.na(spec$n_quarters)) {
    shock_q <- quarter_seq(shock_start, horizon[2])
  } else {
    last_q  <- .quarter_at_offset(shock_start, spec$n_quarters - 1L)
    shock_q <- quarter_seq(shock_start, last_q)
  }
  vals <- .irf_value_vector(spec, length(shock_q))

  # Loosen the add-factor ceiling for the (deliberately large) world-price /
  # FX shocks, restoring the prior setting on exit.
  if (!is.null(spec$ceiling)) {
    old <- getOption("sibyl.af_ceiling")
    options(sibyl.af_ceiling = spec$ceiling)
    on.exit(options(sibyl.af_ceiling = old), add = TRUE)
  }

  af <- adjustment_list(
    adjustment(
      equation  = spec$equation,
      horizon   = shock_q,
      value     = vals,
      rationale = paste("standard IRF:", spec$label),
      tail      = spec$tail
    )
  )
  solve_martin(
    database, adjustments = af, horizon = horizon,
    coefficients = coefficients, estimation_end = estimation_end,
    scenario = spec$label
  )
}

#' Deviation of a scenario from baseline at a set of offsets
#'
#' @param baseline,scenario Long projection tibbles
#'   `(variable, quarter, value)`.
#' @param vars Character vector of variables to report.
#' @param shock_start `"yyyyQq"` the shock begins.
#' @param offsets Integer quarter offsets from `shock_start` (0 = impact).
#' @param scenario_key,scenario_label Identifiers written into the output.
#' @param rate_vars Variables reported as an additive (percentage-point)
#'   deviation rather than a percent deviation. Default [.irf_rate_vars()].
#' @return A tidy tibble: one row per (variable, offset) with `baseline`,
#'   `scenario_value`, `deviation`, and `measure` (`"pct"` or `"ppt"`).
#' @export
irf_deviation_table <- function(baseline, scenario, vars, shock_start,
                                offsets, scenario_key, scenario_label,
                                rate_vars = .irf_rate_vars()) {
  base_lk <- lapply(split(baseline, baseline$variable),
                    function(d) stats::setNames(d$value, d$quarter))
  scen_lk <- lapply(split(scenario, scenario$variable),
                    function(d) stats::setNames(d$value, d$quarter))

  rows <- list()
  for (v in vars) {
    bv <- base_lk[[v]]
    sv <- scen_lk[[v]]
    if (is.null(bv) || is.null(sv)) next
    is_rate <- v %in% rate_vars
    for (o in offsets) {
      q <- .quarter_at_offset(shock_start, o)
      if (!q %in% names(bv) || !q %in% names(sv)) next
      b <- unname(bv[q])
      s <- unname(sv[q])
      if (is_rate) {
        dev <- s - b
        measure <- "ppt"
      } else {
        dev <- if (abs(b) > 1e-12) 100 * (s / b - 1) else NA_real_
        measure <- "pct"
      }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        scenario       = scenario_key,
        scenario_label = scenario_label,
        variable       = v,
        offset_q       = as.integer(o),
        quarter        = q,
        baseline       = b,
        scenario_value = s,
        deviation      = dev,
        measure        = measure
      )
    }
  }
  dplyr::bind_rows(rows)
}

#' Run the standard battery of impulse responses
#'
#' Solves a baseline once, then each shock in [standard_irf_specs()] (or a
#' caller-supplied `specs` subset), and returns a tidy table of deviations from
#' baseline for `report_vars` at `offsets` quarters after the shock starts.
#'
#' @param database The database passed to [solve_martin()] (extend exogenous
#'   paths to `horizon[2]` first when projecting past the data end).
#' @param horizon Length-2 `c("yyyyQq","yyyyQq")`.
#' @param shock_start `"yyyyQq"` for the first shocked quarter. Default `NULL`
#'   derives `horizon[2] - max(offsets)` so the longest offset stays in-window.
#' @param baseline Optional pre-computed baseline projection; solved here if
#'   `NULL`.
#' @param report_vars Variables to report deviations on. Default = the headline
#'   aggregates; pass `martin_model_variables("af", "endogenous")` for every
#'   variable the model solves.
#' @param rate_vars Variables reported as a percentage-point deviation rather
#'   than a percent deviation. Default [.irf_rate_vars()]; pass
#'   `c("LUR", "NCR")` to report only the unemployment and cash rates that way.
#' @param offsets Integer quarter offsets from `shock_start` (0 = impact).
#' @param specs Named list of shock specs; default [standard_irf_specs()].
#' @param coefficients,estimation_end Passed to [solve_martin()] (frozen by
#'   default — see the project conventions on the COVID break).
#' @param progress Logical; one line per shock. Default `interactive()`.
#' @return A tidy tibble (rows = scenario x variable x offset). Attributes:
#'   `shock_start`, `offsets`, `baseline` (the baseline projection),
#'   `convergence` (named list of each scenario's convergence diagnostics).
#' @export
standard_irfs <- function(database,
                          horizon,
                          shock_start  = NULL,
                          baseline     = NULL,
                          report_vars  = c("Y", "GNE", "LUR", "P", "PTM",
                                           "NCR", "RTWI", "TOT", "WPCOM"),
                          rate_vars    = .irf_rate_vars(),
                          offsets      = c(0L, 1L, 4L, 8L, 12L, 16L, 20L),
                          specs        = standard_irf_specs(),
                          coefficients = c("frozen", "reestimated"),
                          estimation_end = NULL,
                          progress     = interactive()) {
  coefficients <- match.arg(coefficients)
  stopifnot(length(horizon) == 2L, is.character(horizon))
  offsets <- as.integer(offsets)

  if (is.null(shock_start)) {
    shock_start <- quarter_minus(horizon[2], max(offsets))
  }

  if (is.null(baseline)) {
    if (progress) message("[standard_irfs] solving baseline ...")
    baseline <- solve_martin(
      database, horizon = horizon, scenario = "baseline",
      coefficients = coefficients, estimation_end = estimation_end
    )
  }

  conv <- list()
  rows <- list()
  for (key in names(specs)) {
    spec <- specs[[key]]
    if (progress) message(sprintf("[standard_irfs] shock: %s", spec$label))
    scen <- run_irf_scenario(
      database, horizon, shock_start, spec,
      coefficients = coefficients, estimation_end = estimation_end
    )
    conv[[key]] <- attr(scen, "convergence")
    rows[[key]] <- irf_deviation_table(
      baseline, scen, report_vars, shock_start, offsets,
      scenario_key = key, scenario_label = spec$label,
      rate_vars = rate_vars
    )
  }

  out <- dplyr::bind_rows(rows)
  attr(out, "shock_start") <- shock_start
  attr(out, "offsets")     <- offsets
  attr(out, "baseline")    <- baseline
  attr(out, "convergence") <- conv
  out
}

#' RBA-convention IRF battery: pinned-path shocks (EViews `exclude`/release)
#'
#' A second, additive library of impulse responses that reproduces the
#' shock-delivery convention of the RBA's own MARTIN IRF programs
#' (`martin_rdp_irfs_public.prg` plus its `sub1_inputs`/`sub2_solve`
#' subroutines): the shocked variable is *pinned* to `baseline +/- delta`
#' over an explicit shock window (the EViews `{model}.exclude(m)
#' VAR(start end)` idiom), then released back to its own equation for the
#' rest of the horizon. A small set of variables that would otherwise react
#' to the shock — RSTAR and GC/GI (the neutral real rate and government
#' demand) in every scenario, plus RTWI/NTWI/PH in the channel-decomposition
#' variants — are held at their baseline path over a longer (40-quarter)
#' window so the IRF isolates one transmission channel at a time.
#'
#' This is delivered with the SAME [solve_martin()] primitives used
#' elsewhere in the package (see the "Add-factor shocks vs. pinned paths"
#' section above for how this differs from [standard_irf_specs()]):
#'   * the shocked variable uses `profiles=` — held over the span of its own
#'     supplied quarters, released after — exactly the `exclude(m)`-then-
#'     resume behaviour of the source `.prg`;
#'   * the "switched off" variables use `exogenize=` /
#'     `baseline_for_exogenize=` — held at the already-solved baseline path
#'     over a shared window.
#'
#' @section Scenarios (see [rba_irf_specs()]):
#'   * `monetary`          — s1: +1ppt NCR, 4 quarters.
#'   * `exchange_rate`     — s2: -10% RTWI, 4 quarters.
#'   * `monetary_no_er`    — s3: s1 + RTWI, NTWI held at baseline, 40
#'     quarters (isolates the monetary shock with the exchange-rate channel
#'     shut off).
#'   * `monetary_no_erap`  — s4: s3 + PH held at baseline, 40 quarters
#'     (also shuts the house-price / asset-price channel).
#'   * `monetary_no_erph`  — s9 in the source `.prg`. The `.prg` *labels* this
#'     "no exchange rate or house price", but its `shock_name_s9` variable
#'     list is byte-for-byte identical to `s4`'s
#'     (`ncr rstar gc gi rtwi ntwi ph`); `s9`'s `shock_type`/`shock_length`/
#'     `shock_size` arrays each carry one extra, unused element (8 entries
#'     against 7 `shock_num`s consumed) — a copy-paste artifact, not a
#'     deliberate 4th channel definition. We reproduce the source faithfully:
#'     `monetary_no_erph` shocks the identical variable set as
#'     `monetary_no_erap` (a duplicate under the source's separate label),
#'     not a genuinely distinct "house-price-only" cut.
#'   * `housing_price`     — s5: PH pinned to a caller-supplied percent-
#'     deviation path (default a flat -10% for 10 quarters, matching the
#'     `.prg`'s `Shock_profile.xlsx` "Eviews" import), with NCR and RTWI —
#'     not RSTAR/GC/GI; the source `s5` scenario omits them — held at
#'     baseline for 40 quarters.
#'
#' @param housing_path Numeric vector of percent deviations from baseline PH,
#'   one per quarter of the housing shock (recycled to length if length 1).
#'   Default a flat -10% for `length(housing_path)` quarters (10 by default).
#' @return A named list of scenario specs; each has `label`, `shock_equation`,
#'   `shock_type` (`"ppt"`, `"pct"`, or `"pct_path"`), `shock_size` (for
#'   `"ppt"`/`"pct"`) or `path` (for `"pct_path"`), `shock_length`,
#'   `hold_vars`, `hold_length`.
#' @export
rba_irf_specs <- function(housing_path = rep(-10, 10L)) {
  list(
    monetary = list(
      label          = "RBA IRF: Monetary policy (+1ppt NCR, 4 quarters)",
      shock_equation = "NCR", shock_type = "ppt", shock_size = 1.0,
      shock_length   = 4L,
      hold_vars      = c("RSTAR", "GC", "GI"), hold_length = 40L
    ),
    exchange_rate = list(
      label          = "RBA IRF: Exchange rate (-10% RTWI, 4 quarters)",
      shock_equation = "RTWI", shock_type = "pct", shock_size = -10,
      shock_length   = 4L,
      hold_vars      = c("RSTAR", "GC", "GI"), hold_length = 40L
    ),
    monetary_no_er = list(
      label          = "RBA IRF: Monetary policy, exchange-rate channel off",
      shock_equation = "NCR", shock_type = "ppt", shock_size = 1.0,
      shock_length   = 4L,
      hold_vars      = c("RSTAR", "GC", "GI", "RTWI", "NTWI"),
      hold_length    = 40L
    ),
    monetary_no_erap = list(
      label          = paste("RBA IRF: Monetary policy, exchange-rate +",
                             "house-price channel off"),
      shock_equation = "NCR", shock_type = "ppt", shock_size = 1.0,
      shock_length   = 4L,
      hold_vars      = c("RSTAR", "GC", "GI", "RTWI", "NTWI", "PH"),
      hold_length    = 40L
    ),
    monetary_no_erph = list(
      # See the docstring above: the source `.prg`'s `s9` shock_name list is
      # identical to `s4`'s, so this reproduces `monetary_no_erap` verbatim
      # under the source's separate label rather than a distinct channel cut.
      label          = paste("RBA IRF: Monetary policy, exchange-rate or",
                             "house-price channel off (s9, duplicate of s4",
                             "in source)"),
      shock_equation = "NCR", shock_type = "ppt", shock_size = 1.0,
      shock_length   = 4L,
      hold_vars      = c("RSTAR", "GC", "GI", "RTWI", "NTWI", "PH"),
      hold_length    = 40L
    ),
    housing_price = list(
      label          = "RBA IRF: Housing price path (PH; NCR & RTWI held at baseline)",
      shock_equation = "PH", shock_type = "pct_path", path = housing_path,
      shock_length   = length(housing_path),
      hold_vars      = c("NCR", "RTWI"), hold_length = 40L
    )
  )
}

#' Solve one RBA-convention IRF scenario
#'
#' Implements the EViews `exclude(m) VAR(start end)` / release convention:
#' `spec$shock_equation` is pinned (via [solve_martin()]'s `profiles=`) to
#' `baseline +/- delta` over `spec$shock_length` quarters starting at
#' `shock_start`, then released back to its own equation; `spec$hold_vars`
#' are separately pinned (via `exogenize=`/`baseline_for_exogenize=`) to the
#' baseline path over `spec$hold_length` quarters from the same start.
#'
#' @param database The same database passed to [solve_martin()].
#' @param horizon Length-2 `c("yyyyQq", "yyyyQq")`. `spec$shock_length` and
#'   `spec$hold_length` are windows *starting* at `shock_start`; either is
#'   clipped to `horizon[2]` if the nominal window would run past it (e.g. a
#'   short in-sample horizon can't fit the full 40-quarter "hold" window —
#'   the held variable is then simply held for as much of it as the horizon
#'   covers).
#' @param shock_start `"yyyyQq"` of the first shocked/held quarter.
#' @param spec One element of [rba_irf_specs()].
#' @param baseline Optional pre-solved baseline projection (a tibble from
#'   [solve_martin()]); solved here if `NULL`.
#' @param coefficients,estimation_end Passed to [solve_martin()].
#' @return The scenario projection tibble (`solve_martin()`'s usual shape and
#'   attributes, including `convergence`); also carries attribute `baseline`.
#' @export
run_rba_irf <- function(database, horizon, shock_start, spec,
                        baseline        = NULL,
                        coefficients    = c("frozen", "reestimated"),
                        estimation_end  = NULL) {
  coefficients <- match.arg(coefficients)

  if (is.null(baseline)) {
    baseline <- solve_martin(
      database, horizon = horizon, scenario = "baseline",
      coefficients = coefficients, estimation_end = estimation_end
    )
  }
  base_lk <- stats::setNames(baseline$value,
                             paste(baseline$variable, baseline$quarter))
  get_base <- function(q, var) unname(base_lk[[paste(var, q)]])

  # Clip a window of quarters (starting at shock_start) to horizon[2]: a
  # short horizon (e.g. an in-sample test window) can't always fit the full
  # 40-quarter "hold" window, so hold for as much of it as the horizon covers.
  h_end     <- parse_quarter(horizon[2])
  h_end_idx <- quarter_index(h_end$year, h_end$quarter)
  clip_to_horizon <- function(qs) {
    p <- parse_quarter(qs)
    qs[quarter_index(p$year, p$quarter) <= h_end_idx]
  }

  # ---- the shocked variable: pin baseline +/- delta over its own window,
  # then release. `profiles=` imposes exactly this: held over the span of
  # the supplied quarters, solved freely again after.
  shock_qs <- clip_to_horizon(
    quarter_offsets(shock_start, n_after = spec$shock_length - 1L)
  )
  if (length(shock_qs) == 0L) {
    stop("run_rba_irf: `horizon` ends before `shock_start`.", call. = FALSE)
  }
  base_vals <- vapply(shock_qs, get_base, numeric(1), var = spec$shock_equation)
  if (anyNA(base_vals)) {
    stop("run_rba_irf: baseline has no value for '", spec$shock_equation,
         "' over the shock window (extend `horizon`/`database`).",
         call. = FALSE)
  }
  shock_vals <- switch(
    spec$shock_type,
    ppt      = base_vals + spec$shock_size,
    pct      = base_vals * (1 + spec$shock_size / 100),
    pct_path = base_vals * (1 + rep_len(spec$path, length(shock_qs)) / 100),
    stop("run_rba_irf: unknown shock_type '", spec$shock_type, "'.",
         call. = FALSE)
  )
  profiles <- data.frame(
    variable = spec$shock_equation, quarter = shock_qs, value = shock_vals,
    stringsAsFactors = FALSE
  )

  # ---- the "switched off" variables: pinned to baseline over a shared
  # window, then released — the RBA's channel-decomposition convention.
  exogenize       <- character(0)
  exogenize_range <- NULL
  if (length(spec$hold_vars) > 0L) {
    hold_qs <- clip_to_horizon(
      quarter_offsets(shock_start, n_after = spec$hold_length - 1L)
    )
    if (length(hold_qs) > 0L) {
      exogenize       <- spec$hold_vars
      exogenize_range <- c(shock_start, hold_qs[length(hold_qs)])
    }
  }

  out <- solve_martin(
    database, horizon = horizon, profiles = profiles,
    exogenize = exogenize,
    baseline_for_exogenize = if (length(exogenize) > 0L) baseline else NULL,
    exogenize_range = exogenize_range,
    coefficients = coefficients, estimation_end = estimation_end,
    scenario = spec$label
  )
  attr(out, "baseline") <- baseline
  out
}
