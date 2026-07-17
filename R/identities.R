# Handlers for MARTIN's deterministic identity-chain inputs:
#
#   IBCTR   — corporate tax rate (piecewise-constant per
#             the EViews MARTIN import_data.prg:535-562)
#   IBNDR   — non-mining business depreciation rate (computed from
#             annual ABS 5204.0 capital-stock and consumption-of-fixed-
#             capital series, interpolated to quarterly)
#   IBNDRA  — annual depreciation rate; identity = IBNDR + 3 lags
#   RBR     — real business rate; identity from IBCTR, NBR, PTM
#   IBCR    — user cost of capital; identity from RBR, IBNDRA, IBCTR,
#             N10R, PIBN, PGNE
#
#   IAD_W_C/I/GI/GC/X — import-adjusted demand weights from ABS
#                       input-output tables (1999, 2002, 2005-2010,
#                       2013-2017), interpolated to quarterly. Vendored
#                       directly from the EViews io_calcs.prg output
#                       (`inst/extdata/iad_weights.csv` = a copy of
#                       the EViews MARTIN t_iad.csv).
#
# IBCR's identity chain (from MARTINMOD_AF.txt:654-681) cascades:
#   IBCR  = (RBR/100 + IBNDRA/100) * ((1 - IBCTR*X) / (1 - IBCTR)) * (PIBN/PGNE)
#   X     = IBNDRA/100 * (1 + N10R/100) / (N10R/100 + IBNDRA/100)
#   RBR   = (1 + (1 - IBCTR) * NBR/100) / (lag(PTM,1)/lag(PTM,5)) * 100 - 100
#   IBNDRA = IBNDR + lag(IBNDR,1) + lag(IBNDR,2) + lag(IBNDR,3)
#
# So once IBCTR, IBNDR are in the database, IBNDRA, RBR, IBCR all follow
# as derived formula rows.

# Piecewise IBCTR breakpoints from import_data.prg:535-562. Each row:
# (from quarter, value). The series is constant at `value` from `from`
# onward, until the next breakpoint.
IBCTR_BREAKPOINTS <- list(
  list(from = "1959Q3", value = 0.40),   # @first 1963q2 — use first-available as start
  list(from = "1963Q3", value = 0.425),
  list(from = "1967Q3", value = 0.45),
  list(from = "1969Q3", value = 0.475),
  list(from = "1973Q3", value = 0.45),
  list(from = "1974Q3", value = 0.425),
  list(from = "1976Q3", value = 0.46),
  list(from = "1986Q3", value = 0.49),
  list(from = "1988Q3", value = 0.39),
  list(from = "1993Q3", value = 0.33),
  list(from = "1995Q3", value = 0.36),
  list(from = "1999Q3", value = 0.34),
  list(from = "2000Q3", value = 0.30)
)

#' Apply the IBCTR piecewise-constant series
#'
#' Builds IBCTR (Australian corporate tax rate) as a piecewise-constant
#' series over the database's span using the breakpoint table from
#' import_data.prg:535-562.
#' Idempotent: leaves IBCTR alone if already present.
#'
#' @param database Named list of bimets ts.
#' @param catalogue [series_catalogue()] (unused in v0).
#' @return Database with IBCTR added.
#' @keywords internal
apply_ibctr <- function(database, catalogue = series_catalogue()) {
  if (!is.null(database[["IBCTR"]])) return(database)
  span <- database_span(database)
  n <- span$n_quarters
  y0 <- span$start_year
  q0 <- span$start_quarter

  vals <- rep(NA_real_, n)
  for (bp in IBCTR_BREAKPOINTS) {
    from_yq <- parse_yyyyQq(bp$from)
    from_idx <- (from_yq$year - y0) * 4L + (from_yq$quarter - q0) + 1L
    from_idx <- max(1L, from_idx)
    if (from_idx > n) next
    vals[from_idx:n] <- bp$value
  }
  # Pre-first-breakpoint quarters: carry the earliest value backward.
  first_bp_yq <- parse_yyyyQq(IBCTR_BREAKPOINTS[[1]]$from)
  first_bp_idx <- (first_bp_yq$year - y0) * 4L +
                   (first_bp_yq$quarter - q0) + 1L
  if (first_bp_idx > 1L) {
    vals[seq_len(min(first_bp_idx - 1L, n))] <-
      IBCTR_BREAKPOINTS[[1]]$value
  }

  database[["IBCTR"]] <- bimets::TIMESERIES(
    vals, START = c(y0, q0), FREQ = 4
  )
  database
}

#' Apply the IAD weights from vendored io_calcs.prg output
#'
#' Reads `inst/extdata/iad_weights.csv` (a verbatim copy of
#' `the EViews MARTIN t_iad.csv` — the output of EViews's
#' `io_calcs.prg`, which interpolates ABS input-output table omega
#' values across 13 years) and inserts each of the five IAD weight
#' series into the database.
#'
#' Vendoring the CSV avoids reimplementing the IO-table extraction (13
#' year-specific XLS files with year-specific cell ranges) — a session
#' unto itself with limited additional fidelity since the omega values
#' only update when ABS releases new IO tables. A separate follow-up
#' could add a `update_iad_weights()` that reads fresh IO tables.
#'
#' @inheritParams apply_ibctr
#' @return Database with IAD_W_C, IAD_W_GC, IAD_W_GI, IAD_W_I, IAD_W_X
#'   added.
#' @keywords internal
apply_iad_weights <- function(database, catalogue = series_catalogue()) {
  weights <- utils::read.csv(extdata_path("iad_weights.csv"), check.names = FALSE)
  # First column is the quarter label (e.g. "1959Q3"); read.csv may give
  # it an empty name, so use positional index. The remaining columns are
  # the five weights.
  qs    <- weights[[1]]
  vars  <- c("IAD_W_C", "IAD_W_GC", "IAD_W_GI", "IAD_W_I", "IAD_W_X")
  for (v in vars) {
    if (!is.null(database[[v]])) next
    if (!(v %in% names(weights))) next  # column missing; skip
    yq <- parse_yyyyQq(qs[1])
    database[[v]] <- bimets::TIMESERIES(
      as.numeric(weights[[v]]),
      START = c(yq$year, yq$quarter), FREQ = 4
    )
  }
  database
}

#' Apply a static IBNDR placeholder series
#'
#' Builds a constant `IBNDR` (quarterly depreciation rate, percent) at
#' the value `IBNDR_STATIC` (currently 1.5, the midpoint of the
#' fixture's [1.14, 1.65] range). This is a v0 placeholder; a faithful
#' port would compute IBNDR from annual ABS 5204.0 capital-stock and
#' consumption-of-fixed-capital series via:
#'
#'   CFCIBN_annual = CFCTOT - CFCID - CFCOTC - CFCIBRE
#'   KIBN_annual   = KTOT   - KID   - KOTC   - KIBRE
#'   IBNDRA_annual = 100 * (CFCIBN / lag(KIBN, 1))
#'   IBNDR_quarterly = ((1 + IBNDRA_annual/100)^(1/4) - 1) * 100
#'                     then linearly interpolated annual→quarterly
#'
#' The four CFC* catalogue rows are present (annual ABS series via
#' Chow-Lin) so a future iteration can flip from this static value to
#' the proper interpolation.
#'
#' @inheritParams apply_ibctr
#' @keywords internal
IBNDR_STATIC <- 1.5

apply_ibndr <- function(database, catalogue = series_catalogue()) {
  if (!is.null(database[["IBNDR"]])) return(database)
  span <- database_span(database)
  database[["IBNDR"]] <- bimets::TIMESERIES(
    rep(IBNDR_STATIC, span$n_quarters),
    START = c(span$start_year, span$start_quarter),
    FREQ  = 4
  )
  database
}

#' Apply the annual-data-driven IBNDR depreciation rate
#'
#' Faithful port of modify_data.prg:487-500.
#' Computes annual non-mining business depreciation rate from the four
#' annual ABS 5204.0 series (CFCTOT, CFCID, CFCOTC, CFCIBRE for
#' consumption-of-fixed-capital; KTOT, KID, KOTC, KIBRE for capital
#' stock), converts to a quarterly compounding rate, linearly
#' interpolates to the quarterly grid, then backcasts to the start of
#' the database and forward-fills to the end.
#'
#'   CFCIBN_annual  = CFCTOT - CFCID - CFCOTC - CFCIBRE
#'   KIBN_annual    = KTOT   - KID   - KOTC   - KIBRE
#'   IBNDRA_annual  = 100 * (CFCIBN_t / KIBN_{t-1})       # annual rate
#'   IBNDR_quarterly = ((1 + IBNDRA_annual/100)^(1/4) - 1) * 100
#'
#' Replaces the static [apply_ibndr] placeholder when the annual inputs
#' are available; falls back to the placeholder if any input is missing.
#'
#' @param database Named list of bimets ts (quarterly).
#' @param annual_db Named list of `ts` (annual, frequency 1).
#' @inheritParams apply_ibctr
#' @keywords internal
apply_ibndr_annual <- function(database, annual_db,
                               catalogue = series_catalogue()) {
  if (!is.null(database[["IBNDR"]])) return(database)
  required <- c("CFCTOT", "CFCID", "CFCOTC", "CFCIBRE",
                "KTOT", "KID", "KOTC", "KIBRE")
  for (v in required) {
    if (is.null(annual_db[[v]])) {
      # At least one input missing — fall through to static handler.
      return(database)
    }
  }
  # Align annual series to the smallest common range.
  starts <- sapply(annual_db[required], function(x) stats::tsp(x)[1])
  ends   <- sapply(annual_db[required], function(x) stats::tsp(x)[2])
  start_year <- floor(max(starts) + 1e-9)
  end_year   <- floor(min(ends) + 1e-9)
  if (start_year >= end_year) return(database)

  align_annual <- function(x) {
    tsp <- stats::tsp(x); s <- floor(tsp[1] + 1e-9)
    lo <- start_year - s + 1L
    hi <- end_year   - s + 1L
    as.numeric(x)[lo:hi]
  }
  cfctot  <- align_annual(annual_db$CFCTOT)
  cfcid   <- align_annual(annual_db$CFCID)
  cfcotc  <- align_annual(annual_db$CFCOTC)
  cfcibre <- align_annual(annual_db$CFCIBRE)
  ktot    <- align_annual(annual_db$KTOT)
  kid     <- align_annual(annual_db$KID)
  kotc    <- align_annual(annual_db$KOTC)
  kibre   <- align_annual(annual_db$KIBRE)

  cfcibn <- cfctot - cfcid - cfcotc - cfcibre
  kibn   <- ktot   - kid   - kotc   - kibre

  # Annual depreciation rate using lag-1 (previous year's stock).
  # Year-1 entry has no lag, so it's NA.
  n_ann <- length(cfcibn)
  ibndra_ann <- rep(NA_real_, n_ann)
  for (i in seq.int(2L, n_ann)) {
    if (!is.na(kibn[i - 1L]) && kibn[i - 1L] > 0 && !is.na(cfcibn[i])) {
      ibndra_ann[i] <- 100 * cfcibn[i] / kibn[i - 1L]
    }
  }
  # Annual rate to quarterly compounding rate.
  ibndr_qrate_ann <- ((1 + ibndra_ann / 100) ^ (1 / 4) - 1) * 100

  # Interpolate annual rates to quarterly grid. Each annual value applies
  # to its fiscal-year-end (typically Q2). We place the value at Q2 of
  # each year and linearly interpolate between, then backcast and
  # forward-fill.
  qspan <- database_span(database)
  qn <- qspan$n_quarters
  qy0 <- qspan$start_year
  qq0 <- qspan$start_quarter

  # Annual anchor positions: year y maps to quarter index (y - qy0)*4 + 2
  # (interpret as Q2 of year y), with adjustment for the database's
  # starting offset.
  ibndr_q <- rep(NA_real_, qn)
  ann_years <- seq(start_year, end_year)
  for (i in seq_along(ann_years)) {
    yr <- ann_years[i]
    # Quarter index in database grid for Q2 of yr:
    idx <- (yr - qy0) * 4L + (2L - qq0) + 1L
    if (idx >= 1L && idx <= qn && !is.na(ibndr_qrate_ann[i])) {
      ibndr_q[idx] <- ibndr_qrate_ann[i]
    }
  }
  # Linear interpolation between non-NA anchors.
  nonna_pos <- which(!is.na(ibndr_q))
  if (length(nonna_pos) >= 2L) {
    for (k in seq_len(length(nonna_pos) - 1L)) {
      lo <- nonna_pos[k]
      hi <- nonna_pos[k + 1L]
      if (hi - lo > 1L) {
        ibndr_q[(lo + 1L):(hi - 1L)] <- seq(
          ibndr_q[lo], ibndr_q[hi],
          length.out = hi - lo + 1L
        )[-c(1, hi - lo + 1L)]
      }
    }
  }
  if (length(nonna_pos) >= 1L) {
    # Backcast: carry first non-NA value backward.
    first <- nonna_pos[1]
    if (first > 1L) ibndr_q[seq_len(first - 1L)] <- ibndr_q[first]
    # Forward-fill: carry last non-NA value forward.
    last <- nonna_pos[length(nonna_pos)]
    if (last < qn) ibndr_q[(last + 1L):qn] <- ibndr_q[last]
  }

  if (all(is.na(ibndr_q))) return(database)  # fell through

  database[["IBNDR"]] <- bimets::TIMESERIES(
    ibndr_q, START = c(qy0, qq0), FREQ = 4
  )
  database
}

# ---- KIBN, IBREDR, IDDR (from-raw constructors) ----------------------------
#
# Three more BEHAVIORAL-equation historical inputs the live/raw-CSV pipeline
# previously could only get from the bundled fixture (audit gap for R3):
# KIBN, IBREDR and IDDR all appear in `extdata/model_af/` as
# `BEHAVIORAL> ... EQ> x = c1*(...)` persistence forms, so — like WRR/WR2R/
# NULCBS below — they need a genuine historical series to estimate against,
# not just an identity chain evaluated once per solve.

#' Disaggregate annual KIBN (non-mining, non-dwelling business capital
#' stock) to quarterly
#'
#' Faithful port of modify_data.prg:478 (`KIBN = KTOT - KID - KOTC - KIBRE`,
#' evaluated on the Rannual page) followed by the same Q2-anchored
#' stock interpolation already used for KID/KIBRE/KTOT/KOTC
#' ([disaggregate_annual_stock()]). Idempotent: leaves KIBN alone if
#' already present; skips if any of the four annual inputs is missing.
#'
#' @param database Named list of bimets ts (quarterly).
#' @param annual_db Named list of `ts` (annual, frequency 1); needs KTOT,
#'   KID, KOTC, KIBRE (the same annual capital-stock series the chowlin
#'   slice of the catalogue already requires).
#' @inheritParams apply_ibctr
#' @keywords internal
apply_kibn <- function(database, annual_db, catalogue = series_catalogue()) {
  if (!is.null(database[["KIBN"]])) return(database)
  required <- c("KTOT", "KID", "KOTC", "KIBRE")
  if (!all(required %in% names(annual_db))) return(database)

  starts <- sapply(annual_db[required], function(x) stats::tsp(x)[1])
  ends   <- sapply(annual_db[required], function(x) stats::tsp(x)[2])
  start_year <- floor(max(starts) + 1e-9)
  end_year   <- floor(min(ends) + 1e-9)
  if (start_year > end_year) return(database)

  align_annual <- function(x) {
    tsp <- stats::tsp(x); s <- floor(tsp[1] + 1e-9)
    lo <- start_year - s + 1L
    hi <- end_year   - s + 1L
    as.numeric(x)[lo:hi]
  }
  kibn_vals <- align_annual(annual_db$KTOT)  - align_annual(annual_db$KID) -
               align_annual(annual_db$KOTC) - align_annual(annual_db$KIBRE)
  kibn_ann <- stats::ts(kibn_vals, start = start_year, frequency = 1)

  span <- database_span(database)
  q <- try(disaggregate_annual_stock(kibn_ann, span), silent = TRUE)
  if (!inherits(q, "try-error") && !is.null(q)) database[["KIBN"]] <- q
  database
}

# Shared by apply_ibredr_annual() / apply_iddr_annual(): the annual
# depreciation rate implied by the capital-accumulation identity
#   stock_t = stock_(t-1) * (1 - rate/100) + flow_t
# rearranged to  rate[t] = 100 * (1 - (stock[t] - flow[t]) / stock[t-1]).
# This is the mining/dwelling-specific counterpart of apply_ibndr_annual()'s
# CFC-based rate -- modify_data.prg uses investment/stock directly here
# (lines 369, 436) rather than consumption-of-fixed-capital.
.annual_stock_flow_depreciation_rate <- function(annual_db, stock_var, flow_var) {
  if (is.null(annual_db[[stock_var]]) || is.null(annual_db[[flow_var]])) {
    return(NULL)
  }
  stock_ts <- annual_db[[stock_var]]
  flow_ts  <- annual_db[[flow_var]]
  s_tsp <- stats::tsp(stock_ts); f_tsp <- stats::tsp(flow_ts)
  start_year <- floor(max(s_tsp[1], f_tsp[1]) + 1e-9)
  end_year   <- floor(min(s_tsp[2], f_tsp[2]) + 1e-9)
  if (start_year >= end_year) return(NULL)

  align_annual <- function(x) {
    tsp <- stats::tsp(x); s <- floor(tsp[1] + 1e-9)
    lo <- start_year - s + 1L
    hi <- end_year   - s + 1L
    as.numeric(x)[lo:hi]
  }
  stock <- align_annual(stock_ts)
  flow  <- align_annual(flow_ts)

  n <- length(stock)
  rate_ann <- rep(NA_real_, n)
  for (i in seq.int(2L, n)) {
    if (!is.na(stock[i - 1L]) && stock[i - 1L] > 0 &&
        !is.na(stock[i]) && !is.na(flow[i])) {
      rate_ann[i] <- 100 * (1 - (stock[i] - flow[i]) / stock[i - 1L])
    }
  }
  list(rate_ann = rate_ann, years = seq(start_year, end_year))
}

# Shared by apply_ibredr_annual() / apply_iddr_annual() / apply_ibndr_annual()
# in spirit (not code -- apply_ibndr_annual() predates this helper and is left
# untouched to avoid disturbing a tested function): converts an annual %-rate
# (one value per calendar year) to a quarterly compounding rate, anchors each
# year's value at Q2, linearly interpolates between anchors, then
# backcasts/forward-fills the ends -- identical placement logic to
# apply_ibndr_annual()'s IBNDR construction.
.annual_rate_to_quarterly <- function(rate_ann, years, database) {
  qrate_ann <- ((1 + rate_ann / 100) ^ (1 / 4) - 1) * 100
  span <- database_span(database)
  qn <- span$n_quarters; qy0 <- span$start_year; qq0 <- span$start_quarter

  q <- rep(NA_real_, qn)
  for (i in seq_along(years)) {
    idx <- (years[i] - qy0) * 4L + (2L - qq0) + 1L
    if (idx >= 1L && idx <= qn && !is.na(qrate_ann[i])) q[idx] <- qrate_ann[i]
  }
  nonna_pos <- which(!is.na(q))
  if (length(nonna_pos) >= 2L) {
    for (k in seq_len(length(nonna_pos) - 1L)) {
      lo <- nonna_pos[k]; hi <- nonna_pos[k + 1L]
      if (hi - lo > 1L) {
        q[(lo + 1L):(hi - 1L)] <- seq(
          q[lo], q[hi], length.out = hi - lo + 1L
        )[-c(1, hi - lo + 1L)]
      }
    }
  }
  if (length(nonna_pos) >= 1L) {
    first <- nonna_pos[1]
    if (first > 1L) q[seq_len(first - 1L)] <- q[first]
    last <- nonna_pos[length(nonna_pos)]
    if (last < qn) q[(last + 1L):qn] <- q[last]
  }
  if (all(is.na(q))) return(NULL)
  bimets::TIMESERIES(q, START = c(qy0, qq0), FREQ = 4)
}

#' Apply the annual-data-driven IBREDR (mining depreciation rate)
#'
#' Faithful port of modify_data.prg:369-371. Uses the same annual KIBRE /
#' IBRE inputs the chowlin slice of the catalogue already disaggregates to
#' quarterly (`annual_db$KIBRE`, `annual_db$IBRE` -- the EViews source's
#' `KIBREA2` / `IBREA2`), so no new raw primitive is required beyond what
#' [apply_chowlin()] already needs. Idempotent; skips if the annual inputs
#' are unavailable.
#'
#' @inheritParams apply_kibn
#' @keywords internal
apply_ibredr_annual <- function(database, annual_db,
                                catalogue = series_catalogue()) {
  if (!is.null(database[["IBREDR"]])) return(database)
  rf <- .annual_stock_flow_depreciation_rate(annual_db, "KIBRE", "IBRE")
  if (is.null(rf)) return(database)
  q <- .annual_rate_to_quarterly(rf$rate_ann, rf$years, database)
  if (!is.null(q)) database[["IBREDR"]] <- q
  database
}

#' Apply the annual-data-driven IDDR (dwelling depreciation rate)
#'
#' Faithful port of modify_data.prg:436-438. Unlike IBREDR, the EViews
#' source computes this from a *separate* annual ABS 5204.0 dwelling
#' investment series (`A2422531C`, import_data.prg:513) from the one behind
#' the catalogue's quarterly `ID` (ABS 5206.0, `A2304098T`, transformation
#' `direct`) -- reusing the "ID" name here would silently mix a real
#' chain-volume quarterly series with a nominal annual capital-stock-
#' consistent one. Raw-CSV callers therefore supply this annual input under
#' its own name, `IDA2` ([read_raw_csv_database()]'s annual CSV), paired
#' with `annual_db$KID` (already required by [apply_chowlin()] /
#' [apply_ibndr_annual()]).
#'
#' @inheritParams apply_kibn
#' @keywords internal
apply_iddr_annual <- function(database, annual_db,
                              catalogue = series_catalogue()) {
  if (!is.null(database[["IDDR"]])) return(database)
  rf <- .annual_stock_flow_depreciation_rate(annual_db, "KID", "IDA2")
  if (is.null(rf)) return(database)
  q <- .annual_rate_to_quarterly(rf$rate_ann, rf$years, database)
  if (!is.null(q)) database[["IDDR"]] <- q
  database
}

# ---- NULCBS, WRR, WR2R, RMR, IAD (pure-algebra from-raw constructors) -----
#
# Four more BEHAVIORAL/data-only variables built from already-catalogued
# quarterly primitives. Unlike KIBN/IBREDR/IDDR these need no annual data --
# just direct algebra on series the direct/derived pipeline already
# populates -- so they're plain formula evaluators, not chowlin-style
# disaggregations.

# Trend coefficient from modify_data.prg:255
# (`nulcbs = nulc*(0.8+0.2*exp(0.002746*@trend))`).
NULCBS_TREND_COEF <- 0.002746

#' Apply NULCBS (Balassa-Samuelson-adjusted nominal unit labour cost)
#'
#' Faithful port of modify_data.prg:255. `@trend` is EViews's quarters-
#' since-workfile-start counter; we reuse the same
#' `REFERENCE_TREND_ZERO` ("1959Q3") convention [dummies.R] anchors its
#' `trend_carry` dummies to, so the absolute calendar quarter (not the
#' database's own start) determines the trend value. Must run after NULC
#' is available (i.e. after [add_derived_series()], since NULC = NHCOE / Y
#' is itself a catalogue-derived row). Idempotent.
#'
#' @inheritParams apply_ibctr
#' @keywords internal
apply_nulcbs <- function(database, catalogue = series_catalogue()) {
  if (!is.null(database[["NULCBS"]])) return(database)
  nulc <- database[["NULC"]]
  if (is.null(nulc)) return(database)

  tsp <- stats::tsp(nulc)
  start_year    <- floor(tsp[1] + 1e-9)
  start_quarter <- round((tsp[1] - start_year) * 4 + 1)
  n <- length(as.numeric(nulc))

  ref <- parse_yyyyQq(REFERENCE_TREND_ZERO)
  base_offset <- .dummies_quarter_index(start_year, start_quarter,
                                        ref$year, ref$quarter)
  trend <- base_offset + seq.int(0L, n - 1L)

  vals <- as.numeric(nulc) * (0.8 + 0.2 * exp(NULCBS_TREND_COEF * trend))
  database[["NULCBS"]] <- bimets::TIMESERIES(
    vals, START = c(start_year, start_quarter), FREQ = 4
  )
  database
}

#' Apply WRR and WR2R (world real short/2yr rates, Fisher-adjusted)
#'
#' Faithful port of modify_data.prg:214-215:
#'   `wrr  = ((1 + wrcr/100) / (1 + wp/wp(-4) - 1)) * 100 - 100`
#'   `wr2r = ((1 + wn2r/100) / (1 + wp/wp(-4) - 1)) * 100 - 100`
#' `WP`, `WRCR`, `WN2R` are all `direct` catalogue rows, so this can run
#' any time after the initial quarterly pivot. Idempotent per variable
#' (WRR and WR2R are populated independently, each only if its own input
#' -- WRCR or WN2R respectively -- is present).
#'
#' @inheritParams apply_ibctr
#' @keywords internal
apply_wrr_wr2r <- function(database, catalogue = series_catalogue()) {
  wp <- database[["WP"]]
  if (is.null(wp)) return(database)
  wp_lag4 <- bimets::TSLAG(wp, 4)

  if (is.null(database[["WRR"]]) && !is.null(database[["WRCR"]])) {
    wrcr <- database[["WRCR"]]
    database[["WRR"]] <- ((1 + wrcr / 100) / (1 + wp / wp_lag4 - 1)) * 100 - 100
  }
  if (is.null(database[["WR2R"]]) && !is.null(database[["WN2R"]])) {
    wn2r <- database[["WN2R"]]
    database[["WR2R"]] <- ((1 + wn2r / 100) / (1 + wp / wp_lag4 - 1)) * 100 - 100
  }
  database
}

#' Apply RMR (real mortgage rate)
#'
#' Faithful port of modify_data.prg:288:
#'   `rmr = ((1 + nmr/100) / (ptm/ptm(-4)) - 1) * 100`
#' `NMR` (direct) and `PTM` (level_from_pct, cumulated by
#' [apply_level_from_pct()]) are both quarterly primitives already in the
#' database by the time this runs. Idempotent.
#'
#' @inheritParams apply_ibctr
#' @keywords internal
apply_rmr <- function(database, catalogue = series_catalogue()) {
  if (!is.null(database[["RMR"]])) return(database)
  nmr <- database[["NMR"]]
  ptm <- database[["PTM"]]
  if (is.null(nmr) || is.null(ptm)) return(database)
  ptm_lag4 <- bimets::TSLAG(ptm, 4)
  database[["RMR"]] <- ((1 + nmr / 100) / (ptm / ptm_lag4) - 1) * 100
  database
}

#' Apply IAD (import-adjusted demand index)
#'
#' Faithful port of modify_data.prg:300:
#'   `iad = rc^iad_w_c * ib^iad_w_i * gi^iad_w_gi * gc^iad_w_gc * x^iad_w_x`
#' Needs the five IAD weight series ([apply_iad_weights()]) plus RC, IB,
#' GI, GC, X (all `direct` catalogue rows). Idempotent.
#'
#' @inheritParams apply_ibctr
#' @keywords internal
apply_iad <- function(database, catalogue = series_catalogue()) {
  if (!is.null(database[["IAD"]])) return(database)
  needed <- c("RC", "IB", "GI", "GC", "X",
             "IAD_W_C", "IAD_W_I", "IAD_W_GI", "IAD_W_GC", "IAD_W_X")
  have <- vapply(needed, function(v) !is.null(database[[v]]), logical(1))
  if (!all(have)) return(database)

  database[["IAD"]] <-
    database[["RC"]] ^ database[["IAD_W_C"]]  *
    database[["IB"]] ^ database[["IAD_W_I"]]  *
    database[["GI"]] ^ database[["IAD_W_GI"]] *
    database[["GC"]] ^ database[["IAD_W_GC"]] *
    database[["X"]]  ^ database[["IAD_W_X"]]
  database
}
