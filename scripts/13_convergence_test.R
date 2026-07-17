# 13 — Long-run convergence test.
#
#   Rscript scripts/13_convergence_test.R
#   MARTIN_CONVERGE_END=2085Q4 Rscript scripts/13_convergence_test.R
#
# Ports the intent of the EViews model_converge.prg: solve MARTIN a long way
# past the data edge on carried-forward exogenous paths and check that the
# model's own long-run identities hold — the point being that MARTIN's
# growth/inflation/unemployment blocks are built to close toward their trend
# anchors (TY, PI_TARGET, TLUR) given enough time and no further shocks.
#
# Method:
#   1. Extend every exogenous series flat (carry-forward) out to
#      MARTIN_CONVERGE_END (env override; default 2060Q4 — the dummy
#      machinery (apply_dummies()/database_span()) already covers out to
#      2050Q4 automatically, but a horizon past that needs the database's own
#      span extended first so database_span() picks up the longer end; this
#      script does that via extend_exogenous() before ever touching the
#      model, so no change to R/dummies.R is needed).
#   2. Solve UNCONDITIONALLY (no add-factors, no imposed paths) from the data
#      edge (the fixture's last actual quarter, 2019Q3, so the horizon starts
#      2019Q4) out to MARTIN_CONVERGE_END. The replay add-factors decay to
#      (numerically) zero well before the terminal decade under the
#      `_a(-1)*-0.5` rule solve_martin() already applies, so this is a clean
#      test of the model's own long-run pull, not a lingering historical
#      shock.
#   3. Over the TERMINAL DECADE (the last 40 quarters of the horizon), test:
#        (a) TSDELTALOG(Y) (quarterly log-diff growth) converges to TY
#            (potential growth, same log-diff units);
#        (b) year-ended PTM inflation (PTM_t / PTM_{t-4} * 100 - 100)
#            converges to PI_TARGET (2.5, the RBA's inflation-target scalar);
#        (c) LUR converges to TLUR (the NAIRU trend).
#      "Converges" = the terminal-decade mean absolute gap is within a
#      tolerance (below). PASS/FAIL is printed per criterion; because a
#      41-year horizon is not necessarily long enough for a slow-moving gap
#      to fully close, we ALSO report whether the gap is shrinking across the
#      terminal decade (a criterion can legitimately FAIL the tolerance while
#      still visibly converging — that's a genuine model-dynamics finding,
#      not a bug, and is exactly what model_converge.prg's charts are for).
#
# Output (committed): results/convergence_test.csv — the terminal-decade
# paths for all three criteria.

root <- tryCatch(here::here(), error = function(e) getwd())
source(file.path(root, "setup.R"))

converge_end <- Sys.getenv("MARTIN_CONVERGE_END", unset = "2060Q4")

# Tolerances for the PASS/FAIL calls below (terminal-decade MEAN ABSOLUTE
# gap). These are deliberately tight RBA-style long-run targets, not
# forecast-accuracy bands — see the section header above for what a FAIL
# here does (and doesn't) mean.
TOL_GROWTH_PPT <- 0.05   # quarterly TSDELTALOG(Y) vs TY_POT, percentage points
TOL_INFL_PPT   <- 0.50   # year-ended PTM inflation vs PI_TARGET, ppt
TOL_LUR_PPT    <- 0.30   # LUR vs TLUR, ppt

cat("Convergence test: extending exogenous series to", converge_end, "...\n")
db <- read_fixture()
db <- extend_exogenous(db, end_quarter = converge_end, rules = "carry_all")

# "Solve from the data edge": the fixture's real data ends 2019Q3 (see
# CLAUDE.md), so the horizon starts the first forecast quarter, 2019Q4 —
# an unconditional projection with no in-sample replay portion.
horizon <- c("2019Q4", converge_end)
cat("Solving unconditionally ", horizon[1], " -> ", horizon[2], " ...\n", sep = "")
base <- solve_martin(db, horizon = horizon, scenario = "baseline")
cat(sprintf("  converged: %s (non-finite cells: %d)\n",
            attr(base, "convergence")$converged,
            attr(base, "convergence")$n_nonfinite))

# ---- combine each series' actual history with its solved path -------------
# Needed because the terminal-decade growth/inflation calculations look back
# 1-4 quarters, and the early forecast quarters' lags fall in actual history
# (before horizon[1]) rather than in `base`.
ts_to_named <- function(ts) {
  t <- as.numeric(stats::time(ts))
  year <- floor(t + 1e-9)
  q    <- round((t - year) * 4 + 1)
  stats::setNames(as.numeric(ts), sprintf("%04dQ%d", year, q))
}
combined_series <- function(var) {
  hist <- ts_to_named(db[[var]])
  sim  <- stats::setNames(base$value[base$variable == var],
                          base$quarter[base$variable == var])
  out <- c(hist, sim)
  out[!duplicated(names(out), fromLast = TRUE)]   # solved value wins on overlap
}
quarter_shift <- function(q, n) {
  p   <- parse_quarter(q)
  idx <- quarter_index(p$year, p$quarter) + as.integer(n)
  format_quarter(idx %/% 4L, idx %% 4L + 1L)
}

Y_all   <- combined_series("Y")
# Potential-growth anchor: TY_POT = TDLLA + TDLLPOP + TDLLHPP, the identity
# model_converge.prg plots DLOG(Y) against (its TY_POT_2). TY (the behavioural
# smoothed trend) converges to the same steady state but more slowly, so
# anchoring on it would understate convergence.
TY_all  <- combined_series("TY_POT")
PTM_all <- combined_series("PTM")
LUR_all  <- stats::setNames(base$value[base$variable == "LUR"],
                            base$quarter[base$variable == "LUR"])
TLUR_all <- stats::setNames(base$value[base$variable == "TLUR"],
                            base$quarter[base$variable == "TLUR"])
pi_target <- unname(db[["PI_TARGET"]][1])   # a constant scalar series

# ---- terminal decade: the last 40 quarters of the horizon ------------------
term_qs <- tail(quarter_seq(horizon[1], horizon[2]), 40L)

row_for <- function(q) {
  qm1 <- quarter_shift(q, -1L)
  qm4 <- quarter_shift(q, -4L)
  y_growth <- if (q %in% names(Y_all) && qm1 %in% names(Y_all) && !is.na(Y_all[[q]])) {
    log(Y_all[[q]]) - log(Y_all[[qm1]])
  } else NA_real_
  ty <- TY_all[[q]]
  pi_yoy <- if (q %in% names(PTM_all) && qm4 %in% names(PTM_all)) {
    100 * (PTM_all[[q]] / PTM_all[[qm4]] - 1)
  } else NA_real_
  lur  <- LUR_all[[q]]
  tlur <- TLUR_all[[q]]
  tibble::tibble(
    quarter        = q,
    y_growth_qtr   = 100 * y_growth,     # percentage points, quarterly
    ty_potential   = 100 * ty,
    growth_gap_ppt = 100 * (y_growth - ty),
    ptm_yoy_infl   = pi_yoy,
    pi_target      = pi_target,
    inflation_gap_ppt = pi_yoy - pi_target,
    lur            = lur,
    tlur           = tlur,
    lur_gap_ppt    = lur - tlur
  )
}
term <- dplyr::bind_rows(lapply(term_qs, row_for))

# ---- PASS/FAIL --------------------------------------------------------------
mean_abs <- function(x) mean(abs(x), na.rm = TRUE)
# Shrinking-gap trend: mean |gap| in the decade's second half vs its first.
half <- floor(nrow(term) / 2)
trend <- function(col) {
  first_half  <- mean_abs(term[[col]][seq_len(half)])
  second_half <- mean_abs(term[[col]][(half + 1L):nrow(term)])
  list(first = first_half, second = second_half,
       shrinking = second_half < first_half - 1e-9)
}

growth_gap <- mean_abs(term$growth_gap_ppt)
infl_gap   <- mean_abs(term$inflation_gap_ppt)
lur_gap    <- mean_abs(term$lur_gap_ppt)
growth_trend <- trend("growth_gap_ppt")
infl_trend   <- trend("inflation_gap_ppt")
lur_trend    <- trend("lur_gap_ppt")

pass_fail <- function(gap, tol) if (gap <= tol) "PASS" else "FAIL"
report_line <- function(label, gap, tol, tr) {
  cat(sprintf(
    "  [%s] %-42s terminal-decade mean |gap| = %6.3f ppt (tol %.2f) — %s half %.3f -> %s half %.3f\n",
    pass_fail(gap, tol), label, gap, tol,
    "1st", tr$first, "2nd", tr$second
  ))
}

cat(sprintf("\nTerminal decade: %s .. %s (%d quarters)\n\n",
            term_qs[1], term_qs[length(term_qs)], length(term_qs)))
cat("Criteria (terminal-decade mean absolute gap vs tolerance):\n")
report_line("TSDELTALOG(Y) vs TY_POT (potential growth)", growth_gap, TOL_GROWTH_PPT, growth_trend)
report_line("Year-ended PTM inflation vs PI_TARGET",  infl_gap,   TOL_INFL_PPT,   infl_trend)
report_line("LUR vs TLUR (NAIRU)",                     lur_gap,    TOL_LUR_PPT,    lur_trend)

for (tr in list(growth_trend, infl_trend, lur_trend)) {
  if (!tr$shrinking) {
    cat("  note: a FAILing criterion whose gap is NOT shrinking across the",
        "terminal decade may need a longer horizon (MARTIN_CONVERGE_END) or",
        "signals a genuine non-convergent long-run property, not just slow",
        "convergence.\n")
    break
  }
}

# ---- write CSV --------------------------------------------------------------
outdir <- file.path(root, "results")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(outdir, "convergence_test.csv")
utils::write.csv(term, out_path, row.names = FALSE)
cat("\nWrote", out_path, "\n")
