# 12 — RBA-convention IRF battery (pinned-path shocks) -> CSV.
#
#   Rscript scripts/12_rba_irfs.R
#
# Reproduces the shock-delivery convention of the RBA's own MARTIN IRF
# programs (martin_rdp_irfs_public.prg + sub1_inputs/sub2_solve): the shocked
# variable is PINNED to baseline +/- delta over an explicit shock window (the
# EViews `{model}.exclude(m) VAR(start end)` idiom), then released back to its
# own equation; RSTAR/GC/GI (and, in the channel-decomposition variants,
# RTWI/NTWI/PH) are held at baseline for 40 quarters so each variant isolates
# one transmission channel. See R/irf_scenarios.R (rba_irf_specs(),
# run_rba_irf()) for the mechanics and how this differs from the add-factor
# standard_irfs() battery (scripts/08).
#
# Six scenarios:
#   monetary          — s1: +1ppt NCR, 4 quarters.
#   exchange_rate     — s2: -10% RTWI, 4 quarters.
#   monetary_no_er    — s3: s1 + RTWI/NTWI held at baseline, 40 quarters.
#   monetary_no_erap  — s4: s3 + PH held at baseline, 40 quarters.
#   monetary_no_erph  — s9: identical shock set to s4 in the source `.prg`
#                        (a copy-paste duplicate under a separate label —
#                        see the docstring in R/irf_scenarios.R).
#   housing_price     — s5: PH pinned to a -10% path for 10 quarters, NCR +
#                        RTWI held at baseline, 40 quarters.
#
# REPORTING UNITS: as scripts/08 — percent (%) deviation from baseline for
# every variable EXCEPT the rate variables (.irf_rate_vars()), which are
# reported as plain percentage-point (ppt) deviations.
#
# Outputs (committed): results/irfs/rba_irfs_long.csv (tidy, all scenarios)
# and one wide table per scenario, results/irfs/rba_<scenario>.csv.

root <- tryCatch(here::here(), error = function(e) getwd())
source(file.path(root, "setup.R"))

db <- read_fixture()

# In-sample structural window: the fixture ends 2019Q3, and the RBA
# convention's longest "hold" window is 40 quarters, so the horizon needs
# room for shock_start + 40 quarters. Using an in-sample window (real,
# historical exogenous paths) keeps this reproducible offline, matching
# scripts/05/08's convention.
horizon     <- c("2005Q1", "2019Q2")
shock_start <- "2009Q3"    # shock_start + 40q = 2019Q2 == horizon[2]

# FULL quarterly response path: every quarter from the shock to the horizon
# end. Override MARTIN_IRF_OFFSETS (comma-separated) to report a subset.
offsets <- {
  env <- Sys.getenv("MARTIN_IRF_OFFSETS", unset = "")
  if (nzchar(env)) {
    as.integer(strsplit(env, ",")[[1]])
  } else {
    0:(length(quarter_seq(shock_start, horizon[2])) - 1L)
  }
}

report_vars <- martin_model_variables("af", which = "endogenous")

cat("Baseline ", horizon[1], " -> ", horizon[2], " ...\n", sep = "")
base <- suppressMessages(suppressWarnings(
  solve_martin(db, horizon = horizon, scenario = "baseline")
))
cat(sprintf("  converged: %s\n", attr(base, "convergence")$converged))

specs <- rba_irf_specs()

cat(sprintf("Running the RBA IRF battery (%d scenarios) over %d variables ...\n",
            length(specs), length(report_vars)))
conv <- list()
rows <- list()
scenarios <- list()
for (key in names(specs)) {
  spec <- specs[[key]]
  cat(sprintf("  scenario: %s\n", spec$label))
  scen <- suppressMessages(suppressWarnings(
    run_rba_irf(db, horizon, shock_start, spec, baseline = base)
  ))
  scenarios[[key]] <- scen
  conv[[key]] <- attr(scen, "convergence")
  rows[[key]] <- irf_deviation_table(
    base, scen, report_vars, shock_start, offsets,
    scenario_key = key, scenario_label = spec$label
  )
}
irf <- dplyr::bind_rows(rows)

cat("\nConvergence:\n")
for (k in names(conv)) {
  cat(sprintf("  %-20s %s\n", k, conv[[k]]$converged))
}

# ---- write CSVs -----------------------------------------------------------
outdir <- file.path(root, "results", "irfs")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

long <- irf
long$unit <- ifelse(long$measure == "ppt", "ppt_deviation", "pct_deviation")
long <- long[, c("scenario", "scenario_label", "variable", "offset_q",
                 "quarter", "baseline", "scenario_value", "deviation", "unit")]
long_path <- file.path(outdir, "rba_irfs_long.csv")
utils::write.csv(long, long_path, row.names = FALSE)

offs <- sort(unique(irf$offset_q))
wide_for <- function(s) {
  vars <- intersect(report_vars, unique(s$variable))
  out  <- data.frame(variable = vars, stringsAsFactors = FALSE)
  out$unit <- vapply(vars, function(v) {
    m <- s$measure[s$variable == v]
    if (length(m) && m[1] == "ppt") "ppt_deviation" else "pct_deviation"
  }, character(1))
  for (o in offs) {
    out[[paste0("h", o)]] <- vapply(vars, function(v) {
      x <- s$deviation[s$variable == v & s$offset_q == o]
      if (length(x)) round(x[1], 5) else NA_real_
    }, numeric(1))
  }
  out
}
written <- character(0)
for (k in names(specs)) {
  p <- file.path(outdir, paste0("rba_", k, ".csv"))
  utils::write.csv(wide_for(irf[irf$scenario == k, , drop = FALSE]),
                   p, row.names = FALSE)
  written <- c(written, p)
}

# ---- channel decomposition: monetary vs no-ER vs no-ERAP vs no-ERPH -------
# Mirrors the prg's "Alternative channels" chart (irf_ncr_y / irf_ncr_y_noer /
# irf_ncr_y_noap / irf_ncr_y_noph): the Y deviation from the plain monetary
# shock, progressively switching off the exchange-rate / house-price channels.
channel_keys   <- c("monetary", "monetary_no_er", "monetary_no_erap",
                    "monetary_no_erph")
channel_labels <- c(monetary = "Baseline (monetary)",
                    monetary_no_er = "No exchange rate",
                    monetary_no_erap = "No ER or asset price",
                    monetary_no_erph = "No ER or house price")
cat("\n=== Alternative channels: Y deviation from baseline (%) ===\n")
cat(sprintf("%-22s", "scenario"))
for (o in offs) cat(sprintf(" %8s", paste0("h+", o)))
cat("\n")
for (k in channel_keys) {
  cat(sprintf("%-22s", channel_labels[[k]]))
  for (o in offs) {
    d <- irf$deviation[irf$scenario == k & irf$variable == "Y" & irf$offset_q == o]
    cat(sprintf(" %8s", if (length(d)) sprintf("%+7.3f%%", d) else "."))
  }
  cat("\n")
}

# ---- compact console preview (headline subset only) -----------------------
headline <- c("Y", "GNE", "LUR", "P", "PTM", "NCR", "RTWI", "PH", "TOT")
fmt_dev <- function(d, measure) {
  ifelse(is.na(d), "    .",
         sprintf("%+7.3f%s", d, ifelse(measure == "ppt", "p", "%")))
}
for (k in names(specs)) {
  s <- irf[irf$scenario == k & irf$variable %in% headline, , drop = FALSE]
  cat(sprintf("\n=== %s (headline subset) ===\n", s$scenario_label[1]))
  cat(sprintf("%-6s", "var"))
  for (o in offs) cat(sprintf(" %8s", paste0("h+", o)))
  cat("\n")
  for (v in intersect(headline, unique(s$variable))) {
    cat(sprintf("%-6s", v))
    for (o in offs) {
      cell <- s[s$variable == v & s$offset_q == o, , drop = FALSE]
      txt  <- if (nrow(cell)) fmt_dev(cell$deviation, cell$measure) else "    ."
      cat(sprintf(" %8s", txt))
    }
    cat("\n")
  }
}
cat("\nRead: 'p' = percentage-point deviation (rates: unemployment + interest",
    "rates/spreads);\n      '%' = percent deviation from baseline (all else).\n")

saveRDS(irf, file.path(root, "data", "rba_irfs.rds"))
cat(sprintf("\nWrote %d CSV files to %s:\n", length(written) + 1L, outdir))
cat("  rba_irfs_long.csv  (tidy, all scenarios x all variables)\n")
for (p in written) cat("  ", basename(p), "\n", sep = "")
