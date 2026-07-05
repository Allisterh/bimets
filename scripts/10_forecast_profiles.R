# 10 — Impose forecast profiles (conditional forecasting).
#
#   Rscript scripts/10_forecast_profiles.R
#
# The everyday RBA forecasting move: pin certain variables to judgemental paths
# and let the model solve everything else around them. `solve_martin(profiles=)`
# does this — each variable is held (Exogenized) at the supplied values over the
# span of ITS OWN quarters, so different variables can be imposed over different
# windows in a single, coherent solve:
#
#   * GLOBAL variables — impose the ENTIRE forecast path. Here: world GDP (WY)
#     +2% and world commodity prices (WPCOM) +10%, held every forecast quarter.
#   * CPI / UNEMPLOYMENT — impose only a 1-QUARTER-AHEAD nowcast, then let the
#     model run free. Here: trimmed-mean CPI (PTM) and the unemployment rate
#     (LUR) for the first forecast quarter only.
#
# `profiles` is a tidy data.frame (variable, quarter, value) in each variable's
# NATIVE units (the level the database stores). The imposed values propagate
# through the rest of the model (exports, activity, the Taylor-rule cash rate,
# the exchange rate, ...). Values are absolute, so they are usually built
# relative to a baseline solve (as below).
#
# This demo runs on the bundled fixture (history to 2019Q3) so it is
# reproducible offline; on live data to 2026Q1 you would set the horizon to,
# say, 2028Q2 and impose the global path over 2026Q2-2028Q2.

root <- tryCatch(here::here(), error = function(e) getwd())
source(file.path(root, "setup.R"))

db <- read_fixture()
db <- extend_exogenous(db, end_quarter = "2022Q4")   # exogenous paths to horizon end
horizon  <- c("2015Q1", "2022Q4")
fc_start <- "2019Q4"                                  # first forecast quarter
fc_qs    <- quarter_seq(fc_start, horizon[2])

g <- function(p, v, q) { x <- p$value[p$variable == v & p$quarter == q]; if (length(x)) x else NA_real_ }

cat("Baseline ...\n")
base <- solve_martin(db, horizon = horizon, scenario = "baseline")

# ---- build the imposed profiles -------------------------------------------
# Pattern 1: full forecast path for the global variables (relative to baseline).
globals <- rbind(
  data.frame(variable = "WY",
             quarter  = fc_qs,
             value    = vapply(fc_qs, function(q) g(base, "WY", q), 0) * 1.02),
  data.frame(variable = "WPCOM",
             quarter  = fc_qs,
             value    = vapply(fc_qs, function(q) g(base, "WPCOM", q), 0) * 1.10)
)
# Pattern 2: 1-quarter-ahead nowcast for CPI and unemployment, first fc quarter.
nowcasts <- rbind(
  data.frame(variable = "PTM", quarter = fc_start,
             value = g(base, "PTM", fc_start) * 1.004),   # ~ +0.4% on the price level
  data.frame(variable = "LUR", quarter = fc_start,
             value = g(base, "LUR", fc_start) + 0.3)      # +0.3 ppt on the rate
)
profiles <- rbind(globals, nowcasts)

cat("Conditional solve with imposed profiles ...\n")
cond <- solve_martin(db, horizon = horizon, profiles = profiles,
                     scenario = "conditioned")
cat(sprintf("  converged: %s\n", attr(cond, "convergence")$converged))

# ---- report: conditioned vs baseline over the forecast --------------------
# Rate variables (unemployment, cash rate) reported as ppt deviations; the rest
# as percent deviations from baseline.
rate_vars    <- c("LUR", "NCR")
report_vars  <- c("WY", "WPCOM", "TOT", "RTWI", "Y", "GNE",
                  "PTM", "P", "LUR", "NCR", "X", "M")
rows <- list()
for (v in report_vars) for (q in fc_qs) {
  b <- g(base, v, q); s <- g(cond, v, q)
  if (!is.finite(b) || !is.finite(s)) next
  is_rate <- v %in% rate_vars
  rows[[length(rows) + 1L]] <- data.frame(
    variable = v, quarter = q, baseline = b, conditioned = s,
    deviation = if (is_rate) s - b else 100 * (s / b - 1),
    unit = if (is_rate) "ppt_deviation" else "pct_deviation",
    stringsAsFactors = FALSE
  )
}
dev <- do.call(rbind, rows)

outdir <- file.path(root, "results", "profiles")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(profiles, file.path(outdir, "profiles_input.csv"), row.names = FALSE)
utils::write.csv(dev, file.path(outdir, "conditional_vs_baseline.csv"), row.names = FALSE)

# ---- console summary ------------------------------------------------------
cat("\nDeviation from baseline (imposed vars in bold-ish; ppt for LUR/NCR):\n")
cat(sprintf("%-6s %8s %8s %8s %8s\n", "var", fc_start, "2020Q4", "2021Q4", "2022Q4"))
show_q <- c(fc_start, "2020Q4", "2021Q4", "2022Q4")
for (v in report_vars) {
  cat(sprintf("%-6s", v))
  for (q in show_q) {
    r <- dev[dev$variable == v & dev$quarter == q, , drop = FALSE]
    if (nrow(r)) {
      cat(sprintf(" %+7.3f%s", r$deviation, if (r$unit == "ppt_deviation") "p" else "%"))
    } else cat(sprintf(" %8s", "."))
  }
  cat("\n")
}
cat("\nWY & WPCOM are held on their imposed path every forecast quarter;",
    "\nPTM & LUR are pinned only in", fc_start, "then solve freely (watch them",
    "\nmove away from the imposed value from 2020Q1).\n")
cat("\nWrote results/profiles/profiles_input.csv and",
    "conditional_vs_baseline.csv\n")
