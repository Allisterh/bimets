# 14 — SMP-style forecast summary table.
#
#   Rscript scripts/14_forecast_table.R
#
# Builds MARTIN's own analogue of the RBA Statement on Monetary Policy
# "Forecast Table" -- the layout the EViews reference driver builds in
# `fcst_table.prg`: year-ended (four-quarter-ended) percentage changes for
# the headline national-accounts aggregates plus the unemployment rate as a
# level, at the SMP's half-yearly Jun/Dec cadence. See R/forecast_table.R.
#
# Pipeline, same shape as scripts/03_forecast_unconditional.R:
#   1. nowcast the ragged edge and splice it back into the fixture database;
#   2. extend exogenous paths forward to the horizon end;
#   3. solve_martin() (frozen coefficients, no add-factors -- the baseline);
#   4. forecast_table() the resulting projection and write it to CSV.

root <- tryCatch(here::here(), error = function(e) getwd())
source(file.path(root, "setup.R"))

db <- read_fixture()                        # history to 2019Q3
horizon <- c("2015Q1", "2024Q4")             # replay from 2015Q1, forecast to 2024Q4

cat("Nowcasting the handover variables ...\n")
hc <- nowcast_handover(db, h = 2L, method = "arima")
db <- splice_handover(db, hc)

cat("Extending exogenous paths to ", horizon[2], " ...\n", sep = "")
db <- extend_exogenous(db, end_quarter = horizon[2])

cat("Solving baseline ", horizon[1], " -> ", horizon[2], " ...\n", sep = "")
base <- solve_martin(db, horizon = horizon, scenario = "baseline")
conv <- attr(base, "convergence")
cat(sprintf("Converged: %s (non-finite cells: %s)\n",
            conv$converged, conv$n_nonfinite))

cat("\nBuilding the forecast table ...\n")
tbl <- forecast_table(base)

out_path <- file.path(root, "results", "forecast_table.csv")
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write_forecast_table(tbl, out_path)
cat("Wrote ", out_path, "\n\n", sep = "")

print(tbl)
