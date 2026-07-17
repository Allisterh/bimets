# 11 — Run MARTIN from RAW/PRIMITIVE source-data CSVs.
#
#   Rscript scripts/11_run_from_raw_csv.R
#
# scripts/09 shows the "already final shape" CSV path (read_csv_database()):
# one column per MARTIN variable, no transformation. This script exercises
# the RAW path (read_raw_csv_database()): a CSV of upstream ABS/RBA/FRED-style
# PRIMITIVE series -- direct imports, percent-change price indices, splice
# inputs -- that still needs the full derivation chain
# (derive_model_database(): Chow-Lin, splices, cumulation, the identity chain,
# derived-formula evaluation) applied, exactly as the live download pipeline
# (update_data() + to_martin_database()) would.
#
# Demo structure:
#   1. Export a raw-primitives template from the bundled fixture
#      (raw_csv_template() -- only PRIMITIVE columns; every DERIVED variable,
#      including the four new from-raw constructors this session added
#      -- NULCBS, WRR, WR2R, RMR, IAD, plus the pre-existing RULC/IBNDR --
#      is therefore absent from the template by construction).
#   2. Read it back via read_raw_csv_database(..., fallback = NULL) so
#      whatever comes back is GENUINELY re-derived, not silently
#      fallback-filled from the fixture.
#   3. For each derived variable, check it against the fixture's own value
#      and print PASS/FAIL -- or SKIP with a reason, for the handful whose
#      raw inputs the bundled fixture never carried in the first place (see
#      docs/running_from_csv.md, "Raw data CSV").
#   4. Re-read WITH the fixture as fallback (the realistic, recommended way
#      to call read_raw_csv_database()) and run a short forecast, to show
#      the resulting database is solvable end to end.

root <- tryCatch(here::here(), error = function(e) getwd())
source(file.path(root, "setup.R"))

fx <- read_fixture()

# ---- 1. Raw-primitives template from the fixture ---------------------------
csv_dir <- file.path(root, "data")
dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
csv_path <- file.path(csv_dir, "martin_raw_primitives.csv")
paths <- raw_csv_template(csv_path, fixture = fx)
cat("Wrote raw-primitives templates:\n")
cat("  quarterly:", paths[["quarterly"]], "\n")
cat("  annual:   ", paths[["annual"]],
   "(header-only -- the fixture carries no annual-frequency data)\n\n")

# ---- 2. Read back WITHOUT a fallback: isolate genuine re-derivation --------
cat("Reading back with fallback = NULL (no fixture backfill) ...\n")
db_no_fallback <- suppressMessages(
  read_raw_csv_database(paths[["quarterly"]], annual_path = paths[["annual"]],
                        fallback = NULL)
)

# ---- 3. PASS/FAIL/SKIP per derived variable ---------------------------------
max_common_diff <- function(a, b) {
  if (is.null(a) || is.null(b)) return(NA_real_)
  la <- ts_to_labelled(a); lb <- ts_to_labelled(b)
  va <- stats::setNames(la$values, la$qi); vb <- stats::setNames(lb$values, lb$qi)
  common <- intersect(names(va), names(vb))
  d <- va[common] - vb[common]
  if (!length(d) || !any(is.finite(d))) return(NA_real_)
  max(abs(d[is.finite(d)]))
}

# Variables whose raw inputs ARE fully present in the primitives-only
# template (full-history direct/level_from_pct/spliced series), so they
# should be genuinely re-derived to close tolerance.
derivable <- c(RMR = 1e-4, RULC = 1e-4, NULCBS = 1e-4, IAD = 1e-2)

# Variables whose raw inputs the bundled fixture never carried (world-economy
# series WRCR/WN2R, or annual ABS 5204.0 capital-stock/investment data) --
# a real deployment supplies these via the annual/quarterly CSVs directly
# (see docs/running_from_csv.md); here they're expected to be absent.
undeliverable <- c(
  "WRR"    = "needs WRCR (world cash rate) -- not a MARTIN model variable, absent from the fixture",
  "WR2R"   = "needs WN2R (world 2yr rate) -- not a MARTIN model variable, absent from the fixture",
  "IBNDR"  = "needs annual CFCTOT/CFCID/CFCOTC/CFCIBRE/KTOT/KID/KOTC/KIBRE -- annual template is header-only; falls to apply_ibndr()'s pre-existing static placeholder instead of NA",
  "IBREDR" = "needs annual KIBRE + IBRE -- annual template is header-only",
  "IDDR"   = "needs annual KID + IDA2 -- annual template is header-only",
  "KIBN"   = "needs annual KTOT/KID/KOTC/KIBRE -- annual template is header-only"
)

cat("\n--- Derived-variable check (against the fixture) ---\n")
all_pass <- TRUE
for (v in names(derivable)) {
  diff <- max_common_diff(db_no_fallback[[v]], fx[[v]])
  ok <- !is.na(diff) && diff < derivable[[v]]
  all_pass <- all_pass && ok
  cat(sprintf("  %-8s %-4s  max abs diff vs fixture: %s (tolerance %.0e)\n",
             v, if (ok) "PASS" else "FAIL",
             if (is.na(diff)) "NA (not derived)" else format(diff, digits = 3),
             derivable[[v]]))
}
for (v in names(undeliverable)) {
  cat(sprintf("  %-8s SKIP  %s\n", v, undeliverable[[v]]))
}
cat(if (all_pass) "\nAll derivable variables matched the fixture.\n"
   else "\nSome derivable variables did NOT match -- see FAIL rows above.\n")

# ---- 4. The realistic call: WITH fallback, then solve ----------------------
cat("\nReading back with fallback = read_fixture() (the recommended call) ...\n")
db <- suppressMessages(
  read_raw_csv_database(paths[["quarterly"]], annual_path = paths[["annual"]],
                        fallback = fx)
)
cat(sprintf("  %d/%d primitive series supplied by the raw CSVs; %d filled from fallback\n",
           length(attr(db, "primitives_supplied")),
           length(attr(db, "primitives_supplied")) +
             length(attr(db, "primitives_missing")),
           length(attr(db, "primitives_missing"))))

horizon <- c("2010Q1", "2019Q3")
cat("\nSolving baseline ", horizon[1], " -> ", horizon[2], " ...\n", sep = "")
base <- solve_martin(db, horizon = horizon, scenario = "baseline_from_raw_csv")
conv <- attr(base, "convergence")
cat(sprintf("Converged: %s (non-finite cells: %s)\n",
           conv$converged, conv$n_nonfinite))

g <- function(p, v, q) {
  x <- p$value[p$variable == v & p$quarter == q]
  if (length(x)) x else NA_real_
}
cat("\nHeadline (from raw-CSV-derived data):\n")
cat(sprintf("%-8s %10s %8s %8s %8s\n", "quarter", "Y", "LUR", "P", "NCR"))
for (q in c("2015Q4", "2017Q4", "2019Q3")) {
  cat(sprintf("%-8s %10.1f %8.2f %8.2f %8.2f\n",
             q, g(base, "Y", q), g(base, "LUR", q), g(base, "P", q), g(base, "NCR", q)))
}

cat("\nSee docs/running_from_csv.md, \"Raw data CSV\", for the full CSV formats",
   "\nand the PEX / world-rate / annual-capital-stock limitations.\n")
