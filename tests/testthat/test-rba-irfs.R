# Tests for the RBA-convention IRF battery (rba_irf_specs() / run_rba_irf()).
# Short in-sample horizons (2010-2015 style), matching the existing IRF
# tests — one short solve per test where possible; real bimets solves.

rba_fixture <- function() {
  skip_if_not_installed("bimets")
  skip_if_not(file.exists(martin_data_fixture()), "MARTINDATA fixture missing")
  read_fixture()
}

# Pull one variable's value at a quarter out of a solve_martin() tibble.
val_at <- function(proj, variable, quarter) {
  r <- proj$value[proj$variable == variable & proj$quarter == quarter]
  if (length(r) == 0L) NA_real_ else r[1]
}

test_that("rba_irf_specs() covers the six RBA scenarios with the prg's shock geometry", {
  specs <- rba_irf_specs()
  expect_setequal(
    names(specs),
    c("monetary", "exchange_rate", "monetary_no_er", "monetary_no_erap",
      "monetary_no_erph", "housing_price")
  )

  # s1: +1ppt NCR, 4 quarters; RSTAR/GC/GI held 40q.
  expect_equal(specs$monetary$shock_equation, "NCR")
  expect_equal(specs$monetary$shock_type, "ppt")
  expect_equal(specs$monetary$shock_size, 1.0)
  expect_equal(specs$monetary$shock_length, 4L)
  expect_setequal(specs$monetary$hold_vars, c("RSTAR", "GC", "GI"))
  expect_equal(specs$monetary$hold_length, 40L)

  # s2: -10% RTWI, 4 quarters.
  expect_equal(specs$exchange_rate$shock_equation, "RTWI")
  expect_equal(specs$exchange_rate$shock_type, "pct")
  expect_equal(specs$exchange_rate$shock_size, -10)

  # s3: s1 + RTWI/NTWI held.
  expect_setequal(specs$monetary_no_er$hold_vars,
                  c("RSTAR", "GC", "GI", "RTWI", "NTWI"))

  # s4: s3 + PH held.
  expect_setequal(specs$monetary_no_erap$hold_vars,
                  c("RSTAR", "GC", "GI", "RTWI", "NTWI", "PH"))

  # s9: per the source `.prg`, shocks the identical variable set as s4
  # (a copy-paste duplicate under a different label — see the docstring).
  expect_setequal(specs$monetary_no_erph$hold_vars,
                  specs$monetary_no_erap$hold_vars)

  # s5: PH pinned via an explicit path, NCR + RTWI held (NOT RSTAR/GC/GI).
  expect_equal(specs$housing_price$shock_equation, "PH")
  expect_equal(specs$housing_price$shock_type, "pct_path")
  expect_setequal(specs$housing_price$hold_vars, c("NCR", "RTWI"))
  expect_equal(specs$housing_price$path, rep(-10, 10L))
})

test_that("monetary spec holds NCR at baseline+1 for 4 quarters then releases", {
  data <- rba_fixture()
  horizon     <- c("2008Q1", "2013Q4")
  shock_start <- "2010Q1"

  base <- suppressMessages(suppressWarnings(
    solve_martin(data, horizon = horizon, scenario = "baseline")
  ))
  scen <- suppressMessages(suppressWarnings(
    run_rba_irf(data, horizon, shock_start, rba_irf_specs()$monetary,
               baseline = base)
  ))
  expect_true(isTRUE(attr(scen, "convergence")$converged))

  held_qs <- quarter_seq(shock_start, "2010Q4")   # the 4 held quarters
  for (q in held_qs) {
    expect_equal(val_at(scen, "NCR", q) - val_at(base, "NCR", q), 1,
                tolerance = 1e-8, label = paste("NCR deviation at", q))
  }

  # Released at quarter 5 (2011Q1): no longer pinned to exactly +1ppt.
  released_dev <- val_at(scen, "NCR", "2011Q1") - val_at(base, "NCR", "2011Q1")
  expect_false(isTRUE(all.equal(released_dev, 1, tolerance = 1e-6)))

  # Tightening lowers activity with a lag.
  y_dev_pct <- 100 * (val_at(scen, "Y", "2011Q1") / val_at(base, "Y", "2011Q1") - 1)
  expect_lt(y_dev_pct, 0)
})

test_that("monetary_no_er holds RTWI exactly at baseline while Y still responds", {
  data <- rba_fixture()
  horizon     <- c("2008Q1", "2013Q4")
  shock_start <- "2010Q1"

  base <- suppressMessages(suppressWarnings(
    solve_martin(data, horizon = horizon, scenario = "baseline")
  ))
  scen <- suppressMessages(suppressWarnings(
    run_rba_irf(data, horizon, shock_start, rba_irf_specs()$monetary_no_er,
               baseline = base)
  ))
  expect_true(isTRUE(attr(scen, "convergence")$converged))

  # RTWI held EXACTLY at baseline over the whole (clipped) hold window —
  # exogenize pins the value, so this should be exact, not approximate.
  hold_qs <- quarter_seq(shock_start, horizon[2])
  for (q in hold_qs) {
    expect_equal(val_at(scen, "RTWI", q), val_at(base, "RTWI", q),
                tolerance = 1e-8, label = paste("RTWI at", q))
  }

  # Y still moves even with the exchange-rate channel switched off.
  y_dev_pct <- 100 * (val_at(scen, "Y", "2011Q1") / val_at(base, "Y", "2011Q1") - 1)
  expect_lt(y_dev_pct, 0)
})

test_that("housing_price spec pins PH to the supplied path, releases after", {
  data <- rba_fixture()
  horizon     <- c("2008Q1", "2014Q4")
  shock_start <- "2010Q1"

  base <- suppressMessages(suppressWarnings(
    solve_martin(data, horizon = horizon, scenario = "baseline")
  ))
  spec <- rba_irf_specs(housing_path = rep(-5, 3L))$housing_price
  scen <- suppressMessages(suppressWarnings(
    run_rba_irf(data, horizon, shock_start, spec, baseline = base)
  ))
  expect_true(isTRUE(attr(scen, "convergence")$converged))

  shocked_qs <- quarter_seq(shock_start, "2010Q3")   # 3 quarters
  for (q in shocked_qs) {
    pct_dev <- 100 * (val_at(scen, "PH", q) / val_at(base, "PH", q) - 1)
    expect_equal(pct_dev, -5, tolerance = 1e-6, label = paste("PH dev at", q))
  }

  # Released at the 4th quarter: no longer pinned to exactly -5%.
  released_pct <- 100 * (val_at(scen, "PH", "2010Q4") / val_at(base, "PH", "2010Q4") - 1)
  expect_false(isTRUE(all.equal(released_pct, -5, tolerance = 1e-6)))

  # NCR and RTWI held at baseline over their window (not RSTAR/GC/GI).
  expect_equal(val_at(scen, "NCR", "2010Q2"), val_at(base, "NCR", "2010Q2"),
              tolerance = 1e-8)
  expect_equal(val_at(scen, "RTWI", "2010Q2"), val_at(base, "RTWI", "2010Q2"),
              tolerance = 1e-8)
})
