# Tests for R/forecast_table.R (the SMP-style forecast summary table) and,
# per R2, the RAIN extension rule in R/extend_exogenous.R.

mk_ts <- function(values, start = c(2020, 1)) {
  bimets::TIMESERIES(values, START = start, FREQ = 4)
}

# Build a solve_martin()-shaped long tibble (variable, quarter, value,
# scenario) straight from a bimets/base TIMESERIES database, without
# running SIMULATE(). Mirrors simulation_to_tibble()'s quarter-string
# conversion so forecast_table() can be unit-tested against known fixture
# levels quickly.
db_to_projection <- function(db, scenario = "baseline") {
  rows <- lapply(names(db), function(var) {
    ts <- db[[var]]
    if (!inherits(ts, "ts")) return(NULL)
    t <- as.numeric(stats::time(ts))
    year    <- floor(t + 1e-9)
    quarter <- round((t - year) * 4 + 1)
    tibble::tibble(
      variable = var,
      quarter  = sprintf("%04dQ%d", year, quarter),
      value    = as.numeric(ts),
      scenario = scenario
    )
  })
  dplyr::bind_rows(rows)
}

test_that("forecast_table() year-ended % change matches hand computation (Y, fixture)", {
  skip_if_not(file.exists(martin_data_fixture()), "fixture missing")
  db <- read_fixture()
  proj <- db_to_projection(db)

  v <- as.numeric(db$Y)
  tsp <- stats::tsp(db$Y)
  yrs <- seq(tsp[1], by = 0.25, length.out = length(v))
  y_2019q3 <- v[which(abs(yrs - 2019.5) < 1e-6)]
  y_2018q3 <- v[which(abs(yrs - 2018.5) < 1e-6)]
  expected <- 100 * (y_2019q3 / y_2018q3 - 1)

  tbl <- forecast_table(proj, quarters = "2019Q3")
  got <- tbl[["2019Q3"]][tbl$variable == "Y"]
  expect_equal(got, expected, tolerance = 1e-8)
})

test_that("forecast_table() LUR row is a level, not a year-ended change", {
  skip_if_not(file.exists(martin_data_fixture()), "fixture missing")
  db <- read_fixture()
  proj <- db_to_projection(db)

  lur <- as.numeric(db$LUR)
  tsp <- stats::tsp(db$LUR)
  yrs <- seq(tsp[1], by = 0.25, length.out = length(lur))
  expected <- lur[which(abs(yrs - 2019.5) < 1e-6)]

  tbl <- forecast_table(proj, quarters = "2019Q3")
  got <- tbl[["2019Q3"]][tbl$variable == "LUR"]
  expect_equal(got, expected, tolerance = 1e-8)
  expect_equal(tbl$measure[tbl$variable == "LUR"], "level")
})

test_that("forecast_table() row list matches fcst_table.prg exactly", {
  db <- list(
    Y = mk_ts(rep(100, 8)), RC = mk_ts(rep(100, 8)), ID = mk_ts(rep(100, 8)),
    IB = mk_ts(rep(100, 8)), G = mk_ts(rep(100, 8)), GNE = mk_ts(rep(100, 8)),
    M = mk_ts(rep(100, 8)), X = mk_ts(rep(100, 8)), HDY = mk_ts(rep(100, 8)),
    TOT = mk_ts(rep(100, 8)), WY = mk_ts(rep(100, 8)), LUR = mk_ts(rep(5, 8)),
    LE = mk_ts(rep(100, 8)), PW = mk_ts(rep(100, 8)), PAE = mk_ts(rep(100, 8)),
    PTM = mk_ts(rep(100, 8)), P = mk_ts(rep(100, 8))
  )
  proj <- db_to_projection(db)
  tbl <- forecast_table(proj, quarters = "2021Q4")

  expected_vars <- c("Y", "RC", "ID", "IB", "G", "GNE", "M", "X", "HDY",
                      "TOT", "WY", "LUR", "LE", "PW", "PAE", "PTM", "P")
  expect_equal(nrow(tbl), 17L)
  expect_equal(tbl$variable, expected_vars)  # order matches the .prg loop
  expect_equal(
    tbl$row_label,
    c("Gross domestic product", "Household consumption", "Dwelling investment",
      "Business investment", "Public demand", "Gross national expenditure",
      "Imports", "Exports", "Real household disposable income",
      "Terms of trade", "USA GDP", "Unemployment rate (quarterly, %)",
      "Employment", "Wage price index", "Nominal average earnings per hour",
      "Trimmed mean inflation", "Consumer Price Index")
  )
  # Every row is a year-ended % change except LUR.
  expect_equal(tbl$measure[tbl$variable != "LUR"], rep("yoy_pct", 16L))
  expect_equal(tbl$measure[tbl$variable == "LUR"], "level")
})

test_that("forecast_table() computes a flat series as 0% year-ended change", {
  db <- list(Y = mk_ts(rep(100, 8)))
  proj <- db_to_projection(db)
  # Pad out the other required rows so forecast_table doesn't warn away the
  # whole test; only Y is checked below.
  for (v in c("RC", "ID", "IB", "G", "GNE", "M", "X", "HDY", "TOT", "WY",
              "LUR", "LE", "PW", "PAE", "PTM", "P")) {
    proj <- dplyr::bind_rows(proj, db_to_projection(list(Y = mk_ts(rep(100, 8)))) |>
      dplyr::mutate(variable = v))
  }
  tbl <- suppressWarnings(forecast_table(proj, quarters = "2021Q4"))
  expect_equal(tbl[["2021Q4"]][tbl$variable == "Y"], 0, tolerance = 1e-8)
})

test_that("forecast_table() defaults to the Jun/Dec cadence with a valid lag", {
  db <- list(Y = mk_ts(seq(100, 100 + 0.25 * 19, by = 0.25), start = c(2018, 1)))
  proj <- db_to_projection(db)
  tbl <- suppressWarnings(forecast_table(proj))
  q <- setdiff(names(tbl), c("row_label", "variable", "measure"))
  expect_true(length(q) > 0)
  quarters_only <- as.integer(substr(q, 6, 6))
  expect_true(all(quarters_only %in% c(2L, 4L)))
  # Every default quarter must have a year-earlier level available.
  years <- as.integer(substr(q, 1, 4))
  for (i in seq_along(q)) {
    lag_q <- sprintf("%04dQ%d", years[i] - 1L, quarters_only[i])
    expect_true(lag_q %in% proj$quarter)
  }
})

test_that("forecast_table() errors on a multi-scenario projection", {
  proj <- tibble::tibble(
    variable = c("Y", "Y"), quarter = c("2020Q1", "2020Q1"),
    value = c(100, 105), scenario = c("baseline", "shock")
  )
  expect_error(forecast_table(proj, quarters = "2020Q1"), "scenario")
})

test_that("forecast_table() warns and NAs out rows for missing variables", {
  proj <- tibble::tibble(
    variable = "Y", quarter = c("2019Q1", "2020Q1"),
    value = c(100, 110), scenario = "baseline"
  )
  expect_warning(tbl <- forecast_table(proj, quarters = "2020Q1"), "missing variable")
  expect_true(is.na(tbl[["2020Q1"]][tbl$variable == "RC"]))
})

test_that("forecast_table() prints without error", {
  db <- list(Y = mk_ts(rep(100, 8)))
  proj <- db_to_projection(db)
  for (v in c("RC", "ID", "IB", "G", "GNE", "M", "X", "HDY", "TOT", "WY",
              "LUR", "LE", "PW", "PAE", "PTM", "P")) {
    proj <- dplyr::bind_rows(proj, db_to_projection(list(Y = mk_ts(rep(100, 8)))) |>
      dplyr::mutate(variable = v))
  }
  tbl <- suppressWarnings(forecast_table(proj, quarters = "2021Q4"))
  expect_output(print(tbl), "Forecast Table")
  expect_s3_class(tbl, "martin_forecast_table")
})

test_that("write_forecast_table() round-trips through CSV", {
  db <- list(Y = mk_ts(rep(100, 8)))
  proj <- db_to_projection(db)
  for (v in c("RC", "ID", "IB", "G", "GNE", "M", "X", "HDY", "TOT", "WY",
              "LUR", "LE", "PW", "PAE", "PTM", "P")) {
    proj <- dplyr::bind_rows(proj, db_to_projection(list(Y = mk_ts(rep(100, 8)))) |>
      dplyr::mutate(variable = v))
  }
  tbl <- suppressWarnings(forecast_table(proj, quarters = c("2020Q4", "2021Q4")))

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  ret <- write_forecast_table(tbl, path)
  expect_identical(ret, path)
  expect_true(file.exists(path))

  back <- readr::read_csv(path, show_col_types = FALSE)
  expect_equal(back$row_label, tbl$row_label)
  expect_equal(back[["2020Q4"]], tbl[["2020Q4"]], tolerance = 1e-8)
  expect_equal(back[["2021Q4"]], tbl[["2021Q4"]], tolerance = 1e-8)
})

# --- RAIN extension rule (R2) -----------------------------------------------

test_that("extend_exogenous() holds RAIN at 0 past the fixture end", {
  skip_if_not(file.exists(martin_data_fixture()), "fixture missing")
  db <- read_fixture()
  out <- extend_exogenous(db, "2022Q4")

  v <- as.numeric(out$RAIN)
  tsp <- stats::tsp(out$RAIN)
  yrs <- seq(tsp[1], by = 0.25, length.out = length(v))
  extension <- v[yrs > 2019.5 + 1e-9]  # strictly past 2019Q3 (fixture end)
  expect_true(length(extension) > 0)
  expect_true(all(extension == 0))
})

test_that("extend_exogenous() RAIN override wins over carry_all's default carry", {
  # Last historical print is nonzero (7): plain "carry" would extend at 7;
  # the RAIN override must still force 0, not the carried last value.
  db <- list(RAIN = mk_ts(c(10, -5, 3, 7), start = c(2020, 1)))
  out <- extend_exogenous(db, "2021Q4", rules = "carry_all")
  v <- as.numeric(out$RAIN)
  expect_equal(tail(v, 4), rep(0, 4))
})
