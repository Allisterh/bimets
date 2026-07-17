# Tests for the raw/primitive-source-data CSV path:
#   R/raw_csv.R           -- read_raw_csv_database() / raw_csv_template()
#   R/update_data.R        -- derive_model_database() (the to_martin_database()
#                              handler-chain extraction)
#   R/identities.R         -- the new from-raw constructors (apply_kibn(),
#                              apply_ibredr_annual(), apply_iddr_annual(),
#                              apply_nulcbs(), apply_wrr_wr2r(), apply_rmr(),
#                              apply_iad())

# Compare two bimets quarterly series on the quarters they share (same helper
# as test-csv-data.R).
max_common_diff <- function(a, b) {
  la <- ts_to_labelled(a)
  lb <- ts_to_labelled(b)
  va <- stats::setNames(la$values, la$qi)
  vb <- stats::setNames(lb$values, lb$qi)
  common <- intersect(names(va), names(vb))
  d <- va[common] - vb[common]
  max(abs(d[is.finite(d)]))
}

csv_fixture_db <- function() {
  skip_if_not_installed("bimets")
  skip_if_not(file.exists(martin_data_fixture()), "MARTINDATA fixture missing")
  read_fixture()
}

# ---- (a) derive_model_database() refactor safety --------------------------

test_that("derive_model_database() is what to_martin_database() now calls", {
  # Same synthetic panel as test-to-martin-database.R's first case.
  panel <- tibble::tibble(
    series_id = "GDPC1",
    source    = "fred",
    date      = seq(as.Date("2020-01-01"), as.Date("2020-10-01"), by = "quarter"),
    value     = c(100, 101, 102, 103),
    vintage   = as.Date("2026-05-23")
  )
  out <- to_martin_database(panel)

  # Hand-reconstruct the pivot half of to_martin_database() and pass it
  # through derive_model_database() directly -- the two paths must agree,
  # since to_martin_database() is now exactly pivot + derive_model_database().
  cat <- series_catalogue()
  joined <- dplyr::left_join(
    panel,
    cat[, c("source_id", "source", "martin_var", "source_frequency",
           "aggregation", "transformation")],
    by = c("series_id" = "source_id", "source")
  )
  joined <- joined[!is.na(joined$martin_var), , drop = FALSE]
  q_rows <- joined[joined$transformation %in%
                     c("direct", "level_from_pct", "spliced"), , drop = FALSE]
  db0 <- pivot_quarterly(q_rows)
  a_rows <- joined[joined$transformation == "chowlin", , drop = FALSE]
  annual_db0 <- pivot_annual(a_rows)

  out2 <- derive_model_database(db0, annual_db0, cat)

  expect_setequal(names(out), names(out2))
  for (v in names(out)) {
    expect_equal(as.numeric(out[[v]]), as.numeric(out2[[v]]),
                info = paste("variable:", v))
  }
})

test_that("derive_model_database() with no annual/world/capital inputs adds only the pre-existing calendar rows (new constructors no-op)", {
  # Mirrors test-to-martin-database.R's "ignores series not in the catalogue"
  # case: an empty database has none of WP/WRCR/WN2R/NMR/RC/IB/GI/GC/X/NULC,
  # nor any annual_db, so every new constructor (apply_kibn,
  # apply_ibredr_annual, apply_iddr_annual, apply_wrr_wr2r, apply_rmr,
  # apply_nulcbs, apply_iad) must no-op -- proving the refactor didn't change
  # the download-path's "nothing supplied" behaviour.
  out <- derive_model_database(list(), list(), series_catalogue())
  for (v in c("KIBN", "IBREDR", "IDDR", "WRR", "WR2R", "RMR", "IAD", "NULCBS")) {
    expect_false(v %in% names(out), info = v)
  }
})

# ---- (b) new constructors: pure algebra, tight tolerance -------------------

test_that("apply_nulcbs() matches the fixture's own NULCBS to ~1e-6", {
  fx <- csv_fixture_db()
  out <- apply_nulcbs(list(NULC = fx[["NULC"]]))
  expect_true("NULCBS" %in% names(out))
  expect_lt(max_common_diff(out[["NULCBS"]], fx[["NULCBS"]]), 1e-5)
})

test_that("apply_nulcbs() is idempotent and no-ops without NULC", {
  fx <- csv_fixture_db()
  already <- list(NULC = fx[["NULC"]], NULCBS = fx[["NULCBS"]])
  out <- apply_nulcbs(already)
  expect_identical(out[["NULCBS"]], already[["NULCBS"]])
  expect_identical(apply_nulcbs(list()), list())
})

test_that("apply_wrr_wr2r() reproduces the Fisher-adjusted formula exactly", {
  n <- 20
  wp   <- bimets::TIMESERIES(cumprod(c(100, rep(1.004, n - 1))),
                             START = c(2000, 1), FREQ = 4)
  wrcr <- bimets::TIMESERIES(rep(3.25, n), START = c(2000, 1), FREQ = 4)
  wn2r <- bimets::TIMESERIES(rep(2.75, n), START = c(2000, 1), FREQ = 4)
  out <- apply_wrr_wr2r(list(WP = wp, WRCR = wrcr, WN2R = wn2r))

  wpv <- as.numeric(wp)
  wp_lag4 <- c(rep(NA_real_, 4), wpv[seq_len(n - 4)])
  manual_wrr  <- ((1 + as.numeric(wrcr) / 100) / (1 + wpv / wp_lag4 - 1)) * 100 - 100
  manual_wr2r <- ((1 + as.numeric(wn2r) / 100) / (1 + wpv / wp_lag4 - 1)) * 100 - 100

  expect_equal(as.numeric(out[["WRR"]]),  manual_wrr[5:n],  tolerance = 1e-9)
  expect_equal(as.numeric(out[["WR2R"]]), manual_wr2r[5:n], tolerance = 1e-9)
})

test_that("apply_wrr_wr2r() no-ops without WP, and per-variable without WRCR/WN2R", {
  expect_identical(apply_wrr_wr2r(list(WRCR = bimets::TIMESERIES(1, START = c(2000, 1), FREQ = 4))),
                   list(WRCR = bimets::TIMESERIES(1, START = c(2000, 1), FREQ = 4)))
  wp <- bimets::TIMESERIES(rep(100, 8), START = c(2000, 1), FREQ = 4)
  out <- apply_wrr_wr2r(list(WP = wp))
  expect_false("WRR" %in% names(out))
  expect_false("WR2R" %in% names(out))
})

test_that("apply_rmr() reproduces the real-mortgage-rate formula exactly", {
  n <- 12
  ptm <- bimets::TIMESERIES(cumprod(c(100, rep(1.005, n - 1))),
                            START = c(2000, 1), FREQ = 4)
  nmr <- bimets::TIMESERIES(rep(6.0, n), START = c(2000, 1), FREQ = 4)
  out <- apply_rmr(list(PTM = ptm, NMR = nmr))

  ptmv <- as.numeric(ptm)
  ptm_lag4 <- c(rep(NA_real_, 4), ptmv[seq_len(n - 4)])
  manual <- ((1 + as.numeric(nmr) / 100) / (ptmv / ptm_lag4) - 1) * 100
  expect_equal(as.numeric(out[["RMR"]]), manual[5:n], tolerance = 1e-9)
})

test_that("apply_iad() reproduces the geometric-weighted-average formula exactly", {
  mk <- function(v) bimets::TIMESERIES(rep(v, 6), START = c(2000, 1), FREQ = 4)
  db <- list(RC = mk(100), IB = mk(50), GI = mk(30), GC = mk(80), X = mk(60),
            IAD_W_C = mk(0.3), IAD_W_I = mk(0.2), IAD_W_GI = mk(0.1),
            IAD_W_GC = mk(0.25), IAD_W_X = mk(0.15))
  out <- apply_iad(db)
  expected <- 100^0.3 * 50^0.2 * 30^0.1 * 80^0.25 * 60^0.15
  expect_equal(as.numeric(out[["IAD"]]), rep(expected, 6), tolerance = 1e-9)
})

test_that("apply_iad() no-ops when any of the five weights or bases is missing", {
  mk <- function(v) bimets::TIMESERIES(rep(v, 4), START = c(2000, 1), FREQ = 4)
  incomplete <- list(RC = mk(100), IB = mk(50), IAD_W_C = mk(0.3))
  expect_identical(apply_iad(incomplete), incomplete)
})

# ---- (b) new constructors: interpolated annual stocks, looser tolerance ---

test_that("apply_kibn() recovers KTOT - KID - KOTC - KIBRE at the Q2 anchors", {
  years <- 1990:2010
  kibre <- 1000 + seq_along(years) * 10
  annual_db <- list(
    KTOT  = stats::ts(kibre * 4,   start = 1990, frequency = 1),
    KID   = stats::ts(kibre * 1,   start = 1990, frequency = 1),
    KOTC  = stats::ts(kibre * 0.5, start = 1990, frequency = 1),
    KIBRE = stats::ts(kibre,       start = 1990, frequency = 1)
  )
  out <- apply_kibn(list(), annual_db)
  expect_true("KIBN" %in% names(out))

  span <- database_span(list())
  idx_2000q2 <- (2000 - span$start_year) * 4L + (2L - span$start_quarter) + 1L
  expected_2000 <- (kibre * 1.5)[years == 2000]
  expect_equal(as.numeric(out[["KIBN"]])[idx_2000q2], expected_2000,
              tolerance = 1e-9)
})

test_that("apply_kibn() is idempotent and no-ops with any annual input missing", {
  out <- apply_kibn(list(KIBN = bimets::TIMESERIES(1, START = c(2000, 1), FREQ = 4)),
                    list(KTOT = stats::ts(1, start = 2000, frequency = 1)))
  expect_equal(as.numeric(out[["KIBN"]]), 1)
  expect_identical(apply_kibn(list(), list(KTOT = stats::ts(1, start = 2000, frequency = 1))),
                   list())
})

test_that("apply_ibredr_annual() recovers a known constant depreciation rate", {
  # Synthetic annual KIBRE/IBRE satisfying the capital-accumulation identity
  # exactly at a known rate, so the recovered quarterly compounding rate
  # should match ((1+r/100)^(1/4)-1)*100 away from the interpolation/
  # backcast edges.
  years <- 1990:2015
  true_rate <- 5.0
  kibre <- numeric(length(years)); kibre[1] <- 1000
  ibre  <- numeric(length(years)); ibre[1]  <- NA
  set.seed(1)
  for (i in 2:length(years)) {
    ibre[i]  <- 50 + stats::rnorm(1, 0, 5)
    kibre[i] <- kibre[i - 1] * (1 - true_rate / 100) + ibre[i]
  }
  annual_db <- list(
    KIBRE = stats::ts(kibre, start = 1990, frequency = 1),
    IBRE  = stats::ts(ibre,  start = 1990, frequency = 1)
  )
  out <- apply_ibredr_annual(list(), annual_db)
  expect_true("IBREDR" %in% names(out))

  span <- database_span(list())
  idx_1995q2 <- (1995 - span$start_year) * 4L + (2L - span$start_quarter) + 1L
  idx_2005q2 <- (2005 - span$start_year) * 4L + (2L - span$start_quarter) + 1L
  expected_q <- ((1 + true_rate / 100)^(1 / 4) - 1) * 100
  vals <- as.numeric(out[["IBREDR"]])[idx_1995q2:idx_2005q2]
  expect_equal(vals, rep(expected_q, length(vals)), tolerance = 1e-6)
})

test_that("apply_iddr_annual() uses IDA2 (not the quarterly ID) and no-ops without it", {
  years <- 1990:2010
  true_rate <- 3.0
  kid <- numeric(length(years)); kid[1] <- 500
  ida2 <- numeric(length(years)); ida2[1] <- NA
  for (i in 2:length(years)) {
    ida2[i] <- 20
    kid[i]  <- kid[i - 1] * (1 - true_rate / 100) + ida2[i]
  }
  annual_db_no_ida2 <- list(KID = stats::ts(kid, start = 1990, frequency = 1))
  expect_identical(apply_iddr_annual(list(), annual_db_no_ida2), list())

  annual_db <- list(
    KID  = stats::ts(kid,  start = 1990, frequency = 1),
    IDA2 = stats::ts(ida2, start = 1990, frequency = 1)
  )
  out <- apply_iddr_annual(list(), annual_db)
  span <- database_span(list())
  idx_2000q2 <- (2000 - span$start_year) * 4L + (2L - span$start_quarter) + 1L
  expected_q <- ((1 + true_rate / 100)^(1 / 4) - 1) * 100
  expect_equal(as.numeric(out[["IDDR"]])[idx_2000q2], expected_q, tolerance = 1e-6)
})

# ---- (c) read_raw_csv_database with a partial primitive set + fallback ----

test_that("read_raw_csv_database() with a subset of primitives + fallback yields a solvable database", {
  fx <- csv_fixture_db()
  tmp <- tempfile(fileext = ".csv")
  # Only a handful of quarterly primitives -- everything else must come from
  # the fallback fixture.
  database_to_csv(fx[c("RC", "NCR", "WPCOM")], tmp)

  db <- suppressMessages(read_raw_csv_database(tmp, fallback = fx))
  expect_true(all(c("RC", "NCR", "WPCOM", "Y", "LUR") %in% names(db)))
  expect_lt(max_common_diff(db[["RC"]], fx[["RC"]]), 1e-6)
  # Untouched variables fall all the way back to the fixture.
  expect_lt(max_common_diff(db[["Y"]], fx[["Y"]]), 1e-6)

  attrs_supplied <- attr(db, "primitives_supplied")
  expect_true("RC" %in% attrs_supplied)
  expect_true(length(attr(db, "primitives_missing")) > 0L)

  m <- load_martin(db, variant = "af")
  base <- solve_martin(db, horizon = c("2010Q1", "2018Q4"))
  conv <- attr(base, "convergence")
  expect_true(isTRUE(conv$converged))
})

test_that("read_raw_csv_database() errors on a missing file", {
  expect_error(read_raw_csv_database(tempfile(fileext = ".csv")), "not found")
})

# ---- (d) raw_csv_template() round-trip -------------------------------------

test_that("raw_csv_template() -> read_raw_csv_database() reproduces derived fixture values", {
  fx  <- csv_fixture_db()
  tmp <- tempfile(fileext = ".csv")
  paths <- raw_csv_template(tmp, fixture = fx)
  expect_true(file.exists(paths[["quarterly"]]))
  expect_true(file.exists(paths[["annual"]]))

  db <- suppressMessages(read_raw_csv_database(
    paths[["quarterly"]], annual_path = NULL, fallback = fx
  ))

  # NULCBS/RMR/IAD are genuinely re-derived (all their inputs are full-
  # history direct/level_from_pct primitives, present in the template) --
  # not just fallback pass-through.
  for (v in c("NULCBS", "RMR", "IAD")) {
    expect_lt(max_common_diff(db[[v]], fx[[v]]), 1e-4)
  }
  # WRR/WR2R/KIBN/IBREDR/IDDR need raw inputs the fixture never carried
  # (WRCR/WN2R/annual capital stocks; see raw_csv_template()'s docs), so
  # they fall all the way back to the fixture -- still an exact match, via
  # the fallback merge rather than fresh derivation.
  for (v in c("WRR", "WR2R")) {
    expect_lt(max_common_diff(db[[v]], fx[[v]]), 1e-9)
  }
})

test_that("raw_csv_template()'s PTM round-trips through the pct-change inverse exactly", {
  fx  <- csv_fixture_db()
  tmp <- tempfile(fileext = ".csv")
  raw_csv_template(tmp, fixture = fx)
  db <- suppressMessages(read_raw_csv_database(tmp, fallback = fx))
  # Post-1982Q1 (the level_from_pct cumulation base), PTM is genuinely
  # re-cumulated from the round-tripped pct-change column. Tolerance is
  # floating-point-level (not exact 1e-9) because the pct-change round trip
  # accumulates rounding error over ~150 quarters of compounding.
  expect_lt(max_common_diff(db[["PTM"]], fx[["PTM"]]), 1e-5)
})
