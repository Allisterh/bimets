# Tests for solve_martin(profiles=): imposing forecast profiles with
# per-variable windows. normalize_profiles() is pure (no bimets); the
# integration test runs a real forecast-period solve.

test_that("normalize_profiles validates and spans per-variable windows", {
  p <- data.frame(
    variable = c("WY", "WY", "WY", "PTM"),
    quarter  = c("2026Q2", "2026Q3", "2026Q4", "2026Q2"),
    value    = c(101, 102, 103, 50)
  )
  spec <- normalize_profiles(p)
  expect_setequal(names(spec), c("WY", "PTM"))
  # WY spans 2026Q2..2026Q4; PTM is a single quarter.
  expect_equal(spec$WY$ex_start$year, 2026);  expect_equal(spec$WY$ex_start$quarter, 2)
  expect_equal(spec$WY$ex_end$year,   2026);  expect_equal(spec$WY$ex_end$quarter,   4)
  expect_equal(spec$PTM$ex_start$quarter, 2)
  expect_equal(spec$PTM$ex_end$quarter,   2)
})

test_that("normalize_profiles rejects bad frames, gaps, dupes, NA", {
  expect_error(normalize_profiles(list(a = 1)), "data.frame")
  expect_error(normalize_profiles(data.frame(variable = "WY", quarter = "2026Q2")),
               "variable.*quarter.*value")
  expect_error(
    normalize_profiles(data.frame(variable = "WY",
                                  quarter = c("2026Q2", "2026Q4"),   # gap
                                  value = c(1, 2))),
    "contiguous")
  expect_error(
    normalize_profiles(data.frame(variable = "WY",
                                  quarter = c("2026Q2", "2026Q2"),   # dupe
                                  value = c(1, 2))),
    "duplicate")
  expect_error(
    normalize_profiles(data.frame(variable = "WY", quarter = "2026Q2",
                                  value = NA_real_)),
    "NA")
})

test_that("profiles impose a global path and a 1-qtr nowcast in one solve", {
  skip_if_not_installed("bimets")
  skip_if_not(file.exists(martin_data_fixture()), "MARTINDATA fixture missing")

  db <- read_fixture()
  db <- extend_exogenous(db, end_quarter = "2021Q4")
  H  <- c("2015Q1", "2021Q4")
  fc <- "2019Q4"
  gv <- function(p, v, q) { x <- p$value[p$variable == v & p$quarter == q]; if (length(x)) x else NA_real_ }

  base <- suppressMessages(suppressWarnings(
    solve_martin(db, horizon = H, scenario = "base")))

  wyq <- quarter_seq(fc, "2021Q4")
  profiles <- rbind(
    data.frame(variable = "WY", quarter = wyq,
               value = vapply(wyq, function(q) gv(base, "WY", q), 0) * 1.02),
    data.frame(variable = "PTM", quarter = fc, value = gv(base, "PTM", fc) * 1.003),
    data.frame(variable = "LUR", quarter = fc, value = gv(base, "LUR", fc) + 0.2)
  )
  cond <- suppressMessages(suppressWarnings(
    solve_martin(db, horizon = H, profiles = profiles, scenario = "cond")))

  expect_true(isTRUE(attr(cond, "convergence")$converged))

  # Global var held on its imposed path EVERY forecast quarter.
  wy_dev <- vapply(wyq, function(q) 100 * (gv(cond, "WY", q) / gv(base, "WY", q) - 1), 0)
  expect_true(all(abs(wy_dev - 2) < 1e-6))

  # 1-qtr nowcasts hit their target in the imposed quarter ...
  expect_equal(gv(cond, "PTM", fc), gv(base, "PTM", fc) * 1.003, tolerance = 1e-6)
  expect_equal(gv(cond, "LUR", fc) - gv(base, "LUR", fc), 0.2, tolerance = 1e-6)
  # ... and are free (model-determined, != baseline) the quarter after.
  expect_gt(abs(gv(cond, "PTM", "2020Q1") - gv(base, "PTM", "2020Q1")), 1e-8)
  expect_gt(abs(gv(cond, "LUR", "2020Q1") - gv(base, "LUR", "2020Q1")), 1e-8)

  # Imposed values propagate: exports respond to the world-GDP path.
  expect_gt(gv(cond, "X", "2021Q4") - gv(base, "X", "2021Q4"), 0)
})
