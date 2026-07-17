# Tests for save_martin_model()/load_martin_model() and solve_martin(model=)
# / solve_martin_stochastic(model=): persisting a fitted bimets model so the
# expensive ESTIMATE() step (95 behavioural regressions) can be paid once and
# reused across solves, instead of on every solve_martin() call.

# Local carry-forward helper (mirrors the one in
# test-regression-against-bimets.R), defined up front (not at file end) since
# testthat evaluates test_that() blocks immediately in file order -- a
# forward reference to a function defined later in the file only works by
# accident when an earlier branch (e.g. `requireNamespace("sibyldata")`)
# happens to skip the call on this machine.
carry_forward_all_for_test <- function(db, end_q) {
  yq <- parse_quarter(end_q)
  target_dec <- yq$year + (yq$quarter - 1) / 4
  for (nm in names(db)) {
    ts  <- db[[nm]]
    tsp <- stats::tsp(ts)
    if (target_dec <= tsp[2] + 1e-9) next
    n_new <- round((target_dec - tsp[2]) * 4)
    vals  <- as.numeric(ts)
    li    <- suppressWarnings(max(which(is.finite(vals))))
    seed  <- if (is.finite(li)) vals[li] else 0
    sy    <- floor(tsp[1] + 1e-9)
    sq    <- round((tsp[1] - sy) * 4 + 1)
    db[[nm]] <- bimets::TIMESERIES(
      c(vals, rep(seed, n_new)), START = c(sy, sq), FREQ = 4
    )
  }
  db
}

test_that("save/load round-trips coefficients and residuals bit-identically", {
  skip_if_not_installed("bimets")
  skip_if_not(file.exists(martin_data_fixture()), "MARTINDATA fixture missing")

  data  <- read_fixture()
  model <- load_martin(data, variant = "af", estimate = TRUE)

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  save_martin_model(model, path)
  expect_true(file.exists(path))

  loaded <- load_martin_model(path)
  expect_s3_class(loaded, "BIMETS_MODEL")

  for (eq in c("RC", "LE", "PTM", "OTC", "N10R", "N2R")) {
    expect_identical(
      model$behaviorals[[eq]]$coefficients,
      loaded$behaviorals[[eq]]$coefficients,
      info = paste("coefficients diverged for", eq)
    )
    expect_identical(
      model$behaviorals[[eq]]$residuals,
      loaded$behaviorals[[eq]]$residuals,
      info = paste("residuals diverged for", eq)
    )
  }
})

test_that("load_martin_model() refuses a non-model file", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(list(not = "a model"), path)
  expect_error(load_martin_model(path), "BIMETS_MODEL")
})

test_that("load_martin_model() refuses a missing path", {
  expect_error(load_martin_model(tempfile(fileext = ".rds")), "No saved model")
})

test_that("save_martin_model() refuses a non-bimets-model object", {
  expect_error(save_martin_model(list(a = 1), tempfile()), "BIMETS_MODEL")
})

test_that("load_martin_model(database=) reattaches data without dropping residuals", {
  skip_if_not_installed("bimets")
  skip_if_not(file.exists(martin_data_fixture()), "MARTINDATA fixture missing")

  data  <- read_fixture()
  model <- load_martin(data, variant = "af", estimate = TRUE)

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  save_martin_model(model, path)

  reloaded <- load_martin_model(path, database = data)
  expect_identical(
    model$behaviorals$RC$coefficients,
    reloaded$behaviorals$RC$coefficients
  )
  expect_identical(
    model$behaviorals$RC$residuals,
    reloaded$behaviorals$RC$residuals
  )
  expect_false(is.null(reloaded$behaviorals$RC$residuals))
})

HEADLINE <- c("Y", "RC", "GNE", "LUR", "PTM", "NCR")
HORIZON  <- c("2010Q1", "2019Q3")

test_that("solve_martin(model=) matches solve_martin() fresh on the same database", {
  skip_if_not_installed("bimets")
  skip_if_not(file.exists(martin_data_fixture()), "MARTINDATA fixture missing")

  data <- read_fixture()

  fresh <- solve_martin(
    database = data, horizon = HORIZON, scenario = "fresh"
  )

  model <- load_martin(data, variant = "af", estimate = TRUE)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  save_martin_model(model, path)
  loaded <- load_martin_model(path)

  from_model <- solve_martin(
    database = data, horizon = HORIZON, scenario = "from_model",
    model = loaded
  )

  for (var in HEADLINE) {
    ref_v <- dplyr::filter(fresh,      variable == var)
    got_v <- dplyr::filter(from_model, variable == var)
    expect_equal(nrow(ref_v), nrow(got_v), info = paste("row count for", var))
    expect_equal(
      got_v$value, ref_v$value,
      tolerance = 1e-8,
      info = paste("value column for", var)
    )
    expect_equal(got_v$quarter, ref_v$quarter,
                 info = paste("quarter alignment for", var))
  }
})

test_that("solve_martin(model=) skips ESTIMATE (no re-fit output)", {
  skip_if_not_installed("bimets")
  skip_if_not(file.exists(martin_data_fixture()), "MARTINDATA fixture missing")

  data  <- read_fixture()
  model <- load_martin(data, variant = "af", estimate = TRUE)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  save_martin_model(model, path)
  loaded <- load_martin_model(path)

  # bimets::ESTIMATE() prints "...ESTIMATE OK" to stdout (not via message()),
  # so this must use type = "output" -- type = "message" would trivially
  # pass regardless of whether ESTIMATE ran at all.
  out <- capture.output(
    solve_martin(
      database = data, horizon = HORIZON, scenario = "from_model",
      model = loaded
    ),
    type = "output"
  )
  # A model=-driven solve must never call ESTIMATE(). LOAD_MODEL_DATA() (used
  # to re-attach `database`) and SIMULATE() print their own distinct banners,
  # which are fine to see.
  expect_false(any(grepl("ESTIMATE OK", out, fixed = TRUE)))
  expect_true(any(grepl("LOAD MODEL DATA OK", out, fixed = TRUE)),
              info = "expected the model= path to still re-attach database")
})

test_that("solve_martin(model=) rejects a non-bimets-model object", {
  expect_error(
    solve_martin(
      database = list(Y = 1), horizon = c("2010Q1", "2010Q2"),
      model = list(not = "a model")
    ),
    "BIMETS_MODEL"
  )
})

test_that("solve_martin(model=) rejects combination with features", {
  expect_error(
    solve_martin(
      database = list(Y = 1), horizon = c("2010Q1", "2010Q2"),
      model = structure(list(), class = "BIMETS_MODEL"),
      features = "elb_floor"
    ),
    "features"
  )
})

test_that("solve_martin(model=) rejects combination with coefficients='reestimated'", {
  expect_error(
    solve_martin(
      database = list(Y = 1), horizon = c("2010Q1", "2010Q2"),
      model = structure(list(), class = "BIMETS_MODEL"),
      coefficients = "reestimated", estimation_end = "2025Q2"
    ),
    "reestimated"
  )
})

test_that("solve_martin(model=) re-attaches spliced data for exogenize", {
  skip_if_not_installed("bimets")
  skip_if_not(file.exists(martin_data_fixture()), "MARTINDATA fixture missing")

  data  <- read_fixture()
  model <- load_martin(data, variant = "af", estimate = TRUE)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  save_martin_model(model, path)
  loaded <- load_martin_model(path)

  baseline <- solve_martin(database = data, horizon = HORIZON, scenario = "base")

  # Exogenise NCR to a constant path well away from baseline; solve_martin()
  # splices this into `database` before ever touching `model`, so a
  # model=-driven solve must reflect it exactly like a fresh one does.
  ncr_flat <- dplyr::mutate(
    dplyr::filter(baseline, variable == "NCR"), value = 9
  )

  fresh_ex <- solve_martin(
    database = data, horizon = HORIZON, scenario = "fresh_ex",
    exogenize = "NCR", baseline_for_exogenize = ncr_flat
  )
  model_ex <- solve_martin(
    database = data, horizon = HORIZON, scenario = "model_ex",
    exogenize = "NCR", baseline_for_exogenize = ncr_flat,
    model = loaded
  )

  ref_ncr <- dplyr::filter(fresh_ex, variable == "NCR")$value
  got_ncr <- dplyr::filter(model_ex, variable == "NCR")$value
  expect_equal(got_ncr, ref_ncr, tolerance = 1e-8)
  expect_true(all(abs(got_ncr - 9) < 1e-6))
})

test_that("solve_martin_stochastic(model=) accepts a persisted model in both bimets and fallback paths", {
  skip_if_not_installed("bimets")
  skip_if_not(file.exists(martin_data_fixture()), "MARTINDATA fixture missing")

  data  <- read_fixture()
  model <- load_martin(data, variant = "af", estimate = TRUE)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  save_martin_model(model, path)
  loaded <- load_martin_model(path)

  fc_horizon <- c("2019Q4", "2020Q1")
  data_fc <- carry_forward_all_for_test(data, "2020Q1")

  out <- solve_martin_stochastic(
    database = data_fc, horizon = fc_horizon, scenario = "band",
    n_draws = 5L, model = loaded
  )
  expect_true(all(c("variable", "quarter", "value", "lower", "upper") %in%
                    names(out)))
  expect_true(nrow(out) > 0L)
})
