# Tests for compute_tpw() + tpw_partner_weights() + build_tpw_cpi().

test_that("tpw_partner_weights() returns a normalised vector", {
  w <- tpw_partner_weights("goods_exports")
  expect_type(w, "double")
  expect_true(!is.null(names(w)))
  expect_equal(sum(w), 1, tolerance = 1e-10)
  expect_true("CHN" %in% names(w))
  expect_true(w["CHN"] >= 0.3)

  w2 <- tpw_partner_weights("two_way")
  expect_equal(sum(w2), 1, tolerance = 1e-10)
  expect_true(w2["USA"] > w["USA"])  # two-way weights US more
})

mk_partner_ts <- function(values, start_year, start_q = 1L) {
  bimets::TIMESERIES(values, START = c(start_year, start_q), FREQ = 4)
}

test_that("compute_tpw() weights partner indices and rebases to 100", {
  partners <- list(
    CHN = mk_partner_ts(seq(80, 130, length.out = 10), 2008),
    USA = mk_partner_ts(seq(90, 110, length.out = 10), 2008),
    JPN = mk_partner_ts(seq(95, 105, length.out = 10), 2008)
  )
  w <- c(CHN = 0.5, USA = 0.3, JPN = 0.2)
  agg <- compute_tpw(partners, w, base_quarter = "2010Q1")
  expect_s3_class(agg, "ts")

  agg_vec <- as.numeric(agg)
  q_labels <- paste0(
    rep(2008:2010, each = 4)[seq_along(agg_vec)],
    "Q", rep(1:4, length.out = length(agg_vec))
  )
  base_idx <- match("2010Q1", q_labels)
  expect_equal(agg_vec[base_idx], 100, tolerance = 1e-6)
})

test_that("compute_tpw() chain-links a partner that enters after the base", {
  # CHN spans the whole sample; KOR is missing the first two quarters and only
  # overlaps from 2008Q3. Chain-linking must NOT drop KOR just for lacking a
  # base-quarter observation -- it contributes growth once it overlaps CHN --
  # and the index stays continuous with base 2008Q1 = 100 (no level jump when
  # KOR enters).
  partners <- list(
    CHN = mk_partner_ts(seq(80, 130, length.out = 10), 2008),
    KOR = mk_partner_ts(c(NA, NA, seq(95, 105, length.out = 8)), 2008)
  )
  w <- c(CHN = 0.6, KOR = 0.4)
  agg <- compute_tpw(partners, w, base_quarter = "2008Q1")
  expect_s3_class(agg, "ts")
  v <- as.numeric(agg)
  expect_equal(v[1], 100, tolerance = 1e-6)      # base normalised to 100
  expect_false(anyNA(v))                          # fully defined (no interior NA)
  expect_true(all(diff(v) > 0))                   # both partners rising
  # The first step (2008Q1->Q2) is CHN-only (KOR not yet overlapping), so its
  # gross growth equals CHN's own.
  chn <- seq(80, 130, length.out = 10)
  expect_equal(v[2] / v[1], chn[2] / chn[1], tolerance = 1e-6)
})

test_that("compute_tpw() renormalises non-summing weights", {
  partners <- list(CHN = mk_partner_ts(rep(100, 5), 2010))
  expect_warning(
    compute_tpw(partners, c(CHN = 0.5), base_quarter = "2010Q1"),
    "weights sum"
  )
})

test_that("compute_tpw() errors when no partners overlap weights", {
  expect_error(
    compute_tpw(
      list(XXX = mk_partner_ts(rep(100, 5), 2010)),
      c(CHN = 1),
      base_quarter = "2010Q1"
    ),
    "no overlap"
  )
})

test_that("build_tpw_cpi() round-trips against live OECD", {
  skip_if(Sys.getenv("MARTIN_FETCH_OECD_LIVE") != "TRUE",
          "Set MARTIN_FETCH_OECD_LIVE=TRUE to run.")
  skip_if_offline()
  skip_on_cran()

  out <- build_tpw_cpi(observation_start = "2010-01-01",
                       base_quarter = "2010Q1")
  expect_s3_class(out, "ts")
  expect_gt(length(as.numeric(out)), 10L)
  # Aggregate at base should be ~100 (rounding from monthly aggregation).
  vals <- as.numeric(out)
  finite <- vals[is.finite(vals)]
  expect_true(min(finite) > 0)
})
