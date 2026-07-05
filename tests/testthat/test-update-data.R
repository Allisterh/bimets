test_that("update_data() rejects unknown sources", {
  expect_error(update_data(sources = "bogus"), "Unknown source")
})

test_that("pivot_quarterly places a gapped panel on a contiguous axis", {
  # 2018Q3 is absent from the panel. It must appear as an NA cell, NOT be
  # dropped (which would shift 2018Q4/2019Q1 one quarter earlier).
  rows <- data.frame(
    martin_var = "GAPV",
    date = as.Date(c("2018-01-01", "2018-04-01", "2018-10-01", "2019-01-01")),
    value = c(10, 20, 40, 50),
    source_frequency = "Q", aggregation = "last",
    stringsAsFactors = FALSE
  )
  ts <- pivot_quarterly(rows)[["GAPV"]]
  expect_equal(as.numeric(ts), c(10, 20, NA, 40, 50))
  expect_equal(as.numeric(stats::window(ts, c(2019, 1), c(2019, 1))), 50)
})

test_that("pivot_annual places a gapped panel on a contiguous axis", {
  rows <- data.frame(
    martin_var = "KA",
    date = as.Date(c("2015-06-30", "2016-06-30", "2018-06-30")),  # 2017 missing
    value = c(100, 110, 130),
    stringsAsFactors = FALSE
  )
  ts <- pivot_annual(rows)[["KA"]]
  expect_equal(as.numeric(ts), c(100, 110, NA, 130))
  expect_equal(stats::start(ts)[1], 2015)
})

test_that("fetch_fred() requires FRED_API_KEY", {
  withr::with_envvar(c(FRED_API_KEY = ""), {
    expect_error(fetch_fred("GDPC1"), "FRED_API_KEY")
  })
})

test_that("fetch_fred() returns canonical panel shape (live API)", {
  skip_if(Sys.getenv("FRED_API_KEY") == "", "FRED_API_KEY not set")
  skip_on_cran()
  skip_if_offline()

  withr::with_envvar(c(MARTIN_DATA_CACHE = tempfile("sibyl-test-")), {
    panel <- fetch_fred("GDPC1", observation_start = "2020-01-01")
    expect_s3_class(panel, "tbl_df")
    expect_setequal(
      names(panel),
      c("series_id", "source", "date", "value", "vintage")
    )
    expect_true(all(panel$source == "fred"))
    expect_true(all(panel$series_id == "GDPC1"))
    expect_true(nrow(panel) >= 4L)  # at least the four 2020 quarters
    expect_type(panel$value, "double")
  })
})

test_that("update_data(sources='fred') caches and re-reads (live API)", {
  skip_if(Sys.getenv("FRED_API_KEY") == "", "FRED_API_KEY not set")
  skip_on_cran()
  skip_if_offline()

  withr::with_envvar(c(MARTIN_DATA_CACHE = tempfile("sibyl-test-")), {
    vintage <- Sys.Date()
    p1 <- update_data(vintage = vintage, sources = "fred")
    expect_gt(nrow(p1), 0L)

    # Second call should come from the cache (no network).
    p2 <- update_data(vintage = vintage, sources = "fred", refresh = FALSE)
    expect_equal(nrow(p1), nrow(p2))
    expect_setequal(unique(p1$series_id), unique(p2$series_id))
  })
})
