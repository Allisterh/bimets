# SMP-style forecast summary table.
#
# Mirrors the layout the EViews reference driver builds in
# `fcst_table.prg` (RBA Statement on Monetary Policy convention): a fixed
# row list of national-accounts aggregates reported as "percentage change
# over year to quarter shown" (i.e. four-quarter-ended growth, EViews'
# `@pcy`), plus the unemployment rate reported as a level. Columns are a
# handful of selected quarters (typically the SMP's own half-yearly
# Jun/Dec cadence).

# Row order + label + measure, taken directly from the `for %var Y RC ID
# IB G GNE M X HDY TOT WY LUR LE PW PAE PTM P` loop in fcst_table.prg —
# same variables, same order, same labels (rows 5-21 of `_smp`). Every row
# is `@pcy` (year-ended % change) except LUR, which the .prg leaves as a
# level (`{%var}_2` with no `@pcy`).
FORECAST_TABLE_ROWS <- tibble::tribble(
  ~variable, ~row_label,                            ~measure,
  "Y",       "Gross domestic product",               "yoy_pct",
  "RC",      "Household consumption",                "yoy_pct",
  "ID",      "Dwelling investment",                  "yoy_pct",
  "IB",      "Business investment",                  "yoy_pct",
  "G",       "Public demand",                        "yoy_pct",
  "GNE",     "Gross national expenditure",            "yoy_pct",
  "M",       "Imports",                               "yoy_pct",
  "X",       "Exports",                               "yoy_pct",
  "HDY",     "Real household disposable income",      "yoy_pct",
  "TOT",     "Terms of trade",                        "yoy_pct",
  "WY",      "USA GDP",                                "yoy_pct",
  "LUR",     "Unemployment rate (quarterly, %)",       "level",
  "LE",      "Employment",                             "yoy_pct",
  "PW",      "Wage price index",                       "yoy_pct",
  "PAE",     "Nominal average earnings per hour",      "yoy_pct",
  "PTM",     "Trimmed mean inflation",                 "yoy_pct",
  "P",       "Consumer Price Index",                   "yoy_pct"
)

#' Build an SMP-style forecast summary table
#'
#' Reduces a [solve_martin()] projection to the RBA Statement on Monetary
#' Policy "Forecast Table" layout: year-ended (four-quarter-ended)
#' percentage changes for the headline national-accounts aggregates, plus
#' the unemployment rate as a level. Row list, order, and measure are
#' lifted directly from the EViews reference (`fcst_table.prg`).
#'
#' Year-ended change at quarter `q` is `(value[q] / value[q - 4Q] - 1) *
#' 100`, computed straight from the quarterly paths in `projection`. This
#' is the history/forecast join: [solve_martin()]'s baseline replays
#' history into the same tibble as the forecast (see CLAUDE.md), so as
#' long as `projection` spans at least four quarters before the first
#' `quarters` entry, the lag lookup resolves whether the lagged level sits
#' in the replayed-history portion or the forecast portion — the caller
#' does not need to special-case either.
#'
#' @param projection A long tibble as returned by [solve_martin()]
#'   (columns `variable`, `quarter` (`"yyyyQq"`), `value`, and typically
#'   `scenario`). Must contain a single scenario — filter first if the
#'   tibble holds more than one.
#' @param quarters Character vector of `"yyyyQq"` quarters to show as
#'   table columns. Default `NULL` derives the SMP's own half-yearly
#'   cadence: every Jun/Dec quarter present in `projection` that also has
#'   a year-earlier level available for the lag.
#' @return A `martin_forecast_table` (a tibble subclass) with columns
#'   `row_label`, `variable`, `measure` (`"yoy_pct"` or `"level"`), and one
#'   column per entry of `quarters`, named by that quarter string. Rows
#'   for a variable absent from `projection` are `NA` (with a warning).
#' @export
forecast_table <- function(projection, quarters = NULL) {
  required_cols <- c("variable", "quarter", "value")
  missing_cols <- setdiff(required_cols, names(projection))
  if (length(missing_cols)) {
    stop("`projection` is missing column(s): ",
         paste(missing_cols, collapse = ", "),
         " -- pass the long tibble returned by solve_martin().",
         call. = FALSE)
  }
  if ("scenario" %in% names(projection) &&
      dplyr::n_distinct(projection$scenario) > 1L) {
    stop("`projection` contains more than one scenario (",
         paste(unique(projection$scenario), collapse = ", "),
         "); filter to a single scenario before calling forecast_table().",
         call. = FALSE)
  }

  missing_vars <- setdiff(FORECAST_TABLE_ROWS$variable, unique(projection$variable))
  if (length(missing_vars)) {
    warning("forecast_table(): projection is missing variable(s): ",
            paste(missing_vars, collapse = ", "),
            " -- those rows will be NA.", call. = FALSE)
  }

  all_quarters <- sort(unique(projection$quarter))
  if (is.null(quarters)) {
    quarters <- forecast_table_default_quarters(all_quarters)
  }
  if (length(quarters) == 0L) {
    stop("No quarters available to tabulate (need at least one quarter with ",
         "a year-earlier level present in `projection`).", call. = FALSE)
  }

  row_matrix <- lapply(seq_len(nrow(FORECAST_TABLE_ROWS)), function(i) {
    forecast_table_row_values(
      projection, FORECAST_TABLE_ROWS$variable[i],
      FORECAST_TABLE_ROWS$measure[i], quarters
    )
  })

  out <- tibble::tibble(
    row_label = FORECAST_TABLE_ROWS$row_label,
    variable  = FORECAST_TABLE_ROWS$variable,
    measure   = FORECAST_TABLE_ROWS$measure
  )
  # Each `row_matrix` element is one table row (a named vector over
  # `quarters`); rbind them into a rows x quarters matrix before widening.
  row_mat <- do.call(rbind, row_matrix)
  colnames(row_mat) <- quarters
  wide_df <- as.data.frame(row_mat, check.names = FALSE,
                            stringsAsFactors = FALSE)
  out <- dplyr::bind_cols(out, tibble::as_tibble(wide_df))
  class(out) <- c("martin_forecast_table", class(out))
  out
}

# Value lookup for one (variable, measure) across `quarters`: level for
# `measure == "level"`, year-ended % change otherwise. A missing current or
# lagged value (variable absent, quarter outside the projection's span)
# yields NA for that cell rather than an error -- a forecast table is a
# summary view and partial coverage should degrade gracefully.
forecast_table_row_values <- function(projection, variable, measure, quarters) {
  sub <- projection[projection$variable == variable, c("quarter", "value")]
  vals <- stats::setNames(as.numeric(sub$value), sub$quarter)
  vapply(quarters, function(q) {
    # `[` (not `[[`) on a named atomic vector returns NA for an unmatched
    # name instead of erroring, which is what "quarter not in projection"
    # should mean here.
    cur <- unname(vals[q])
    if (is.na(cur)) return(NA_real_)
    if (identical(measure, "level")) return(as.numeric(cur))
    pq <- parse_quarter(q)
    lag_q <- format_quarter(pq$year - 1L, pq$quarter)
    prev <- unname(vals[lag_q])
    if (is.na(prev) || prev == 0) return(NA_real_)
    100 * (cur / prev - 1)
  }, numeric(1))
}

# Default column quarters: the SMP's own half-yearly Jun/Dec cadence,
# restricted to quarters that actually have a year-earlier level to lag
# against (so every default column is computable, never blanket-NA).
forecast_table_default_quarters <- function(available) {
  if (length(available) == 0L) return(character(0))
  pq <- parse_quarter(available)
  has_lag <- vapply(seq_along(available), function(i) {
    format_quarter(pq$year[i] - 1L, pq$quarter[i]) %in% available
  }, logical(1))
  candidates <- available[has_lag & pq$quarter %in% c(2L, 4L)]
  if (length(candidates) == 0L) candidates <- available[has_lag]
  candidates
}

#' @rdname forecast_table
#' @param x A `martin_forecast_table`.
#' @param ... Unused; present for S3 compatibility.
#' @export
print.martin_forecast_table <- function(x, ...) {
  q_cols <- setdiff(names(x), c("row_label", "variable", "measure"))
  header <- vapply(q_cols, function(q) {
    pq <- parse_quarter(q)
    mon <- switch(as.character(pq$quarter),
                   "2" = "Jun", "4" = "Dec", paste0("Q", pq$quarter))
    paste0(mon, " ", pq$year)
  }, character(1))

  label_width <- max(nchar(x$row_label),
                      nchar("Percentage change over year to quarter shown"))
  col_width <- max(8L, max(nchar(header)) + 2L)

  cat("Forecast Table -- MARTIN\n")
  cat("Percentage change over year to quarter shown ",
      "(unemployment rate: level, %)\n\n", sep = "")
  cat(formatC("", width = label_width, flag = "-"))
  for (h in header) cat(formatC(h, width = col_width))
  cat("\n")
  for (i in seq_len(nrow(x))) {
    cat(formatC(x$row_label[i], width = label_width, flag = "-"))
    for (q in q_cols) {
      v <- x[[q]][i]
      cell <- if (is.na(v)) "n/a" else formatC(v, format = "f", digits = 1)
      cat(formatC(cell, width = col_width))
    }
    cat("\n")
  }
  invisible(x)
}

#' Write a forecast table to CSV
#'
#' Thin wrapper over [readr::write_csv()] so callers don't need to
#' remember to coerce away the `martin_forecast_table` subclass first.
#'
#' @param tbl A `martin_forecast_table` (or any [forecast_table()]-shaped
#'   tibble).
#' @param path Destination CSV path.
#' @return `path`, invisibly.
#' @export
write_forecast_table <- function(tbl, path) {
  readr::write_csv(tibble::as_tibble(tbl), path)
  invisible(path)
}
