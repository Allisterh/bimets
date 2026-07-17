#' Load a MARTIN database from RAW/PRIMITIVE source-data CSVs
#'
#' A third entry point alongside the public-download pipeline
#' ([update_data()] + [to_martin_database()]) and the already-final-shape CSV
#' loader ([read_csv_database()]). Where those two hand you (or expect) MARTIN's
#' own final variable names (`RC`, `NCR`, `WPCOM`, ...), this one accepts the
#' *raw upstream* series the public pipeline downloads -- ABS/RBA/FRED source
#' data, still needing splicing, Chow-Lin disaggregation, cumulation, and
#' formula evaluation -- and runs the exact same [derive_model_database()]
#' chain [to_martin_database()] uses on a live download.
#'
#' This is for a caller who has their own primitive source data (e.g. a fresh
#' ABS/RBA export) but no working `update_data()` network path, and wants the
#' full derivation pipeline (Chow-Lin, splices, PIM, the identity chain,
#' derived formulas, state-space trends) applied to it, rather than supplying
#' MARTIN's ~200 final variables by hand.
#'
#' @section CSV formats:
#'   **Quarterly primitives CSV** (`path`): a period column (see
#'   [read_csv_database()] for the accepted formats) plus one column per
#'   *primitive* MARTIN variable -- catalogue rows tagged `direct`, `spliced`,
#'   or `level_from_pct`. This includes:
#'   * ordinary direct imports (`RC`, `NCR`, `WPCOM`, state-space signal
#'     inputs like `LA`, `LPOP`, `PI_E_BOND`, `GBUSEXP`, ...);
#'   * `P`, `PEX`, `PTM` as **percent-change** series (not the cumulated
#'     level MARTIN's equations use -- [apply_level_from_pct()] cumulates
#'     them from their registered 1982Q1 bases);
#'   * the splice inputs `NCR_HIST`, `NBR_SPLICE`, `PH_OLD`.
#'
#'   **Annual capital CSV** (`annual_path`, optional): a `year` column plus one
#'   column per annual capital-stock/investment-flow series: `KTOT`, `KID`,
#'   `KOTC`, `KIBRE` (net capital stocks), `CFCTOT`, `CFCID`, `CFCOTC`,
#'   `CFCIBRE` (consumption of fixed capital), `IBRE`, `NIBRE` (mining
#'   investment, real and nominal), and `IDA2` (dwelling investment -- a
#'   *different* annual ABS 5204.0 series from the quarterly `ID` already in
#'   the primitives CSV; see [apply_iddr_annual()]). One row per year; any
#'   subset of columns is accepted.
#'
#'   Both CSVs accept **any subset** of their columns -- whatever's missing is
#'   either derived from what *is* present (e.g. supply `WP`+`WRCR` and get
#'   `WRR` for free) or filled from `fallback` (default [read_fixture()]) via
#'   [derive_model_database()]'s own fallback merge, exactly like
#'   [read_csv_database()]'s `fallback` argument.
#'
#'   Use [raw_csv_template()] to generate a starting point for both files.
#'
#' @param path Path to the quarterly primitives CSV.
#' @param annual_path Path to the annual capital CSV, or `NULL` (default) to
#'   skip it -- the annual-driven constructors ([apply_kibn()],
#'   [apply_ibredr_annual()], [apply_iddr_annual()], the Chow-Lin
#'   disaggregation of `IBRE`/`NIBRE`/`KIBRE`/`KID`/`KTOT`/`KOTC`) then simply
#'   fall through to `fallback`.
#' @param date_col Name of the quarterly CSV's period column. `NULL`
#'   (default) auto-detects (see [read_csv_database()]).
#' @param catalogue [series_catalogue()].
#' @param fallback Named-list database whose values fill any variable/quarter
#'   the raw data + derivation chain could not supply. Default
#'   [read_fixture()] (pass `NULL` to disable).
#'
#' @return A named list of `bimets::TIMESERIES`, with attributes:
#'   * `primitives_supplied` / `primitives_missing` -- which catalogue
#'     primitive rows (`direct`/`spliced`/`level_from_pct`/`chowlin`) the two
#'     CSVs did/didn't provide.
#'   * `unknown_columns` -- CSV columns that aren't in the catalogue (loaded
#'     anyway, same convention as [read_csv_database()]).
#'   * `period_column` -- the quarterly CSV's detected/given period column.
#' @export
read_raw_csv_database <- function(path,
                                  annual_path = NULL,
                                  date_col    = NULL,
                                  catalogue   = series_catalogue(),
                                  fallback    = read_fixture()) {
  if (!file.exists(path)) {
    stop("Raw-primitives CSV not found at ", path, call. = FALSE)
  }
  raw <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                         na.strings = c("NA", "", "."))
  if (ncol(raw) < 2L) {
    stop("CSV must have a period column plus at least one variable column.",
         call. = FALSE)
  }

  # Reuse the quarterly-CSV period-parsing machinery from R/csv_data.R --
  # same accepted formats ("yyyyQq" / quarter-start dates), same errors.
  date_col <- date_col %||% detect_period_column(names(raw))
  if (!date_col %in% names(raw)) {
    stop("Period column '", date_col, "' not found. Columns: ",
         paste(names(raw), collapse = ", "), call. = FALSE)
  }
  qi <- parse_period_vector(raw[[date_col]])
  if (anyNA(qi)) {
    bad <- unique(raw[[date_col]][is.na(qi)])
    stop("Could not parse ", sum(is.na(qi)), " period value(s) in '",
         date_col, "', e.g.: ", paste(utils::head(bad, 3), collapse = ", "),
         ". Use 'yyyyQq' (2019Q3) or quarter-start dates (2019-07-01).",
         call. = FALSE)
  }
  if (anyDuplicated(qi)) {
    dup <- raw[[date_col]][duplicated(qi)]
    stop("Duplicate periods in '", date_col, "': ",
         paste(utils::head(unique(dup), 3), collapse = ", "),
         ". Each quarter must appear once.", call. = FALSE)
  }

  full_qi   <- seq.int(min(qi), max(qi))
  pos       <- match(qi, full_qi)
  data_cols <- setdiff(names(raw), date_col)

  database <- list()
  for (nm in data_cols) {
    vals_full <- rep(NA_real_, length(full_qi))
    vals_full[pos] <- coerce_numeric(raw[[nm]])
    ts <- numeric_to_quarterly_ts(vals_full, full_qi, trim_na = TRUE)
    if (!is.null(ts)) database[[nm]] <- ts
  }

  annual_db <- if (!is.null(annual_path)) {
    read_annual_csv(annual_path)
  } else {
    list()
  }

  db <- derive_model_database(database, annual_db, catalogue,
                              fallback = fallback)

  primitive_vars <- catalogue$martin_var[catalogue$transformation %in%
                                    c("direct", "spliced", "level_from_pct",
                                      "chowlin")]
  supplied_names       <- union(names(database), names(annual_db))
  primitives_supplied  <- intersect(primitive_vars, supplied_names)
  primitives_missing   <- setdiff(primitive_vars, primitives_supplied)
  unknown_columns      <- setdiff(supplied_names, catalogue$martin_var)

  message("read_raw_csv_database: supplied ", length(primitives_supplied), "/",
          length(primitive_vars), " primitive series",
          if (length(primitives_missing)) {
            sprintf("; %d not supplied (filled from fallback where possible)",
                    length(primitives_missing))
          } else "")

  attr(db, "primitives_supplied") <- primitives_supplied
  attr(db, "primitives_missing")  <- primitives_missing
  attr(db, "unknown_columns")     <- unknown_columns
  attr(db, "period_column")       <- date_col
  db
}

#' Write raw/primitive-data CSV templates
#'
#' The [read_raw_csv_database()] counterpart to [database_to_csv()]: writes a
#' fill-in-the-blanks quarterly primitives CSV (`path`) and a sibling annual
#' capital CSV (`annual_path`), so a caller can see exactly which raw source
#' series to supply instead of guessing catalogue column names.
#'
#' The quarterly template is populated from `fixture` wherever a primitive
#' column is present there (with `P`/`PEX`/`PTM` converted back from their
#' cumulated level to the percent-change form [apply_level_from_pct()]
#' expects -- an exact inverse of [cumulate_pct_to_level()], so a round-trip
#' through [read_raw_csv_database()] reproduces the fixture level to floating-
#' point precision). Some primitives (world-economy series like `WRCR`,
#' `WN2R`, `LA`, and the annual ABS 5204.0 capital-stock/CFC/investment
#' series) never made it into the bundled fixture -- it only carries MARTIN's
#' *final* model variables (see [read_fixture()]) -- so those columns are
#' written with headers only, `NA`-valued, naming exactly what a caller needs
#' to source themselves (see `docs/running_from_csv.md`, "Raw data CSV").
#'
#' @param path Output path for the quarterly primitives CSV.
#' @param annual_path Output path for the annual capital CSV. `NULL`
#'   (default) derives it from `path` (`"foo.csv"` -> `"foo_annual.csv"`).
#' @param catalogue [series_catalogue()].
#' @param fixture A MARTIN database to source values from. Default
#'   [read_fixture()].
#' @return `c(quarterly = path, annual = annual_path)`, invisibly.
#' @export
raw_csv_template <- function(path, annual_path = NULL,
                             catalogue = series_catalogue(),
                             fixture   = read_fixture()) {
  primitive_vars <- catalogue$martin_var[catalogue$transformation %in%
                                    c("direct", "spliced", "level_from_pct")]
  cols <- list()
  for (mv in primitive_vars) {
    src <- fixture[[mv]]
    if (is.null(src)) next
    cols[[mv]] <- if (!is.null(LEVEL_FROM_PCT_BASES[[mv]])) {
      level_to_pct_ts(src)
    } else {
      src
    }
  }
  if (length(cols) == 0L) {
    stop("raw_csv_template: no primitive columns available from `fixture`.",
         call. = FALSE)
  }
  database_to_csv(cols, path)

  if (is.null(annual_path)) {
    annual_path <- sub("(\\.csv)?$", "_annual.csv", path, ignore.case = TRUE)
  }
  # The chowlin slice of the catalogue plus IDA2 (the dwelling-investment
  # annual primitive apply_iddr_annual() needs; see its roxygen for why it
  # isn't just "ID"). read_fixture() carries no annual-frequency data (it's
  # quarterly-only), so this side is a header-only template.
  annual_vars <- sort(unique(c(
    catalogue$martin_var[catalogue$transformation == "chowlin"], "IDA2"
  )))
  ann <- data.frame(year = integer(0))
  for (v in annual_vars) ann[[v]] <- numeric(0)
  utils::write.csv(ann, annual_path, row.names = FALSE)

  invisible(c(quarterly = path, annual = annual_path))
}

# ---- internals --------------------------------------------------------------

# Read the annual capital CSV: a year column (auto-detected) plus one column
# per annual martin_var, one row per year. Returns a named list of
# frequency-1 `ts`, the shape derive_model_database()'s `annual_db` expects
# (matching pivot_annual()'s output shape in R/update_data.R).
read_annual_csv <- function(annual_path) {
  if (!file.exists(annual_path)) {
    stop("Annual CSV not found at ", annual_path, call. = FALSE)
  }
  raw <- utils::read.csv(annual_path, check.names = FALSE,
                         stringsAsFactors = FALSE,
                         na.strings = c("NA", "", "."))
  if (ncol(raw) < 2L) {
    stop("Annual CSV must have a year column plus at least one variable ",
         "column.", call. = FALSE)
  }
  if (nrow(raw) == 0L) return(list())  # header-only template; nothing to load

  year_col <- detect_year_column(names(raw))
  years <- suppressWarnings(as.integer(raw[[year_col]]))
  if (anyNA(years)) {
    stop("Could not parse year values in '", year_col,
         "' of the annual CSV.", call. = FALSE)
  }
  if (anyDuplicated(years)) {
    stop("Duplicate years in '", year_col, "' of the annual CSV.",
         call. = FALSE)
  }

  full_years <- seq.int(min(years), max(years))
  pos        <- match(years, full_years)
  data_cols  <- setdiff(names(raw), year_col)

  annual_db <- list()
  for (nm in data_cols) {
    vals_full <- rep(NA_real_, length(full_years))
    vals_full[pos] <- coerce_numeric(raw[[nm]])
    if (all(is.na(vals_full))) next
    annual_db[[nm]] <- stats::ts(vals_full, start = full_years[1],
                                 frequency = 1)
  }
  annual_db
}

# Pick the year column of the annual CSV by name, else fall back to the
# first column (mirrors detect_period_column() in R/csv_data.R).
detect_year_column <- function(cols) {
  lower <- tolower(trimws(cols))
  known <- c("year", "fy", "financial_year", "annual")
  hit <- which(lower %in% known | cols %in% c("", "V1"))
  if (length(hit) >= 1L) return(cols[hit[1]])
  message("read_raw_csv_database: no obvious year column in the annual ",
          "CSV; using the first column '", cols[1], "'.")
  cols[1]
}

# Inverse of cumulate_pct_to_level() (R/transformations.R): recover the
# percent-change series a level index was cumulated from. Exact (lossless)
# since it is the literal algebraic inverse of
# level[i] = level[i-1] * (1 + pct[i]/100). Used only by raw_csv_template()
# to turn the fixture's already-cumulated PTM/P/PEX level series back into
# the raw pct-change primitive read_raw_csv_database() expects.
level_to_pct_ts <- function(level_ts) {
  v   <- as.numeric(level_ts)
  pct <- c(NA_real_, 100 * (v[-1] / v[-length(v)] - 1))
  tsp <- stats::tsp(level_ts)
  start_year    <- floor(tsp[1] + 1e-9)
  start_quarter <- round((tsp[1] - start_year) * 4 + 1)
  bimets::TIMESERIES(pct, START = c(start_year, start_quarter), FREQ = 4)
}
