#' Path to a MARTIN model variant
#'
#' The default `"af"` form is split, for readability, into **one file per
#' equation** (`RC.txt`, `ID.txt`, ...) grouped into per-block subdirectories
#' under `extdata/model_af/` (`01_household/`, `05_prices_wages/`, ...) and
#' assembled at load time by [read_model_lines()] — so for `"af"` this returns
#' the *directory*. The `"identity"` and `"est"` variants are still single
#' vendored `.txt` files.
#'
#' @param variant One of `"af"` (default — the behavioural form with a
#'   `ConstantAdjustment` add-factor slot on every equation; 95 `BEHAVIORAL>`
#'   equations, of which only ~51 carry a `RESTRICT> c1=1` while the rest impose
#'   real cross-coefficient restrictions), `"identity"` (`MARTINMOD.txt`), or
#'   `"est"` (`MARTINMOD_EST.txt`). `bimets::ESTIMATE()` re-fits the free
#'   coefficients on every load — it does not replay published EViews values.
#' @return Absolute path — the `model_af/` directory for `"af"`, or the `.txt`
#'   file for the other variants.
#' @export
model_file_path <- function(variant = c("af", "identity", "est")) {
  variant <- match.arg(variant)
  if (variant == "af") return(model_af_dir())
  extdata_path(switch(variant, identity = "MARTINMOD.txt", est = "MARTINMOD_EST.txt"))
}

#' Directory holding the split AF model (one file per economic block)
#'
#' @return Absolute path to `extdata/model_af/`. Errors if absent.
#' @export
model_af_dir <- function() {
  d <- file.path(.martin_root(), "extdata", "model_af")
  if (!dir.exists(d)) {
    stop("model_af/ directory not found: ", d,
         "\n  (source 'setup.R' from the repo root first).", call. = FALSE)
  }
  d
}

#' Read a MARTIN model variant as a character vector of model-definition lines
#'
#' The `"af"` variant is assembled from the per-equation files in
#' [model_af_dir()] (recursively, sorted so block subdirectories order by their
#' numeric prefix and equations sort within each; wrapped in `MODEL` ... `END`);
#' the other variants are read straight from their single vendored file.
#' Equation order does not affect estimation or simulation — bimets builds the
#' system from the equations, not their file order.
#'
#' @param variant See [model_file_path()].
#' @return Character vector of model-definition lines.
#' @export
read_model_lines <- function(variant = c("af", "identity", "est")) {
  variant <- match.arg(variant)
  if (variant != "af") return(readLines(model_file_path(variant)))
  files <- sort(list.files(model_af_dir(), pattern = "\\.txt$",
                           full.names = TRUE, recursive = TRUE))
  c("MODEL",
    "COMMENT> MARTIN macroeconometric model (bimets additive-factor form)",
    "COMMENT> assembled from the per-equation files in extdata/model_af/",
    "",
    unlist(lapply(files, readLines), use.names = FALSE),
    "",
    "END")
}

#' Path to the bundled MARTINDATA fixture
#'
#' The frozen `MARTINDATA_XLSX.xlsx` copied from the bimets MARTIN port. Used
#' by the regression test that asserts MARTIN's solve matches the bimets
#' reference solve.
#'
#' @return Absolute path to the `.xlsx` file.
#' @export
martin_data_fixture <- function() {
  extdata_path("martin_data_fixture.xlsx")
}

#' Load a MARTIN bimets model with data
#'
#' Wraps the canonical pattern from
#' `the bimets MARTIN port BIMETS_MARTIN_LOAD.R`:
#'
#' ```r
#' MARTIN <- bimets::LOAD_MODEL("MARTINMOD_AF.txt")
#' MARTIN <- bimets::LOAD_MODEL_DATA(MARTIN, data)
#' MARTIN <- bimets::ESTIMATE(MARTIN)
#' ```
#'
#' For the default `variant = "af"`, ESTIMATE actually re-fits the free
#' coefficients of all 95 behavioural equations on every load — it does NOT
#' merely confirm imposed values. Only ~51 of those equations carry a
#' `RESTRICT> c1=1`; the rest impose genuine cross-coefficient restrictions
#' (e.g. `c4+c5+c6+c7=1`). ESTIMATE also computes each equation's residuals,
#' and those residuals are what the add-factor pipeline consumes downstream.
#' "Frozen" coefficients (the default) means estimating over the model file's
#' embedded 2019Q3 `TSRANGE` sample, not loading published EViews values.
#'
#' @param database A named list of `bimets::TIMESERIES` keyed by MARTIN
#'   variable name. Eventually produced by
#'   [to_martin_database()]; for now, by [read_fixture()] in tests.
#' @param variant Which model file to load. See [model_file_path()].
#' @param estimation_end Optional `"yyyyQq"` string. When set, rewrites
#'   every `TSRANGE` line's end date to this quarter before bimets
#'   loads the model — letting ESTIMATE re-fit the behavioural
#'   coefficients on data through the supplied end (typically the
#'   latest National Accounts release). Per-equation start dates are
#'   preserved. The model file's frozen default is 2019Q3.
#' @param estimate Logical. If `TRUE` (default), call `bimets::ESTIMATE()`
#'   after loading data. Skip only for path-checking; the residual slots
#'   downstream code uses are populated by ESTIMATE.
#'
#' @return A loaded bimets model object.
#' @export
load_martin <- function(database,
                        variant         = c("af", "identity", "est"),
                        estimate        = TRUE,
                        estimation_end  = NULL,
                        features        = character(0),
                        feature_params  = list()) {
  variant <- match.arg(variant)
  if (!is.list(database) || length(database) == 0L) {
    stop("`database` must be a non-empty named list of bimets TIMESERIES.",
         call. = FALSE)
  }
  if (is.null(names(database)) || any(!nzchar(names(database)))) {
    stop("`database` must be named; names are MARTIN variable codes.",
         call. = FALSE)
  }

  model_lines <- read_model_lines(variant)
  if (!is.null(estimation_end)) {
    model_lines <- rewrite_tsrange_end(model_lines, estimation_end)
  }
  if (length(features)) {
    # Seed any series the features introduce (idempotent: only adds what is
    # missing), so load_martin(features=) works standalone, not only via
    # solve_martin().
    database <- seed_feature_data(database, features, feature_params)
    model_lines <- apply_model_features(model_lines, features, feature_params)
  }
  model_text <- paste(model_lines, collapse = "\n")
  .suppress_bimets_version_warning({
    m <- bimets::LOAD_MODEL(modelText = model_text)
    m <- bimets::LOAD_MODEL_DATA(m, database)
    if (isTRUE(estimate)) {
      m <- bimets::ESTIMATE(m)
    }
  })
  m
}

# Rewrite each `TSRANGE <start_year> <start_quarter> <end_year> <end_quarter>`
# line in the model text so the end equals `end_quarter` (a "yyyyQq" string),
# preserving each equation's per-line start date. Used by load_martin's
# `estimation_end` option to re-estimate behavioural coefficients on a
# longer sample than the model file's frozen 2019Q3 default.
#
# Guarded: if `estimation_end` predates a given equation's own TSRANGE start,
# that equation's estimation window would be empty/inverted (end < start),
# which bimets fails on deep inside ESTIMATE with an opaque error. We check
# every equation's start up front and stop() with the offending equation
# names plus the minimum viable `estimation_end` (the latest start across all
# equations), rather than letting that surface as a confusing bimets error.
rewrite_tsrange_end <- function(lines, end_quarter) {
  if (!grepl("^[0-9]{4}Q[1-4]$", end_quarter)) {
    stop("`estimation_end` must be a 'yyyyQq' string (e.g. '2025Q2').",
         call. = FALSE)
  }
  new_year    <- substr(end_quarter, 1, 4)
  new_quarter <- substr(end_quarter, 6, 6)
  end_idx     <- quarter_index(as.integer(new_year), as.integer(new_quarter))

  is_tsrange <- grepl("^TSRANGE ", lines)
  is_decl    <- grepl("^(BEHAVIORAL>|IDENTITY>)", lines)

  # Name of the equation each TSRANGE line belongs to: the nearest preceding
  # BEHAVIORAL>/IDENTITY> declaration (equation files are `<decl>` then
  # `TSRANGE` then `EQ>`, so this is always the line's own equation).
  decl_name <- character(length(lines))
  cur <- NA_character_
  for (i in seq_along(lines)) {
    if (is_decl[i]) cur <- sub("^(BEHAVIORAL>|IDENTITY>)\\s*", "", lines[i])
    decl_name[i] <- cur
  }

  offending     <- character(0)
  max_start_idx <- -Inf
  for (i in which(is_tsrange)) {
    parts <- strsplit(lines[i], " ")[[1]]
    # Expected: TSRANGE start_year start_q end_year end_q  (5 tokens)
    if (length(parts) != 5L) {
      warning("rewrite_tsrange_end(): TSRANGE line does not split into 5 ",
              "space-separated tokens; left unchanged: '", lines[i], "'",
              call. = FALSE)
      next
    }
    start_idx <- quarter_index(as.integer(parts[2]), as.integer(parts[3]))
    if (start_idx > max_start_idx) max_start_idx <- start_idx
    if (start_idx > end_idx) offending <- c(offending, decl_name[i])
  }

  if (length(offending) > 0L) {
    min_viable <- format_quarter(max_start_idx %/% 4L, max_start_idx %% 4L + 1L)
    stop(
      "`estimation_end` = '", end_quarter, "' predates the TSRANGE start of ",
      length(unique(offending)), " equation(s): ",
      paste(unique(offending), collapse = ", "),
      ". Minimum viable `estimation_end` (>= every equation's own start) is '",
      min_viable, "'.", call. = FALSE
    )
  }

  lines[is_tsrange] <- vapply(lines[is_tsrange], function(ln) {
    parts <- strsplit(ln, " ")[[1]]
    if (length(parts) != 5L) return(ln)
    paste("TSRANGE", parts[2], parts[3], new_year, new_quarter)
  }, character(1))
  lines
}

#' Save a fitted MARTIN bimets model to disk
#'
#' Persists the object [load_martin()] returns (class `"BIMETS_MODEL"`) via
#' `saveRDS()`, so the expensive `ESTIMATE()` step -- 95 behavioural
#' regressions on every load -- can be paid once and the fitted coefficients
#' + residuals reused across many [solve_martin()] calls. Records the bimets
#' version and a few structural counts as an attribute for later inspection;
#' does not itself change anything about the model object's behaviour when
#' passed back into `bimets::SIMULATE()`.
#'
#' @param model A bimets model object (class `"BIMETS_MODEL"`), typically
#'   from [load_martin()].
#' @param path File path to write the `.rds` to.
#' @return `path`, invisibly.
#' @export
save_martin_model <- function(model, path) {
  if (!inherits(model, "BIMETS_MODEL")) {
    stop("`model` must be a bimets model object (class 'BIMETS_MODEL'); got: ",
         paste(class(model), collapse = ", "), call. = FALSE)
  }
  attr(model, "martin_persisted") <- list(
    bimets_version = if (!is.null(model$bimets_version)) {
      model$bimets_version
    } else {
      as.character(utils::packageVersion("bimets"))
    },
    saved_at      = format(Sys.time(), tz = "UTC", usetz = TRUE),
    n_behaviorals = length(model$behaviorals),
    n_identities  = length(model$identities)
  )
  saveRDS(model, path)
  invisible(path)
}

#' Load a previously saved MARTIN bimets model
#'
#' Reads back a model [save_martin_model()] wrote, optionally re-attaching a
#' fresh `database` via `bimets::LOAD_MODEL_DATA()` -- WITHOUT re-running
#' `ESTIMATE()`, so the persisted (possibly frozen) coefficients are never
#' refit. This is the pattern for reusing a fitted model against later data
#' (a new data build, or a database [solve_martin()] has spliced for
#' `exogenize=`/`profiles=`) without paying the ESTIMATE cost again.
#'
#' @param path File path a prior [save_martin_model()] call wrote.
#' @param database Optional named list of `bimets::TIMESERIES` (same shape
#'   [load_martin()] takes) to re-attach. Omit to get the model exactly as
#'   saved.
#' @return The loaded bimets model object.
#' @export
load_martin_model <- function(path, database = NULL) {
  if (!file.exists(path)) {
    stop("No saved model at: ", path, call. = FALSE)
  }
  model <- readRDS(path)
  if (!inherits(model, "BIMETS_MODEL")) {
    stop("File at '", path, "' is not a saved bimets model (class ",
         "'BIMETS_MODEL'); got: ", paste(class(model), collapse = ", "),
         call. = FALSE)
  }
  if (!is.null(database)) {
    model <- reattach_model_data(model, database)
  }
  model
}

# Re-attach `database` to an already-loaded/ESTIMATEd bimets `model` via
# bimets::LOAD_MODEL_DATA(), without re-running ESTIMATE (which would refit
# every behavioural coefficient). Shared by load_martin_model()'s `database`
# argument and by solve_martin(model = ...) when the caller's `database` was
# further modified (exogenize/profiles/feature splicing) after the model was
# built or persisted.
#
# Empirically verified (see tests/testthat/test-model-persistence.R):
# bimets::LOAD_MODEL_DATA() does not touch `$behaviorals` at all -- calling
# it again, even with materially different data, leaves `$coefficients` and
# `$residuals` `identical()` to what they were pre-call, because those slots
# are populated only by ESTIMATE(), which LOAD_MODEL_DATA() never invokes; it
# only refreshes the raw data SIMULATE() reads from. We still defensively
# restore `$behaviorals` from the pre-call object afterwards, so a future
# bimets version that starts touching that slot on LOAD_MODEL_DATA() cannot
# silently reset a persisted model's frozen coefficients/residuals out from
# under this package.
reattach_model_data <- function(model, database) {
  if (!is.list(database) || length(database) == 0L) {
    stop("`database` must be a non-empty named list of bimets TIMESERIES.",
         call. = FALSE)
  }
  pre_behaviorals <- model$behaviorals
  .suppress_bimets_version_warning({
    model <- bimets::LOAD_MODEL_DATA(model, database)
  })
  model$behaviorals <- pre_behaviorals
  model
}
