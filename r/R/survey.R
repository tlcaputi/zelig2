#' Survey Design Helpers
#'
#' Internal utilities for constructing \code{\link[survey]{svydesign}} objects
#' from the convenience parameters accepted by \code{\link{zelig2}}.
#'
#' @name survey-helpers
#' @keywords internal
NULL

#' Resolve survey design
#'
#' Creates a \code{\link[survey]{svydesign}} object from a pre-built design,
#' or from individual components (\code{ids}, \code{strata}, \code{fpc},
#' \code{weights}).
#'
#' @param data A data frame.
#' @param weights Weights: numeric vector, one-sided formula, or column-name
#'   string.
#' @param survey_design An existing \code{survey.design} object, or
#'   \code{NULL}.
#' @param ids Cluster IDs (formula or column-name string).
#' @param strata Strata (formula or column-name string).
#' @param fpc Finite population correction (formula or column-name string).
#' @param nest Logical; nest clusters within strata?
#' @return A \code{survey.design} object, or \code{NULL} if no survey
#'   parameters were supplied.
#' @keywords internal
resolve_survey <- function(data, weights = NULL, survey_design = NULL,
                           ids = NULL, strata = NULL, fpc = NULL,
                           nest = FALSE) {
  if (!is.null(survey_design)) {
    if (!inherits(survey_design, "survey.design")) {
      stop("survey_design must be a survey.design object.", call. = FALSE)
    }
    # A pre-built design already carries its own weights and clustering, so
    # anything passed via `weights`/`ids`/`strata`/`fpc` cannot be honoured. This
    # used to be discarded SILENTLY: a caller supplying both got results from the
    # design's weights while believing their own had been applied.
    #
    # This ERRORS rather than warns, deliberately. There is no coherent reason to
    # supply a complete design and separate components at the same time -- it is
    # always a mistake about which one is in force. In a statistical package the
    # cost of guessing wrong is a wrong published number, and a warning emitted
    # halfway through a long script is easily missed. Fail loudly instead.
    conflicting <- c(
      if (!is.null(weights)) "weights",
      if (!is.null(ids))     "ids",
      if (!is.null(strata))  "strata",
      if (!is.null(fpc))     "fpc"
    )
    if (length(conflicting) > 0) {
      stop(
        "Both `survey_design` and ", paste(conflicting, collapse = "`, `"),
        " were supplied. A pre-built design already carries its own weights and ",
        "clustering, so these cannot be applied and it is ambiguous which you ",
        "intended. Pass EITHER a pre-built `survey_design` OR the components ",
        "(`weights`/`ids`/`strata`/`fpc`), not both.",
        call. = FALSE
      )
    }
    return(survey_design)
  }

  has_survey_params <- !is.null(ids) || !is.null(strata) || !is.null(fpc)
  if (!has_survey_params && is.null(weights)) return(NULL)

  ids_formula <- if (!is.null(ids)) to_formula(ids) else ~1
  strata_formula <- if (!is.null(strata)) to_formula(strata) else NULL
  fpc_formula <- if (!is.null(fpc)) to_formula(fpc) else NULL
  weights_formula <- if (!is.null(weights)) {
    resolve_weights(weights, data)
  } else {
    NULL
  }

  # BUGFIX: resolve_weights() returns only a formula. When `weights` is a numeric
  # vector it names a column `.zelig2_weights`, but R's copy-on-modify semantics
  # mean that column was added to resolve_weights()'s OWN copy of `data` and is
  # gone by the time we get here. svydesign() then fails with
  # "object '.zelig2_weights' not found". Attach it to the data we actually pass.
  # Only bites the survey-design path (ids/strata/fpc); weights-only works because
  # that path hands the vector straight to svyglm instead of building a design.
  if (is.numeric(weights)) {
    data[[".zelig2_weights"]] <- weights
  }

  args <- list(ids = ids_formula, data = data, nest = nest)
  if (!is.null(strata_formula)) args$strata <- strata_formula
  if (!is.null(fpc_formula)) args$fpc <- fpc_formula
  if (!is.null(weights_formula)) args$weights <- weights_formula

  do.call(survey::svydesign, args)
}

#' Resolve weights to a formula
#'
#' @param weights Weight specification (formula, string, or numeric vector).
#' @param data A data frame.
#' @return A one-sided formula.
#' @keywords internal
resolve_weights <- function(weights, data) {
  if (inherits(weights, "formula")) return(weights)
  if (is.character(weights) && length(weights) == 1) {
    if (!(weights %in% names(data))) {
      stop(sprintf("Weight column '%s' not found in data.", weights),
           call. = FALSE)
    }
    return(stats::as.formula(paste0("~", weights)))
  }
  if (is.numeric(weights)) {
    col_name <- ".zelig2_weights"
    data[[col_name]] <- weights
    return(stats::as.formula(paste0("~", col_name)))
  }
  stop("weights must be a numeric vector, formula, or column-name string.",
       call. = FALSE)
}

#' Convert to a one-sided formula
#' @param x A formula or character string.
#' @return A one-sided formula.
#' @keywords internal
to_formula <- function(x) {
  if (inherits(x, "formula")) return(x)
  if (is.character(x) && length(x) == 1) {
    return(stats::as.formula(paste0("~", x)))
  }
  x
}
