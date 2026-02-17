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
