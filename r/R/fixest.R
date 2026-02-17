#' Fixed Effects Support via fixest
#'
#' Internal utilities for detecting and fitting models with fixed effects
#' using the \pkg{fixest} package. When a formula contains the \code{|}
#' operator (e.g., \code{y ~ x1 + x2 | fe1 + fe2}) or when the \code{fixef}
#' argument is supplied, \code{zelig2()} automatically routes fitting to the
#' appropriate \pkg{fixest} function.
#'
#' @name fixest-support
#' @keywords internal
NULL

#' Check whether a formula contains fixed effects
#'
#' @param formula A model formula.
#' @param fixef Optional fixed-effects specification.
#' @return Logical.
#' @keywords internal
has_fixed_effects <- function(formula, fixef = NULL) {
  if (!is.null(fixef)) return(TRUE)
  f_str <- paste(deparse(formula, width.cutoff = 500), collapse = "")
  grepl("\\|", f_str)
}

#' Parse a formula into linear and fixed-effect parts
#'
#' @param formula A model formula, optionally containing \code{|}.
#' @param fixef Optional fixed-effects specification: a one-sided formula
#'   (e.g., \code{~ state + year}) or a character vector of variable names.
#' @return A named list with components:
#'   \describe{
#'     \item{full}{The complete formula with FE (for fixest fitting).}
#'     \item{linear}{The linear part of the formula (without FE).}
#'     \item{fe_vars}{Character vector of FE variable names.}
#'   }
#' @keywords internal
parse_fe_formula <- function(formula, fixef = NULL) {
  if (!is.null(fixef)) {
    if (inherits(fixef, "formula")) {
      fe_str <- deparse(fixef[[2]], width.cutoff = 500)
    } else if (is.character(fixef)) {
      fe_str <- paste(fixef, collapse = " + ")
    } else {
      stop("fixef must be a one-sided formula or character vector.", call. = FALSE)
    }
    linear <- formula
    full_str <- paste(deparse(formula, width.cutoff = 500), collapse = "")
    full_formula <- stats::as.formula(paste(full_str, "|", fe_str))
    fe_vars <- trimws(strsplit(fe_str, "\\+")[[1]])
    return(list(full = full_formula, linear = linear, fe_vars = fe_vars))
  }

  f_str <- paste(deparse(formula, width.cutoff = 500), collapse = "")
  parts <- strsplit(f_str, "\\|")[[1]]
  linear <- stats::as.formula(trimws(parts[1]))
  fe_str <- trimws(parts[2])
  fe_vars <- trimws(strsplit(fe_str, "\\+")[[1]])
  list(full = stats::as.formula(f_str), linear = linear, fe_vars = fe_vars)
}

#' Fit a model with fixed effects using fixest
#'
#' Maps a zelig2 model name to the corresponding \pkg{fixest} function and
#' fits the model.
#'
#' @param model_name Character string model name (e.g., "ls", "logit").
#' @param formula The full formula including FE (with \code{|}).
#' @param data A data frame.
#' @param weights_col Optional column name containing weights.
#' @param ... Additional arguments passed to the fixest fitting function.
#' @return A fitted \code{fixest} object.
#' @keywords internal
fit_fixest <- function(model_name, formula, data, weights_col = NULL, ...) {
  wt_arg <- if (!is.null(weights_col)) {
    stats::as.formula(paste0("~", weights_col))
  } else {
    NULL
  }

  fit <- switch(model_name,
    "ls" = fixest::feols(formula, data = data, weights = wt_arg, ...),
    "logit" = fixest::feglm(formula, data = data,
                             family = stats::binomial(link = "logit"),
                             weights = wt_arg, ...),
    "probit" = fixest::feglm(formula, data = data,
                              family = stats::binomial(link = "probit"),
                              weights = wt_arg, ...),
    "poisson" = fixest::feglm(formula, data = data,
                               family = stats::poisson(),
                               weights = wt_arg, ...),
    "negbin" = fixest::fenegbin(formula, data = data, ...),
    "gamma" = fixest::feglm(formula, data = data,
                             family = stats::Gamma(link = "inverse"),
                             weights = wt_arg, ...),
    stop(sprintf(
      "Fixed effects not supported for model '%s'. Supported: ls, logit, probit, poisson, negbin, gamma.",
      model_name
    ), call. = FALSE)
  )
  fit
}

#' Compute the fixed-effect contribution for a scenario
#'
#' Looks up the estimated fixed-effect intercepts from a fitted \pkg{fixest}
#' model and sums the contributions for the requested levels. For any FE
#' dimension not specified in \code{fe_vals}, the mean across all levels is
#' used.
#'
#' @param fit A fitted \code{fixest} object.
#' @param fe_vals A named list mapping FE variable names to level values
#'   (e.g., \code{list(state = "CA", year = "2020")}). If \code{NULL}, all
#'   FE dimensions are averaged.
#' @return A scalar: the total fixed-effect contribution to the linear
#'   predictor.
#' @keywords internal
compute_fe_contribution <- function(fit, fe_vals = NULL) {
  if (!inherits(fit, "fixest")) return(0)

  fe_list <- fixest::fixef(fit)
  if (is.null(fe_list) || length(fe_list) == 0) return(0)

  contribution <- 0
  for (fe_name in names(fe_list)) {
    fe_vec <- fe_list[[fe_name]]
    if (!is.null(fe_vals) && fe_name %in% names(fe_vals)) {
      level <- as.character(fe_vals[[fe_name]])
      if (level %in% names(fe_vec)) {
        contribution <- contribution + fe_vec[level]
      } else {
        warning(sprintf(
          "FE level '%s' not found for '%s'; using mean across levels.",
          level, fe_name
        ), call. = FALSE)
        contribution <- contribution + mean(fe_vec)
      }
    } else {
      contribution <- contribution + mean(fe_vec)
    }
  }
  contribution
}
