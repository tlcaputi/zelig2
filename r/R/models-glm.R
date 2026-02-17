#' GLM Model Registrations
#'
#' Registers the six core GLM-based model families in the \pkg{zelig2} model
#' registry: \code{ls} (OLS), \code{logit}, \code{probit}, \code{poisson},
#' \code{negbin}, and \code{gamma}.
#'
#' Each registration provides three functions:
#' \describe{
#'   \item{\code{fit_fn}}{Fits the model (dispatches to \code{glm},
#'     \code{MASS::glm.nb}, or \code{survey::svyglm} depending on context).}
#'   \item{\code{param_fn}}{Draws simulated parameter vectors
#'     (multivariate normal).}
#'   \item{\code{qi_fn}}{Computes expected and predicted values.}
#' }
#'
#' @name models-glm
#' @keywords internal
NULL

#' Fit a GLM model
#'
#' Branches on whether a survey design is supplied.
#'
#' @param formula Model formula.
#' @param data Data frame.
#' @param family GLM family object.
#' @param survey_design A \code{survey.design} object or \code{NULL}.
#' @param weights_col Weights column name or \code{NULL}.
#' @param ... Additional arguments passed to \code{\link[stats]{glm}} or
#'   \code{\link[survey]{svyglm}}.
#' @return A fitted model object.
#' @keywords internal
fit_glm <- function(formula, data, family, survey_design = NULL,
                    weights_col = NULL, ...) {
  if (!is.null(survey_design)) {
    survey::svyglm(formula, design = survey_design, family = family, ...)
  } else if (!is.null(weights_col)) {
    # Use do.call to force eager evaluation of weights (avoids NSE issues
    # where glm() captures the expression and evaluates in wrong env)
    glm_args <- list(formula = formula, data = data, family = family,
                     weights = data[[weights_col]])
    do.call(stats::glm, c(glm_args, list(...)))
  } else {
    stats::glm(formula, data = data, family = family, ...)
  }
}

#' Register all GLM-based models
#' @keywords internal
register_glm_models <- function() {

  # --- ls (OLS / linear regression) ------------------------------------------
  register_model(
    name = "ls",
    category = "continuous",
    description = "Least Squares (Linear Regression)",
    fit_fn = function(formula, data, survey_design = NULL,
                      weights_col = NULL, ...) {
      fit_glm(formula, data, family = stats::gaussian(),
              survey_design = survey_design, weights_col = weights_col, ...)
    },
    param_fn = param_fn_mvn,
    qi_fn = function(params, x_row, category, fit, fe_offset = 0) {
      qi_fn_continuous(params, x_row, category, fit,
                       link_inv = identity, fe_offset = fe_offset)
    }
  )

  # --- logit (logistic regression) -------------------------------------------
  register_model(
    name = "logit",
    category = "binary",
    description = "Logistic Regression",
    fit_fn = function(formula, data, survey_design = NULL,
                      weights_col = NULL, ...) {
      fit_glm(formula, data, family = stats::binomial(link = "logit"),
              survey_design = survey_design, weights_col = weights_col, ...)
    },
    param_fn = param_fn_mvn,
    qi_fn = function(params, x_row, category, fit, fe_offset = 0) {
      qi_fn_binchoice(params, x_row, category, fit,
                      link_inv = stats::binomial(link = "logit")$linkinv,
                      fe_offset = fe_offset)
    }
  )

  # --- probit ----------------------------------------------------------------
  register_model(
    name = "probit",
    category = "binary",
    description = "Probit Regression",
    fit_fn = function(formula, data, survey_design = NULL,
                      weights_col = NULL, ...) {
      fit_glm(formula, data, family = stats::binomial(link = "probit"),
              survey_design = survey_design, weights_col = weights_col, ...)
    },
    param_fn = param_fn_mvn,
    qi_fn = function(params, x_row, category, fit, fe_offset = 0) {
      qi_fn_binchoice(params, x_row, category, fit,
                      link_inv = stats::binomial(link = "probit")$linkinv,
                      fe_offset = fe_offset)
    }
  )

  # --- poisson ---------------------------------------------------------------
  register_model(
    name = "poisson",
    category = "count",
    description = "Poisson Regression",
    fit_fn = function(formula, data, survey_design = NULL,
                      weights_col = NULL, ...) {
      if (!is.null(survey_design)) {
        survey::svyglm(formula, design = survey_design,
                       family = stats::quasipoisson(), ...)
      } else if (!is.null(weights_col)) {
        glm_args <- list(formula = formula, data = data,
                         family = stats::poisson(),
                         weights = data[[weights_col]])
        do.call(stats::glm, c(glm_args, list(...)))
      } else {
        stats::glm(formula, data = data, family = stats::poisson(), ...)
      }
    },
    param_fn = param_fn_mvn,
    qi_fn = function(params, x_row, category, fit, fe_offset = 0) {
      qi_fn_count(params, x_row, category, fit,
                  link_inv = stats::poisson()$linkinv,
                  fe_offset = fe_offset)
    }
  )

  # --- negbin (negative binomial) --------------------------------------------
  register_model(
    name = "negbin",
    category = "count",
    description = "Negative Binomial Regression",
    fit_fn = function(formula, data, survey_design = NULL,
                      weights_col = NULL, ...) {
      if (!is.null(survey_design)) {
        survey::svyglm(formula, design = survey_design,
                       family = stats::quasipoisson(), ...)
      } else if (!is.null(weights_col)) {
        glm_args <- list(formula = formula, data = data,
                         weights = data[[weights_col]])
        do.call(MASS::glm.nb, c(glm_args, list(...)))
      } else {
        MASS::glm.nb(formula, data = data, ...)
      }
    },
    param_fn = param_fn_mvn,
    qi_fn = function(params, x_row, category, fit, fe_offset = 0) {
      qi_fn_count(params, x_row, category, fit,
                  link_inv = stats::poisson()$linkinv,
                  fe_offset = fe_offset)
    }
  )

  # --- gamma -----------------------------------------------------------------
  register_model(
    name = "gamma",
    category = "continuous",
    description = "Gamma Regression",
    fit_fn = function(formula, data, survey_design = NULL,
                      weights_col = NULL, ...) {
      if (!is.null(survey_design)) {
        survey::svyglm(formula, design = survey_design,
                       family = stats::Gamma(link = "inverse"), ...)
      } else if (!is.null(weights_col)) {
        glm_args <- list(formula = formula, data = data,
                         family = stats::Gamma(link = "inverse"),
                         weights = data[[weights_col]])
        do.call(stats::glm, c(glm_args, list(...)))
      } else {
        stats::glm(formula, data = data,
                   family = stats::Gamma(link = "inverse"), ...)
      }
    },
    param_fn = param_fn_mvn,
    qi_fn = function(params, x_row, category, fit, fe_offset = 0) {
      qi_fn_continuous(params, x_row, category, fit,
                       link_inv = stats::Gamma(link = "inverse")$linkinv,
                       fe_offset = fe_offset)
    }
  )
}
