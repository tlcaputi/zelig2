#' Non-GLM Model Registrations
#'
#' Registers the \code{tobit} (censored regression via \pkg{AER}) and
#' \code{quantile} (quantile regression via \pkg{quantreg}) models in the
#' \pkg{zelig2} model registry.
#'
#' @name models-other
#' @keywords internal
NULL

#' Register non-GLM models
#' @keywords internal
register_other_models <- function() {

  # --- tobit (censored regression) -------------------------------------------
  register_model(
    name = "tobit",
    category = "continuous",
    description = "Tobit (Censored) Regression",
    fit_fn = function(formula, data, survey_design = NULL,
                      weights_col = NULL, ...) {
      if (!is.null(survey_design)) {
        warning("Survey weights are not directly supported for tobit; ",
                "ignoring survey_design.", call. = FALSE)
      }
      args <- list(formula = formula, data = data)
      if (!is.null(weights_col)) {
        args$weights <- data[[weights_col]]
      }
      args <- c(args, list(...))
      do.call(AER::tobit, args)
    },
    param_fn = function(coefs, V, num) {
      # Tobit vcov includes Log(scale) but coef() does not
      V_sub <- V[names(coefs), names(coefs), drop = FALSE]
      param_fn_mvn(coefs, V_sub, num)
    },
    qi_fn = function(params, x_row, category, fit, fe_offset = 0) {
      qi_fn_tobit(params, x_row, category, fit, fe_offset = fe_offset)
    }
  )

  # --- quantile (quantile regression) ----------------------------------------
  register_model(
    name = "quantile",
    category = "continuous",
    description = "Quantile Regression",
    fit_fn = function(formula, data, survey_design = NULL,
                      weights_col = NULL, tau = 0.5, ...) {
      if (!is.null(survey_design)) {
        warning("Survey weights are not directly supported for quantile ",
                "regression; ignoring survey_design.", call. = FALSE)
      }
      if (!is.null(weights_col)) {
        quantreg::rq(formula, data = data, tau = tau,
                     weights = data[[weights_col]], ...)
      } else {
        quantreg::rq(formula, data = data, tau = tau, ...)
      }
    },
    param_fn = function(coefs, V, num) {
      param_fn_mvn(coefs, V, num)
    },
    qi_fn = function(params, x_row, category, fit, fe_offset = 0) {
      qi_fn_quantile(params, x_row, category, fit, fe_offset = fe_offset)
    }
  )
}
