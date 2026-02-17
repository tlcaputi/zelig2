#' Estimate a Statistical Model
#'
#' The main entry point for the \pkg{zelig2} workflow. Fits a statistical model
#' and stores all metadata needed for counterfactual analysis via
#' \code{\link{setx}}, \code{\link{sim}}, and \code{\link{plot.zelig2}}.
#'
#' \code{zelig2} supports eight model families (see \code{\link{list_models}})
#' and automatically handles:
#' \itemize{
#'   \item Character-to-factor conversion for categorical predictors.
#'   \item Survey weights (via \pkg{survey}) with no need for separate model
#'     types.
#'   \item Fixed effects (via \pkg{fixest}) when the formula contains
#'     \code{|} or the \code{fixef} argument is used.
#'   \item Robust, clustered, and bootstrap standard errors (via
#'     \pkg{sandwich}).
#' }
#'
#' @param formula A model formula. For fixed effects, use the \pkg{fixest}
#'   pipe syntax (e.g., \code{y ~ x1 + x2 | fe1 + fe2}).
#' @param model Character string naming the model family. One of
#'   \code{"ls"}, \code{"logit"}, \code{"probit"}, \code{"poisson"},
#'   \code{"negbin"}, \code{"gamma"}, \code{"tobit"}, or \code{"quantile"}.
#'   See \code{\link{list_models}}.
#' @param data A data frame. Character columns are automatically converted to
#'   factors.
#' @param weights Optional frequency / analytic weights. Accepts a numeric
#'   vector, a one-sided formula, or a column-name string.
#' @param survey_design An optional \code{\link[survey]{svydesign}} object.
#'   If supplied, a survey-weighted model is fitted.
#' @param ids Cluster IDs for automatic survey-design construction (formula
#'   or string).
#' @param strata Strata for survey design (formula or string).
#' @param fpc Finite population correction (formula or string).
#' @param nest Logical; nest clusters within strata? Default \code{FALSE}.
#' @param fixef Optional fixed-effects specification. A one-sided formula
#'   (e.g., \code{~ state + year}) or a character vector of variable names.
#'   This is an alternative to including \code{|} in the main formula.
#' @param vcov_type Character string specifying the variance--covariance
#'   estimator. One of \code{"default"}, \code{"robust"} (alias for
#'   \code{"HC1"}), \code{"HC0"}--\code{"HC4"}, \code{"cluster"}, or
#'   \code{"bootstrap"}. See \code{\link[sandwich]{vcovHC}}.
#' @param cluster Cluster variable for cluster-robust SEs. Accepts a vector,
#'   a one-sided formula, or a column-name string.
#' @param bootstrap_n Integer; number of bootstrap replicates when
#'   \code{vcov_type = "bootstrap"} (default 500).
#' @param num Integer; number of Monte Carlo simulation draws used by
#'   \code{\link{sim}} (default 1000).
#' @param ... Additional arguments passed to the model-fitting function.
#'
#' @return An object of class \code{"zelig2"}, a named list containing:
#'   \describe{
#'     \item{formula}{The model formula (linear part if fixed effects used).}
#'     \item{model_name}{Character string model name.}
#'     \item{fit}{The underlying fitted model object.}
#'     \item{coef, vcov}{Coefficients and variance--covariance matrix.}
#'     \item{data}{The (possibly factorized) data frame.}
#'     \item{is_survey, is_fixest}{Logical flags.}
#'   }
#'
#' @seealso \code{\link{setx}} to set covariate scenarios,
#'   \code{\link{sim}} to simulate quantities of interest,
#'   \code{\link{plot.zelig2}} to visualise results,
#'   \code{\link{summary.zelig2}} for coefficient tables.
#'
#' @examples
#' # Linear regression
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
#' z <- setx(z, hp = 150, wt = 3)
#' z <- sim(z)
#' summary(z)
#'
#' # Logistic regression with robust SEs
#' z <- zelig2(vs ~ hp + wt, model = "logit", data = mtcars,
#'             vcov_type = "robust")
#'
#' # Fixed effects (requires fixest)
#' z <- zelig2(mpg ~ hp | cyl, model = "ls", data = mtcars)
#'
#' @export
zelig2 <- function(formula, model, data, weights = NULL,
                   survey_design = NULL, ids = NULL, strata = NULL,
                   fpc = NULL, nest = FALSE, fixef = NULL,
                   vcov_type = "default", cluster = NULL,
                   bootstrap_n = 500L, num = 1000L, ...) {

  # --- input validation -------------------------------------------------------
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula object.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  if (!is.character(model) || length(model) != 1) {
    stop("'model' must be a single character string.", call. = FALSE)
  }

  # --- preprocessing ----------------------------------------------------------
  data <- auto_factorize(data)

  # Determine if fixed effects are requested
  use_fixest <- has_fixed_effects(formula, fixef)

  # --- fixed-effects path (fixest) --------------------------------------------
  if (use_fixest) {
    parsed <- parse_fe_formula(formula, fixef)

    # Weights: add as column so fixest can reference by formula
    weights_col <- NULL
    if (is.numeric(weights)) {
      weights_col <- ".zelig2_wt"
      data[[weights_col]] <- weights
    }

    fit <- fit_fixest(model, parsed$full, data,
                      weights_col = weights_col, ...)

    spec <- get_model_spec(model)
    coefs <- stats::coef(fit)
    V <- compute_vcov(fit, vcov_type = vcov_type, cluster = cluster,
                      data = data)
    # Ensure vcov dimensions match coef (e.g., fenegbin includes .theta)
    if (!all(names(coefs) %in% rownames(V)) || nrow(V) != length(coefs)) {
      shared <- intersect(names(coefs), rownames(V))
      if (length(shared) == length(coefs)) {
        V <- V[shared, shared, drop = FALSE]
      } else {
        V <- V[seq_along(coefs), seq_along(coefs), drop = FALSE]
      }
    }
    cat_vars <- detect_categorical_vars(data, parsed$linear)

    obj <- list(
      formula       = parsed$linear,
      full_formula  = parsed$full,
      model_name    = model,
      model_spec    = spec,
      category      = spec$category,
      fit           = fit,
      data          = data,
      coef          = coefs,
      vcov          = V,
      num           = num,
      is_survey     = FALSE,
      is_fixest     = TRUE,
      fe_names      = parsed$fe_vars,
      survey_design = NULL,
      vcov_type     = vcov_type,
      cat_vars      = cat_vars,
      weights_col   = weights_col,
      scenario      = NULL,
      scenario1     = NULL,
      sim_out       = NULL
    )
    class(obj) <- "zelig2"
    return(obj)
  }

  # --- standard path (glm / survey / other) -----------------------------------
  weights_col <- NULL
  survey_weights <- weights
  if (is.numeric(weights) && is.null(survey_design) && is.null(ids)) {
    # Store weights in data and create a proper survey design with ids = ~1.
    # This ensures correct handling for all model families (e.g., binomial
    # models require probability weights via svyglm, not frequency weights).
    data[[".zelig2_wt"]] <- weights
    survey_weights <- ~.zelig2_wt
    weights_col <- NULL
  }

  svy_design <- resolve_survey(
    data = data, weights = survey_weights, survey_design = survey_design,
    ids = ids, strata = strata, fpc = fpc, nest = nest
  )
  is_survey <- !is.null(svy_design)

  spec <- get_model_spec(model)

  fit <- spec$fit_fn(formula, data = data, survey_design = svy_design,
                     weights_col = weights_col, ...)

  V <- compute_vcov(
    fit, vcov_type = vcov_type, cluster = cluster, data = data,
    bootstrap_n = bootstrap_n, is_survey = is_survey
  )

  coefs <- stats::coef(fit)
  cat_vars <- detect_categorical_vars(data, formula)

  obj <- list(
    formula       = formula,
    full_formula  = NULL,
    model_name    = model,
    model_spec    = spec,
    category      = spec$category,
    fit           = fit,
    data          = data,
    coef          = coefs,
    vcov          = V,
    num           = num,
    is_survey     = is_survey,
    is_fixest     = FALSE,
    fe_names      = NULL,
    survey_design = svy_design,
    vcov_type     = vcov_type,
    cat_vars      = cat_vars,
    weights_col   = weights_col,
    scenario      = NULL,
    scenario1     = NULL,
    sim_out       = NULL
  )
  class(obj) <- "zelig2"
  obj
}
