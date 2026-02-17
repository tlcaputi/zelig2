#' Variance--Covariance Matrix Computation
#'
#' Computes the variance--covariance matrix of estimated coefficients using
#' one of several estimators. Dispatches to \pkg{sandwich} for
#' heteroskedasticity-robust, cluster-robust, and bootstrap estimators, to
#' \pkg{survey} for survey-weighted models, and to \pkg{fixest} for
#' fixed-effect models.
#'
#' @name vcov-compute
#' @keywords internal
NULL

#' Compute the variance--covariance matrix
#'
#' @param fit A fitted model object (class \code{glm}, \code{negbin},
#'   \code{svyglm}, \code{survreg}, \code{rq}, or \code{fixest}).
#' @param vcov_type Character string specifying the estimator:
#'   \describe{
#'     \item{\code{"default"}}{Model-based (OLS or MLE) standard errors.
#'       For survey models, returns the design-based vcov. For fixest models,
#'       returns the fixest default (clustered by the first FE if present).}
#'     \item{\code{"robust"}, \code{"HC1"}}{Heteroskedasticity-consistent
#'       (White / Eicker--Huber--White) SEs.}
#'     \item{\code{"HC0"}, \code{"HC2"}, \code{"HC3"}, \code{"HC4"}}{Other
#'       HC variants (see \code{\link[sandwich]{vcovHC}}).}
#'     \item{\code{"cluster"}}{Cluster-robust SEs (requires \code{cluster}).}
#'     \item{\code{"bootstrap"}}{Bootstrap SEs (uses \code{bootstrap_n}
#'       replicates).}
#'   }
#' @param cluster Cluster identifier: a vector, a one-sided formula, or a
#'   column-name string.
#' @param data The data frame used for fitting.
#' @param bootstrap_n Integer number of bootstrap replicates (default 500).
#' @param is_survey Logical; is this a survey-weighted model?
#' @return A square numeric matrix (the variance--covariance matrix).
#' @keywords internal
compute_vcov <- function(fit, vcov_type = "default", cluster = NULL,
                         data = NULL, bootstrap_n = 500L,
                         is_survey = FALSE) {

  # --- fixest models ---
  if (inherits(fit, "fixest")) {
    return(compute_vcov_fixest(fit, vcov_type, cluster))
  }

  # --- quantreg models ---
  if (inherits(fit, "rq")) {
    if (vcov_type == "default" || is_survey) {
      s <- quantreg::summary.rq(fit, covariance = TRUE)
      return(s$cov)
    }
  }

  # --- survey or default ---
  if (is_survey || vcov_type == "default") {
    return(stats::vcov(fit))
  }

  # --- robust (HC) SEs via sandwich ---
  if (vcov_type %in% c("HC0", "HC1", "HC2", "HC3", "HC4", "robust")) {
    hc_type <- if (vcov_type == "robust") "HC1" else vcov_type
    return(sandwich::vcovHC(fit, type = hc_type))
  }

  # --- cluster-robust SEs ---
  if (vcov_type == "cluster") {
    if (is.null(cluster)) {
      stop("vcov_type = 'cluster' requires the 'cluster' argument.",
           call. = FALSE)
    }
    if (inherits(cluster, "formula")) {
      cluster_var <- stats::model.frame(cluster, data = data)[[1]]
    } else if (is.character(cluster) && length(cluster) == 1) {
      cluster_var <- data[[cluster]]
    } else {
      cluster_var <- cluster
    }
    return(sandwich::vcovCL(fit, cluster = cluster_var))
  }

  # --- bootstrap SEs ---
  if (vcov_type == "bootstrap") {
    return(sandwich::vcovBS(fit, R = bootstrap_n))
  }

  stop(sprintf("Unknown vcov_type: '%s'.", vcov_type), call. = FALSE)
}

#' Compute vcov for fixest models
#'
#' Uses the \pkg{fixest} variance--covariance infrastructure.
#'
#' @param fit A fitted \code{fixest} object.
#' @param vcov_type Character string (see \code{\link{compute_vcov}}).
#' @param cluster Cluster specification.
#' @return A variance--covariance matrix.
#' @keywords internal
compute_vcov_fixest <- function(fit, vcov_type, cluster) {
  if (vcov_type == "default") {
    return(stats::vcov(fit))
  }

  if (vcov_type %in% c("robust", "HC0", "HC1", "HC2", "HC3", "HC4")) {
    s <- summary(fit, vcov = "hetero")
    return(stats::vcov(s))
  }

  if (vcov_type == "cluster") {
    if (!is.null(cluster)) {
      cl <- if (inherits(cluster, "formula")) {
        cluster
      } else if (is.character(cluster) && length(cluster) == 1) {
        stats::as.formula(paste0("~", cluster))
      } else {
        stop("For fixest models, 'cluster' must be a formula or column name.",
             call. = FALSE)
      }
      s <- summary(fit, vcov = cl)
    } else {
      s <- summary(fit, vcov = "cluster")
    }
    return(stats::vcov(s))
  }

  if (vcov_type == "bootstrap") {
    warning("Bootstrap SEs are not supported for fixest models; ",
            "returning default vcov.", call. = FALSE)
    return(stats::vcov(fit))
  }

  stats::vcov(fit)
}
