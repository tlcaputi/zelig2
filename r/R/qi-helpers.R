#' Quantity of Interest Helper Functions
#'
#' Low-level functions for drawing parameters from their sampling distribution
#' and computing expected values (EV) and predicted values (PV) for each
#' model family.
#'
#' @name qi-helpers
#' @keywords internal
NULL

#' Draw parameters from a multivariate normal distribution
#'
#' Draws \code{num} parameter vectors from
#' \eqn{\mathcal{N}(\hat{\beta}, \hat{V})}{N(beta-hat, V-hat)}.
#'
#' @param coefs Named numeric vector of estimated coefficients.
#' @param V Variance--covariance matrix of the coefficients.
#' @param num Integer number of draws.
#' @return A \code{num x length(coefs)} matrix; each row is one draw.
#' @keywords internal
param_fn_mvn <- function(coefs, V, num) {
  MASS::mvrnorm(n = num, mu = coefs, Sigma = V)
}

#' QI function for continuous outcomes
#'
#' Computes expected and predicted values for models with continuous outcomes
#' (ls, gamma). Expected values are the inverse-link-transformed linear
#' predictor; predicted values add observation-level noise from the fitted
#' error distribution.
#'
#' @param params Matrix of parameter draws (\code{num x p}).
#' @param x_row Numeric vector of length \code{p}: one row of the model matrix.
#' @param category Character string model category.
#' @param fit The fitted model object.
#' @param link_inv Inverse link function (default \code{identity}).
#' @param fe_offset Scalar fixed-effect contribution to add to the linear
#'   predictor (default 0).
#' @return A list with numeric vectors \code{ev} (expected values) and
#'   \code{pv} (predicted values), each of length \code{num}.
#' @keywords internal
qi_fn_continuous <- function(params, x_row, category, fit,
                             link_inv = identity, fe_offset = 0) {
  eta <- as.numeric(params %*% x_row) + fe_offset
  ev <- link_inv(eta)

  if (inherits(fit, "svyglm")) {
    sigma <- sqrt(summary(fit)$dispersion)
  } else if (!is.null(fit$family) && fit$family$family == "Gamma") {
    sigma <- NULL
  } else if (inherits(fit, "fixest")) {
    sigma <- sqrt(summary(fit)$sigma2 %||% stats::var(stats::residuals(fit)))
  } else {
    sigma <- summary(fit)$sigma %||% sqrt(summary(fit)$dispersion)
  }

  if (!is.null(fit$family) && fit$family$family == "Gamma") {
    disp <- summary(fit)$dispersion
    pv <- stats::rgamma(length(ev), shape = 1 / disp, scale = ev * disp)
  } else {
    pv <- stats::rnorm(length(ev), mean = ev, sd = sigma)
  }
  list(ev = ev, pv = pv)
}

#' QI function for binary choice models
#'
#' Computes expected values (predicted probabilities) and predicted values
#' (Bernoulli draws) for logit and probit models.
#'
#' @inheritParams qi_fn_continuous
#' @param link_inv Inverse link function (logit or probit).
#' @return A list with \code{ev} (probabilities between 0 and 1) and
#'   \code{pv} (binary 0/1 draws).
#' @keywords internal
qi_fn_binchoice <- function(params, x_row, category, fit,
                            link_inv, fe_offset = 0) {
  eta <- as.numeric(params %*% x_row) + fe_offset
  ev <- link_inv(eta)
  pv <- stats::rbinom(length(ev), size = 1, prob = ev)
  list(ev = ev, pv = pv)
}

#' QI function for count models
#'
#' Computes expected values (predicted counts) and predicted values (Poisson
#' or negative-binomial draws) for count data models.
#'
#' @inheritParams qi_fn_continuous
#' @param link_inv Inverse link function (log link inverse = \code{exp}).
#' @return A list with \code{ev} (expected counts) and \code{pv} (integer
#'   count draws).
#' @keywords internal
qi_fn_count <- function(params, x_row, category, fit,
                        link_inv, fe_offset = 0) {
  eta <- as.numeric(params %*% x_row) + fe_offset
  ev <- link_inv(eta)

  is_negbin <- inherits(fit, "negbin") ||
    inherits(fit, "fixest_nl") ||
    (is.list(fit$family) && !is.null(fit$family$family) &&
     fit$family$family == "Negative Binomial") ||
    (is.character(fit$family) && grepl("negbin", fit$family, ignore.case = TRUE))

  if (is_negbin) {
    theta <- fit$theta %||%
      (if (!is.null(fit$NP_origin)) fit$NP_origin else NULL) %||%
      fit$call$init.theta %||% 1
    pv <- stats::rnbinom(length(ev), size = theta, mu = ev)
  } else {
    pv <- stats::rpois(length(ev), lambda = ev)
  }
  list(ev = ev, pv = pv)
}

#' QI function for tobit models
#'
#' Computes expected and predicted values for censored (tobit) regression.
#' Predicted values are left-censored at zero.
#'
#' @inheritParams qi_fn_continuous
#' @return A list with \code{ev} (latent expected values) and \code{pv}
#'   (predicted values censored at 0).
#' @keywords internal
qi_fn_tobit <- function(params, x_row, category, fit,
                        link_inv = identity, fe_offset = 0) {
  n_coef <- length(x_row)
  beta_draws <- params[, seq_len(n_coef), drop = FALSE]
  eta <- as.numeric(beta_draws %*% x_row) + fe_offset
  ev <- eta
  sigma <- fit$scale
  pv <- stats::rnorm(length(ev), mean = ev, sd = sigma)
  pv <- pmax(pv, 0)
  list(ev = ev, pv = pv)
}

#' QI function for quantile regression
#'
#' Computes expected values (conditional quantile at the specified tau) and
#' predicted values (same as expected values for quantile regression, since
#' there is no stochastic prediction component).
#'
#' @inheritParams qi_fn_continuous
#' @return A list with \code{ev} and \code{pv} (both the conditional quantile).
#' @keywords internal
qi_fn_quantile <- function(params, x_row, category, fit,
                           link_inv = identity, fe_offset = 0) {
  eta <- as.numeric(params %*% x_row) + fe_offset
  ev <- eta
  pv <- ev
  list(ev = ev, pv = pv)
}
