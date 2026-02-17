#' Simulate Quantities of Interest
#'
#' Draws parameter vectors from their estimated sampling distribution
#' (\eqn{\mathcal{N}(\hat{\beta}, \hat{V})}{N(beta-hat, V-hat)}) and computes
#' model-specific quantities of interest (expected values, predicted values,
#' and, when a contrast scenario is set, first differences and risk ratios).
#'
#' The simulation is performed at the covariate scenario(s) stored by
#' \code{\link{setx}} (and optionally \code{\link{setx1}}).
#'
#' @param object A \code{zelig2} object with at least one scenario set via
#'   \code{\link{setx}}.
#' @param num Integer number of simulation draws. If \code{NULL} (the
#'   default), uses the value stored during the \code{\link{zelig2}} call.
#' @param ... Currently unused.
#'
#' @return A new \code{zelig2} object with simulation results in
#'   \code{object$sim_out}, a list containing:
#'   \describe{
#'     \item{ev}{Expected values at the primary scenario.}
#'     \item{pv}{Predicted values at the primary scenario.}
#'     \item{ev1}{Expected values at the contrast scenario (if set).}
#'     \item{pv1}{Predicted values at the contrast scenario (if set).}
#'     \item{fd}{First differences (\code{ev1 - ev}), if applicable.}
#'     \item{rr}{Risk ratios (\code{ev1 / ev}), for binary / count models.}
#'     \item{num}{Number of simulation draws used.}
#'   }
#'
#' @seealso \code{\link{setx}}, \code{\link{plot.zelig2}},
#'   \code{\link{summary.zelig2}}.
#'
#' @examples
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars) |>
#'   setx(hp = 150, wt = 3) |>
#'   sim()
#' summary(z)
#'
#' @export
sim <- function(object, num = NULL, ...) {
  if (!inherits(object, "zelig2")) {
    stop("object must be a zelig2 object.", call. = FALSE)
  }
  if (is.null(object$scenario)) {
    stop("No scenario set. Call setx() before sim().", call. = FALSE)
  }

  num <- num %||% object$num
  spec <- object$model_spec

  params <- spec$param_fn(object$coef, object$vcov, num)

  qi0 <- simulate_at(params, object$scenario, spec, object)

  qi1 <- NULL
  fd <- NULL
  rr <- NULL
  if (!is.null(object$scenario1)) {
    qi1 <- simulate_at(params, object$scenario1, spec, object)
    fd <- qi1$ev - qi0$ev
    if (object$category %in% c("binary", "count")) {
      rr <- qi1$ev / qi0$ev
    }
  }

  sim_result <- list(
    ev  = qi0$ev,
    pv  = qi0$pv,
    ev1 = if (!is.null(qi1)) qi1$ev else NULL,
    pv1 = if (!is.null(qi1)) qi1$pv else NULL,
    fd  = fd,
    rr  = rr,
    num = num,
    scenario  = object$scenario,
    scenario1 = object$scenario1
  )

  new_obj <- object
  new_obj$sim_out <- sim_result
  class(new_obj) <- "zelig2"
  new_obj
}

#' Simulate at a single scenario
#'
#' Internal function that evaluates the model-specific QI function at each
#' row of the scenario's model matrix.
#'
#' @param params Parameter-draw matrix (\code{num x p}).
#' @param scenario Scenario list from \code{\link{build_scenario}}.
#' @param spec Model specification list.
#' @param object The \code{zelig2} object.
#' @return A list with \code{ev} and \code{pv}. For range scenarios these
#'   are matrices (\code{num x n_range_vals}).
#' @keywords internal
simulate_at <- function(params, scenario, spec, object) {
  x_mat <- scenario$x_matrix
  n_rows <- nrow(x_mat)
  fe_offset <- scenario$fe_contribution %||% 0

  if (n_rows == 1) {
    x_row <- as.numeric(x_mat[1, ])
    return(spec$qi_fn(params, x_row, spec$category, object$fit,
                      fe_offset = fe_offset))
  }

  ev_mat <- matrix(NA_real_, nrow = nrow(params), ncol = n_rows)
  pv_mat <- matrix(NA_real_, nrow = nrow(params), ncol = n_rows)
  for (i in seq_len(n_rows)) {
    x_row <- as.numeric(x_mat[i, ])
    qi <- spec$qi_fn(params, x_row, spec$category, object$fit,
                     fe_offset = fe_offset)
    ev_mat[, i] <- qi$ev
    pv_mat[, i] <- qi$pv
  }
  colnames(ev_mat) <- scenario$range_vals
  colnames(pv_mat) <- scenario$range_vals
  list(ev = ev_mat, pv = pv_mat)
}
