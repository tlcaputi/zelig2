#' Print a zelig2 Object
#'
#' Displays a concise overview of a fitted \code{zelig2} model: model type,
#' formula, sample size, standard-error type, and coefficients.
#'
#' @param x A \code{zelig2} object.
#' @param ... Additional arguments (currently unused).
#' @return Invisibly returns \code{x}.
#'
#' @examples
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
#' print(z)
#'
#' @export
print.zelig2 <- function(x, ...) {
  cat("zelig2: ", x$model_spec$description, "\n")
  cat("Formula: ", deparse(x$formula), "\n")
  if (!is.null(x$full_formula)) {
    cat("Full formula: ", deparse(x$full_formula), "\n")
  }
  cat("N: ", nrow(x$data), "\n")
  if (isTRUE(x$is_survey)) cat("Survey-weighted: yes\n")
  if (isTRUE(x$is_fixest)) {
    cat("Fixed effects: ", paste(x$fe_names, collapse = ", "), "\n")
  }
  if (x$vcov_type != "default") cat("Vcov type: ", x$vcov_type, "\n")
  cat("\nCoefficients:\n")
  print(x$coef)
  if (!is.null(x$sim_out)) {
    cat("\nSimulation results available (", x$sim_out$num, " draws)\n")
  }
  invisible(x)
}

#' Summarise a zelig2 Object
#'
#' Prints a detailed summary including a coefficient table (with standard
#' errors, z-values, and p-values derived from the stored variance--covariance
#' matrix), and, if simulations have been run, a summary of the quantities of
#' interest.
#'
#' @param object A \code{zelig2} object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns a list with components:
#'   \describe{
#'     \item{coef_table}{Matrix with columns \code{Estimate},
#'       \code{Std. Error}, \code{z value}, \code{Pr(>|z|)}.}
#'     \item{sim_out}{The simulation results (if available).}
#'   }
#'
#' @examples
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars) |>
#'   setx(hp = 150, wt = 3) |>
#'   sim()
#' summary(z)
#'
#' @export
summary.zelig2 <- function(object, ...) {
  coefs <- object$coef
  V <- object$vcov
  se <- sqrt(diag(V))
  z_val <- coefs / se
  p_val <- 2 * stats::pnorm(-abs(z_val))

  coef_table <- cbind(
    Estimate   = coefs,
    `Std. Error` = se,
    `z value`  = z_val,
    `Pr(>|z|)` = p_val
  )

  cat("zelig2: ", object$model_spec$description, "\n")
  cat("Formula: ", deparse(object$formula), "\n")
  if (!is.null(object$full_formula)) {
    cat("Full formula: ", deparse(object$full_formula), "\n")
  }
  cat("N: ", nrow(object$data), "\n")
  if (isTRUE(object$is_survey)) cat("Survey-weighted: yes\n")
  if (isTRUE(object$is_fixest)) {
    cat("Fixed effects: ", paste(object$fe_names, collapse = ", "), "\n")
  }
  if (object$vcov_type != "default") cat("Vcov type: ", object$vcov_type, "\n")
  cat("\nCoefficients:\n")
  stats::printCoefmat(coef_table, P.values = TRUE, has.Pvalue = TRUE)

  if (!is.null(object$sim_out)) {
    sim <- object$sim_out
    cat("\n--- Simulation Summary (", sim$num, " draws) ---\n")
    if (is.matrix(sim$ev)) {
      cat("Expected Values (range):\n")
      ev_summary <- apply(sim$ev, 2, function(col) {
        c(mean = mean(col), sd = stats::sd(col),
          `2.5%` = stats::quantile(col, 0.025),
          `97.5%` = stats::quantile(col, 0.975))
      })
      print(round(t(ev_summary), 4))
    } else {
      cat("Expected Values:\n")
      cat(sprintf("  Mean: %.4f  SD: %.4f  [%.4f, %.4f]\n",
                  mean(sim$ev), stats::sd(sim$ev),
                  stats::quantile(sim$ev, 0.025),
                  stats::quantile(sim$ev, 0.975)))
    }
    if (!is.null(sim$fd)) {
      cat("First Differences:\n")
      if (is.matrix(sim$fd)) {
        fd_summary <- apply(sim$fd, 2, function(col) {
          c(mean = mean(col), sd = stats::sd(col),
            `2.5%` = stats::quantile(col, 0.025),
            `97.5%` = stats::quantile(col, 0.975))
        })
        print(round(t(fd_summary), 4))
      } else {
        cat(sprintf("  Mean: %.4f  SD: %.4f  [%.4f, %.4f]\n",
                    mean(sim$fd), stats::sd(sim$fd),
                    stats::quantile(sim$fd, 0.025),
                    stats::quantile(sim$fd, 0.975)))
      }
    }
  }

  invisible(list(coef_table = coef_table, sim_out = object$sim_out))
}

#' Extract Coefficients
#'
#' Returns the estimated coefficients from a \code{zelig2} model. For models
#' with fixed effects, only the slope coefficients are returned (not the
#' fixed-effect intercepts; see \code{\link[fixest]{fixef}} for those).
#'
#' @param object A \code{zelig2} object.
#' @param ... Additional arguments (currently unused).
#' @return A named numeric vector of coefficients.
#'
#' @examples
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
#' coef(z)
#'
#' @export
coef.zelig2 <- function(object, ...) {
  object$coef
}

#' Extract the Variance--Covariance Matrix
#'
#' Returns the variance--covariance matrix of the estimated coefficients,
#' using whatever estimator was specified via \code{vcov_type} in
#' \code{\link{zelig2}}.
#'
#' @param object A \code{zelig2} object.
#' @param ... Additional arguments (currently unused).
#' @return A square numeric matrix.
#'
#' @examples
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars,
#'             vcov_type = "robust")
#' vcov(z)
#'
#' @export
vcov.zelig2 <- function(object, ...) {
  object$vcov
}
