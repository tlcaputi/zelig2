#' Set Covariate Values for Counterfactual Scenarios
#'
#' After fitting a model with \code{\link{zelig2}}, use \code{setx()} to
#' specify the covariate values at which quantities of interest will be
#' computed. Covariates not explicitly set default to their sample median
#' (numeric) or mode (factor / character).
#'
#' For \strong{range scenarios}, pass a vector of values for one covariate
#' (e.g., \code{x1 = seq(0, 10, by = 1)}). All other covariates are held at
#' their defaults. Only one variable may vary at a time.
#'
#' For models with \strong{fixed effects}, specify FE levels by name
#' (e.g., \code{state = "CA"}). Unspecified FE dimensions default to the
#' mean of their estimated intercepts.
#'
#' @param object A \code{zelig2} object produced by \code{\link{zelig2}}.
#' @param ... Named covariate values. Scalars set a point scenario; a vector
#'   creates a range scenario.
#' @param fn Optional character string naming a function (e.g., \code{"mean"})
#'   to use as the default for unspecified numeric covariates.
#' @param factor_default How to set unspecified \emph{factor} (or character)
#'   covariates. \code{"mode"} (the default) sets each unspecified factor to
#'   its modal level — i.e., the counterfactual person belongs to a single,
#'   most-common category. \code{"mean"} replaces each unspecified factor
#'   variable's dummy columns with their column means in the fitted model
#'   matrix, so the counterfactual person represents the average composition
#'   across categories rather than a single modal level. When the fit has
#'   non-trivial observation weights (e.g., a survey-weighted \code{svyglm}
#'   fit), the column means under \code{"mean"} are computed as
#'   \emph{weighted} means using those weights, so the counterfactual
#'   represents the \emph{population} the survey is designed to estimate.
#'   For unweighted fits the means are ordinary (unweighted) column means.
#'   The \code{"mean"} option matches the convention used by manual
#'   implementations of King et al. (2000) simulation and is appropriate when
#'   the desired quantity is a population-average effect rather than an
#'   effect at the modal covariate profile.
#'
#' @return A new \code{zelig2} object with the scenario stored. Any previous
#'   simulation results are cleared.
#'
#' @seealso \code{\link{setx1}} for contrast (alternative) scenarios,
#'   \code{\link{sim}} to simulate quantities of interest.
#'
#' @examples
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
#'
#' # Point scenario
#' z <- setx(z, hp = 150, wt = 3)
#'
#' # Range scenario
#' z <- setx(z, hp = seq(50, 300, by = 50))
#'
#' # Use mean instead of median as default
#' z <- setx(z, fn = "mean")
#'
#' # Use mean-of-dummies for unspecified factor covariates
#' z <- setx(z, fn = "mean", factor_default = "mean")
#'
#' @export
setx <- function(object, ..., fn = NULL,
                 factor_default = c("mode", "mean")) {
  if (!inherits(object, "zelig2")) {
    stop("object must be a zelig2 object.", call. = FALSE)
  }
  factor_default <- match.arg(factor_default)
  scenario <- build_scenario(object, list(...), fn = fn,
                             factor_default = factor_default)
  new_obj <- object
  new_obj$scenario <- scenario
  new_obj$sim_out <- NULL
  class(new_obj) <- "zelig2"
  new_obj
}

#' Set Contrast Scenario for First Differences
#'
#' Sets the alternative (contrast) scenario against which the primary scenario
#' (set by \code{\link{setx}}) is compared. The first difference is defined as
#' \eqn{\text{EV}(\texttt{setx1}) - \text{EV}(\texttt{setx})}{EV(setx1) - EV(setx)}.
#'
#' @inheritParams setx
#'
#' @return A new \code{zelig2} object with the contrast scenario stored.
#'
#' @seealso \code{\link{setx}}, \code{\link{sim}}.
#'
#' @examples
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
#' z <- setx(z, hp = 100)
#' z <- setx1(z, hp = 200)
#' z <- sim(z)
#' summary(z)
#'
#' @inheritParams setx
#' @export
setx1 <- function(object, ..., fn = NULL,
                  factor_default = c("mode", "mean")) {
  if (!inherits(object, "zelig2")) {
    stop("object must be a zelig2 object.", call. = FALSE)
  }
  factor_default <- match.arg(factor_default)
  scenario <- build_scenario(object, list(...), fn = fn,
                             factor_default = factor_default)
  new_obj <- object
  new_obj$scenario1 <- scenario
  new_obj$sim_out <- NULL
  class(new_obj) <- "zelig2"
  new_obj
}

#' Build a scenario from user inputs
#'
#' Internal workhorse that translates user-provided covariate values into a
#' model-matrix representation ready for simulation.
#'
#' @param object A \code{zelig2} object.
#' @param user_vals A named list of covariate values.
#' @param fn Optional default-function name.
#' @return A scenario list with components \code{x_matrix},
#'   \code{user_vals}, \code{is_range}, \code{range_var},
#'   \code{range_vals}, and \code{fe_contribution}.
#' @param factor_default How to set unspecified factor variables: \code{"mode"}
#'   (default) uses the modal level; \code{"mean"} replaces the variable's
#'   dummy columns with their column means in the fitted model matrix.
#' @keywords internal
build_scenario <- function(object, user_vals, fn = NULL,
                           factor_default = "mode") {
  data <- object$data
  formula <- object$formula

  tt <- stats::terms(object$formula)
  pred_vars <- all.vars(stats::delete.response(tt))

  # Separate FE values from covariate values
  fe_vals <- NULL
  if (isTRUE(object$is_fixest) && !is.null(object$fe_names)) {
    fe_var_names <- object$fe_names
    fe_vals <- user_vals[names(user_vals) %in% fe_var_names]
    if (length(fe_vals) == 0) fe_vals <- NULL
    user_vals <- user_vals[!names(user_vals) %in% fe_var_names]
  }

  # Compute FE contribution
  fe_contribution <- 0
  if (isTRUE(object$is_fixest)) {
    fe_contribution <- compute_fe_contribution(object$fit, fe_vals)
  }

  default_fn <- if (!is.null(fn)) match.fun(fn) else NULL

  # Check for range variables
  range_var <- NULL
  range_vals <- NULL
  for (nm in names(user_vals)) {
    if (length(user_vals[[nm]]) > 1) {
      if (!is.null(range_var)) {
        stop("Only one variable can have a range of values in setx().",
             call. = FALSE)
      }
      range_var <- nm
      range_vals <- user_vals[[nm]]
    }
  }

  if (!is.null(range_var)) {
    rows <- lapply(range_vals, function(rv) {
      vals <- user_vals
      vals[[range_var]] <- rv
      build_single_scenario(object, vals, pred_vars, default_fn,
                            factor_default = factor_default)
    })
    x_matrix <- do.call(rbind, rows)
    return(list(
      x_matrix = x_matrix,
      user_vals = user_vals,
      range_var = range_var,
      range_vals = range_vals,
      is_range = TRUE,
      fe_contribution = fe_contribution
    ))
  }

  x_row <- build_single_scenario(object, user_vals, pred_vars, default_fn,
                                 factor_default = factor_default)
  list(
    x_matrix = matrix(x_row, nrow = 1),
    user_vals = user_vals,
    range_var = NULL,
    range_vals = NULL,
    is_range = FALSE,
    fe_contribution = fe_contribution
  )
}

#' Build a single scenario row
#'
#' Constructs a one-row model-matrix representation of a covariate scenario.
#'
#' @param object A \code{zelig2} object.
#' @param user_vals Named list of covariate values.
#' @param pred_vars Character vector of predictor variable names.
#' @param default_fn Default function for unspecified numeric covariates
#'   (or \code{NULL}).
#' @param factor_default How to set unspecified factor variables: \code{"mode"}
#'   uses the modal level (giving a single-category counterfactual);
#'   \code{"mean"} replaces the dummy columns with their column means in the
#'   fitted model matrix (giving a population-average counterfactual).
#' @return A named numeric vector (one model-matrix row).
#' @keywords internal
build_single_scenario <- function(object, user_vals, pred_vars, default_fn,
                                  factor_default = "mode") {
  data <- object$data

  # Intercept-only models
  if (length(pred_vars) == 0) {
    return(1)
  }

  new_row <- list()
  for (v in pred_vars) {
    if (v %in% names(user_vals)) {
      val <- user_vals[[v]]
      if (is.factor(data[[v]])) {
        val <- factor(val, levels = levels(data[[v]]))
      }
      new_row[[v]] <- val
    } else {
      if (!is.null(default_fn) && is.numeric(data[[v]])) {
        new_row[[v]] <- default_fn(data[[v]])
      } else {
        new_row[[v]] <- default_val(data[[v]])
      }
    }
  }
  new_df <- as.data.frame(new_row, stringsAsFactors = FALSE)

  for (v in names(new_df)) {
    if (v %in% names(data) && is.factor(data[[v]])) {
      new_df[[v]] <- factor(new_df[[v]], levels = levels(data[[v]]))
    }
  }

  # Build model matrix from the linear formula (excludes FE for fixest)
  if (isTRUE(object$is_fixest)) {
    tt_noresp <- stats::delete.response(stats::terms(object$formula))
    mm <- stats::model.matrix(tt_noresp, data = new_df)
    # fixest absorbs the intercept into FEs; drop it from model matrix
    mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  } else {
    tt_noresp <- stats::delete.response(stats::terms(object$fit))
    mm <- stats::model.matrix(tt_noresp, data = new_df)
  }

  # If factor_default == "mean", replace the dummy columns of every
  # *unspecified* factor / character covariate with their column means in the
  # full fitted model matrix. This gives a population-average counterfactual
  # for those variables (rather than holding them at the mode).
  #
  # When the underlying fit has non-trivial observation weights (e.g., a
  # survey-weighted svyglm fit), the column means are computed as weighted
  # means using those weights. This makes the counterfactual covariate row
  # represent the *population* average composition rather than the
  # *sample* average composition. For unweighted (or constant-weight) fits,
  # ordinary unweighted column means are used.
  if (identical(factor_default, "mean")) {
    full_mm <- tryCatch(
      stats::model.matrix(object$fit),
      error = function(e) {
        # Fallback: rebuild from object$data using the same formula path
        tt_full <- if (isTRUE(object$is_fixest)) {
          stats::delete.response(stats::terms(object$formula))
        } else {
          stats::delete.response(stats::terms(object$fit))
        }
        stats::model.matrix(tt_full, data = data)
      }
    )

    # Try to obtain prior observation weights from the fit; fall back to
    # unweighted means if none are available or they're degenerate.
    fit_w <- tryCatch(
      stats::weights(object$fit, type = "prior"),
      error = function(e) NULL
    )
    use_weighted <- !is.null(fit_w) &&
      length(fit_w) == nrow(full_mm) &&
      all(is.finite(fit_w)) &&
      any(fit_w > 0) &&
      stats::var(fit_w) > 0

    if (use_weighted) {
      w_sum <- sum(fit_w)
      full_means <- as.numeric(crossprod(full_mm, fit_w)) / w_sum
      names(full_means) <- colnames(full_mm)
    } else {
      full_means <- colMeans(full_mm)
    }

    unspecified <- setdiff(pred_vars, names(user_vals))
    for (v in unspecified) {
      # Identify columns produced by this variable. For factors,
      # model.matrix names dummy columns by concatenating the variable name
      # with each non-reference level (e.g., "race_f5"). For numeric
      # variables, the column name is exactly the variable name. For
      # interactions, columns containing ":" are skipped to avoid
      # corrupting interaction terms involving v.
      col_names <- colnames(mm)
      if (is.factor(data[[v]]) || is.character(data[[v]])) {
        owns_col <- vapply(col_names, function(cn) {
          if (grepl(":", cn, fixed = TRUE)) return(FALSE)
          startsWith(cn, v) &&
            (nchar(cn) > nchar(v)) &&
            (substr(cn, nchar(v) + 1, nchar(v) + 1) != "(")
        }, logical(1))
      } else {
        # Numeric / logical: a single column with the same name as the
        # variable. Skip if it doesn't exist (e.g., a transformed term).
        owns_col <- col_names == v
      }
      idx <- which(owns_col)
      if (length(idx) > 0) {
        fm_idx <- match(col_names[idx], names(full_means))
        fm_idx <- fm_idx[!is.na(fm_idx)]
        if (length(fm_idx) == length(idx)) {
          mm[1, idx] <- full_means[fm_idx]
        }
      }
    }
  }

  as.numeric(mm[1, ])
}
