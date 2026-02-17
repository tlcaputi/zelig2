#' Convert a Fitted Model to a zelig2 Object
#'
#' Wraps an existing fitted model (e.g., from \code{\link[stats]{glm}} or
#' \code{\link[survey]{svyglm}}) in the \code{zelig2} structure so that
#' \code{\link{setx}}, \code{\link{sim}}, and \code{\link{plot.zelig2}} can be
#' applied to it.
#'
#' @param fit A fitted model object (e.g., class \code{glm}, \code{lm},
#'   \code{negbin}, \code{svyglm}, \code{fixest}).
#' @param model Character string naming the \pkg{zelig2} model type that
#'   corresponds to \code{fit} (e.g., \code{"ls"}, \code{"logit"}).
#' @param data The data frame that was used to fit \code{fit}.
#' @param vcov_type Variance--covariance estimator (default \code{"default"}).
#'   See \code{\link{zelig2}}.
#' @param cluster Cluster variable for clustered SEs.
#' @param num Integer; number of simulation draws (default 1000).
#'
#' @return A \code{zelig2} object.
#'
#' @seealso \code{\link{zelig2}}, \code{\link{from_zelig2_model}}.
#'
#' @examples
#' data(mtcars)
#' raw_fit <- glm(mpg ~ hp + wt, data = mtcars, family = gaussian())
#' z <- to_zelig2(raw_fit, model = "ls", data = mtcars)
#' z <- setx(z, hp = 150) |> sim()
#' summary(z)
#'
#' @export
to_zelig2 <- function(fit, model, data, vcov_type = "default",
                      cluster = NULL, num = 1000L) {
  spec <- get_model_spec(model)
  formula <- stats::formula(fit)
  data <- auto_factorize(data)

  V <- compute_vcov(fit, vcov_type = vcov_type, cluster = cluster,
                    data = data)
  coefs <- stats::coef(fit)
  cat_vars <- detect_categorical_vars(data, formula)

  is_fe <- inherits(fit, "fixest")
  fe_names <- NULL
  if (is_fe) {
    fe_names <- names(fixest::fixef(fit))
  }

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
    is_survey     = inherits(fit, "svyglm"),
    is_fixest     = is_fe,
    fe_names      = fe_names,
    survey_design = NULL,
    vcov_type     = vcov_type,
    cat_vars      = cat_vars,
    weights_col   = NULL,
    scenario      = NULL,
    scenario1     = NULL,
    sim_out       = NULL
  )
  class(obj) <- "zelig2"
  obj
}

#' Extract the Fitted Model from a zelig2 Object
#'
#' Returns the underlying model object (e.g., a \code{glm} or \code{fixest}
#' fit) stored inside a \code{zelig2} object. This is useful for calling
#' methods that are specific to the fitted-model class.
#'
#' @param object A \code{zelig2} object.
#' @return The underlying fitted model object.
#'
#' @seealso \code{\link{to_zelig2}}.
#'
#' @examples
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
#' fit <- from_zelig2_model(z)
#' class(fit)  # "glm" "lm"
#'
#' @export
from_zelig2_model <- function(object) {
  if (!inherits(object, "zelig2")) {
    stop("object must be a zelig2 object.", call. = FALSE)
  }
  object$fit
}

#' Convert Simulation Output to a Data Frame
#'
#' Transforms the simulation results stored in a \code{zelig2} object into a
#' tidy data frame suitable for further analysis or custom plotting.
#'
#' @param object A \code{zelig2} object with simulation results (after
#'   calling \code{\link{sim}}).
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{ev}{Expected values.}
#'     \item{pv}{Predicted values.}
#'     \item{ev1, pv1}{Contrast-scenario values (if \code{\link{setx1}} was
#'       used).}
#'     \item{fd}{First differences (if applicable).}
#'     \item{rr}{Risk ratios (if applicable).}
#'   }
#'   For range scenarios, the data frame is in long format with an additional
#'   column for the range variable.
#'
#' @seealso \code{\link{sim}}.
#'
#' @examples
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars) |>
#'   setx(hp = c(100, 150, 200)) |>
#'   sim()
#' df <- zelig2_qi_to_df(z)
#' head(df)
#'
#' @export
zelig2_qi_to_df <- function(object) {
  if (!inherits(object, "zelig2") || is.null(object$sim_out)) {
    stop("object must be a zelig2 object with simulation results.",
         call. = FALSE)
  }
  sim <- object$sim_out

  if (is.matrix(sim$ev)) {
    n_draws <- nrow(sim$ev)
    n_vals <- ncol(sim$ev)
    range_vals <- rep(as.numeric(colnames(sim$ev)), each = n_draws)
    df <- data.frame(
      range_val = range_vals,
      ev = as.numeric(sim$ev),
      pv = as.numeric(sim$pv)
    )
    if (!is.null(sim$scenario$range_var)) {
      names(df)[1] <- sim$scenario$range_var
    }
  } else {
    df <- data.frame(ev = sim$ev, pv = sim$pv)
    if (!is.null(sim$ev1)) df$ev1 <- sim$ev1
    if (!is.null(sim$pv1)) df$pv1 <- sim$pv1
    if (!is.null(sim$fd)) df$fd <- sim$fd
    if (!is.null(sim$rr)) df$rr <- sim$rr
  }
  df
}
