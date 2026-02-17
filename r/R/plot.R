#' Plot Simulation Results
#'
#' Visualises the quantities of interest from \code{\link{sim}} using
#' \pkg{ggplot2}. The plot type is selected automatically based on the model
#' family and the scenario structure:
#' \itemize{
#'   \item \strong{Density plot}: continuous / count outcomes with a point
#'     scenario.
#'   \item \strong{Bar chart}: binary outcomes (predicted values).
#'   \item \strong{Ribbon plot}: range scenarios showing the confidence band
#'     over the varying covariate.
#'   \item \strong{First-difference density}: when a contrast scenario is set.
#' }
#'
#' @param x A \code{zelig2} object with simulation results (i.e., after
#'   calling \code{\link{sim}}).
#' @param ... Currently unused.
#' @param ci Numeric confidence level for the plotted interval
#'   (default 0.95).
#'
#' @return A \code{ggplot} object, or a \code{patchwork} composition of
#'   multiple \code{ggplot} objects when several quantities are plotted.
#'
#' @seealso \code{\link{sim}}, \code{\link{summary.zelig2}}.
#'
#' @examples
#' data(mtcars)
#' z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars) |>
#'   setx(hp = 150, wt = 3) |>
#'   sim()
#' plot(z)
#'
#' @export
plot.zelig2 <- function(x, ..., ci = 0.95) {
  if (is.null(x$sim_out)) {
    stop("No simulation results. Call sim() before plot().", call. = FALSE)
  }

  sim_out <- x$sim_out
  alpha <- 1 - ci
  plots <- list()

  is_range <- !is.null(sim_out$scenario) && isTRUE(sim_out$scenario$is_range)

  if (is_range) {
    plots$ev <- plot_range(sim_out$ev, sim_out$scenario$range_var,
                           sim_out$scenario$range_vals, alpha,
                           title = "Expected Values")
  } else if (x$category == "binary") {
    plots$ev <- plot_density(sim_out$ev, alpha,
                             title = "Expected Values: Pr(Y = 1)")
    plots$pv <- plot_bar(sim_out$pv, alpha,
                         title = "Predicted Values", discrete = TRUE)
  } else {
    plots$ev <- plot_density(sim_out$ev, alpha, title = "Expected Values")
    plots$pv <- plot_density(sim_out$pv, alpha, title = "Predicted Values")
  }

  if (!is.null(sim_out$fd)) {
    if (is.matrix(sim_out$fd)) {
      plots$fd <- plot_range(sim_out$fd, sim_out$scenario$range_var,
                             sim_out$scenario$range_vals, alpha,
                             title = "First Differences")
    } else {
      plots$fd <- plot_density(sim_out$fd, alpha, title = "First Differences")
    }
  }

  if (length(plots) == 1) return(plots[[1]])
  patchwork::wrap_plots(plots, ncol = 1)
}

#' @keywords internal
plot_density <- function(vals, alpha, title = "") {
  lo <- alpha / 2
  hi <- 1 - alpha / 2
  q <- stats::quantile(vals, c(lo, hi), na.rm = TRUE)
  df <- data.frame(value = vals)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_density(fill = "steelblue", alpha = 0.5, color = NA) +
    ggplot2::geom_vline(xintercept = q, linetype = "dashed",
                        color = "grey30") +
    ggplot2::geom_vline(xintercept = mean(vals, na.rm = TRUE),
                        color = "darkblue", linewidth = 0.8) +
    ggplot2::labs(title = title, x = "Value", y = "Density") +
    ggplot2::theme_minimal()
}

#' @keywords internal
plot_bar <- function(vals, alpha, title = "", discrete = FALSE) {
  if (discrete) {
    counts <- table(factor(vals, levels = c(0, 1)))
    df <- data.frame(outcome = names(counts), count = as.numeric(counts))
    ggplot2::ggplot(df, ggplot2::aes(x = .data$outcome, y = .data$count)) +
      ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
      ggplot2::labs(title = title, x = "Outcome", y = "Count") +
      ggplot2::theme_minimal()
  } else {
    plot_density(vals, alpha, title = title)
  }
}

#' @keywords internal
plot_range <- function(ev_mat, range_var, range_vals, alpha, title = "") {
  lo <- alpha / 2
  hi <- 1 - alpha / 2
  range_vals_num <- as.numeric(range_vals)
  means <- colMeans(ev_mat, na.rm = TRUE)
  lowers <- apply(ev_mat, 2, stats::quantile, probs = lo, na.rm = TRUE)
  uppers <- apply(ev_mat, 2, stats::quantile, probs = hi, na.rm = TRUE)
  df <- data.frame(x = range_vals_num, mean = means,
                   lower = lowers, upper = uppers)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower,
                                       ymax = .data$upper),
                         fill = "steelblue", alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = .data$mean),
                       color = "darkblue", linewidth = 0.8) +
    ggplot2::labs(title = title, x = range_var, y = "Value") +
    ggplot2::theme_minimal()
}
