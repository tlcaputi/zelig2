#' zelig2: Modern Statistical Modeling with Quantities of Interest
#'
#' A modern reimplementation of the Zelig package (King, Tomz, and Wittenberg
#' 2000) providing a unified, four-step workflow for statistical inference:
#'
#' \enumerate{
#'   \item \strong{Estimate}: \code{\link{zelig2}(formula, model, data)}
#'   \item \strong{Set scenarios}: \code{\link{setx}(z, x1 = value)}
#'   \item \strong{Simulate}: \code{\link{sim}(z)}
#'   \item \strong{Visualise}: \code{\link{plot.zelig2}(z)}
#' }
#'
#' @section Supported models:
#' \describe{
#'   \item{ls}{Least squares (OLS / linear regression)}
#'   \item{logit}{Logistic regression}
#'   \item{probit}{Probit regression}
#'   \item{poisson}{Poisson regression}
#'   \item{negbin}{Negative binomial regression}
#'   \item{gamma}{Gamma regression}
#'   \item{tobit}{Tobit (censored) regression}
#'   \item{quantile}{Quantile regression}
#' }
#'
#' @section Key features:
#' \itemize{
#'   \item \strong{Fixed effects}: Include \code{|} in the formula (e.g.,
#'     \code{y ~ x | fe}) or pass \code{fixef} to use \pkg{fixest} for fast
#'     high-dimensional FE estimation.
#'   \item \strong{Survey weights}: Pass a \code{survey_design} or use
#'     convenience parameters (\code{ids}, \code{strata}, \code{fpc},
#'     \code{weights}) with no need for separate model types.
#'   \item \strong{Robust / clustered / bootstrap SEs}: Set
#'     \code{vcov_type} to \code{"robust"}, \code{"HC0"}--\code{"HC4"},
#'     \code{"cluster"}, or \code{"bootstrap"}.
#'   \item \strong{Automatic categorical handling}: Character columns are
#'     converted to factors; \code{setx()} accepts level names directly.
#'   \item \strong{ggplot2 visualisation}: All plots use \pkg{ggplot2} and
#'     \pkg{patchwork}.
#' }
#'
#' @docType package
#' @name zelig2-package
#'
#' @importFrom MASS mvrnorm glm.nb
#' @importFrom ggplot2 ggplot aes geom_density geom_bar geom_ribbon geom_line
#'   geom_vline labs theme_minimal scale_fill_manual scale_color_manual .data
#' @importFrom patchwork wrap_plots
#' @importFrom sandwich vcovHC vcovCL vcovBS
#' @importFrom survey svydesign svyglm
#' @importFrom stats coef vcov model.matrix terms formula predict
#'   delete.response quantile median pnorm glm binomial poisson Gamma gaussian
#'   na.omit model.frame as.formula sd printCoefmat rbinom rnorm rpois rnbinom
#'   rgamma residuals var
#' @importFrom utils head
#' @keywords internal
"_PACKAGE"
