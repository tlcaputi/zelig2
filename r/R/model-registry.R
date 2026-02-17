#' Model Registry
#'
#' An internal package-level environment that maps model-name strings to
#' specification lists. This replaces the class-hierarchy approach used by
#' the original Zelig package with a lightweight, extensible registry.
#'
#' @name model-registry
#' @keywords internal
NULL

# Package-level environment
.model_registry <- new.env(parent = emptyenv())

#' Register a model in the registry
#'
#' @param name Character string model name (e.g., \code{"logit"}).
#' @param fit_fn Fitting function.
#' @param param_fn Parameter-drawing function.
#' @param qi_fn Quantity-of-interest function.
#' @param category Character string category (\code{"continuous"},
#'   \code{"binary"}, or \code{"count"}).
#' @param description Optional human-readable description.
#' @param ... Additional named elements stored in the spec.
#' @keywords internal
register_model <- function(name, fit_fn, param_fn, qi_fn, category,
                           description = NULL, ...) {
  spec <- list(
    name = name,
    fit_fn = fit_fn,
    param_fn = param_fn,
    qi_fn = qi_fn,
    category = category,
    description = description %||% name
  )
  extras <- list(...)
  if (length(extras) > 0) spec <- c(spec, extras)
  assign(name, spec, envir = .model_registry)
  invisible(spec)
}

#' Retrieve a model specification
#'
#' @param name Character string model name.
#' @return A named list with the model specification.
#' @keywords internal
get_model_spec <- function(name) {
  if (!exists(name, envir = .model_registry, inherits = FALSE)) {
    available <- list_models()
    stop(
      sprintf("Model '%s' not found. Available: %s",
              name, paste(available, collapse = ", ")),
      call. = FALSE
    )
  }
  get(name, envir = .model_registry, inherits = FALSE)
}

#' List Available Models
#'
#' Returns the names of all model families registered in the \pkg{zelig2}
#' model registry.
#'
#' @return A character vector of model names.
#'
#' @examples
#' list_models()
#'
#' @export
list_models <- function() {
  ls(envir = .model_registry)
}
