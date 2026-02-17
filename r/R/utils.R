#' Internal Utility Functions
#'
#' Shared helper functions used throughout the \pkg{zelig2} package.
#'
#' @name utils
#' @keywords internal
NULL

#' Null coalescing operator
#' @param a Left-hand value.
#' @param b Default value if \code{a} is \code{NULL}.
#' @return \code{a} if not \code{NULL}, otherwise \code{b}.
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Statistical mode
#'
#' Returns the most frequent value in a vector. For ties, returns the first
#' value encountered.
#'
#' @param x A vector.
#' @return The most frequent value.
#' @keywords internal
mode_zelig2 <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Median with factor support
#'
#' Wrapper around \code{\link[stats]{median}} that handles factors by
#' returning the level closest to the median of the underlying integer codes.
#' For character vectors, falls back to the mode.
#'
#' @param x A vector.
#' @return The median value (or mode for character / factor).
#' @keywords internal
median_zelig2 <- function(x) {
  if (is.factor(x)) {
    lvls <- levels(x)
    med_idx <- round(stats::median(as.integer(x), na.rm = TRUE))
    med_idx <- max(1L, min(med_idx, length(lvls)))
    factor(lvls[med_idx], levels = lvls)
  } else if (is.character(x)) {
    mode_zelig2(x)
  } else {
    stats::median(x, na.rm = TRUE)
  }
}

#' Convert character columns to factors
#'
#' Converts all character columns in a data frame to factors, replicating
#' the behaviour of \code{\link[stats]{lm}} and \code{\link[stats]{glm}}.
#'
#' @param data A data frame.
#' @return The data frame with character columns converted to factors.
#' @keywords internal
auto_factorize <- function(data) {
  char_cols <- vapply(data, is.character, logical(1))
  if (any(char_cols)) {
    data[char_cols] <- lapply(data[char_cols], as.factor)
  }
  data
}

#' Detect categorical variables in a formula
#'
#' @param data A data frame.
#' @param formula A model formula.
#' @return A named list of factor level vectors, keyed by variable name.
#' @keywords internal
detect_categorical_vars <- function(data, formula) {
  vars <- all.vars(formula)
  vars <- intersect(vars, names(data))
  cat_info <- list()
  for (v in vars) {
    if (is.factor(data[[v]]) || is.character(data[[v]])) {
      cat_info[[v]] <- levels(as.factor(data[[v]]))
    }
  }
  cat_info
}

#' Default value for a variable
#'
#' Returns the median for numeric variables and the mode for factors,
#' characters, and logicals.
#'
#' @param x A vector.
#' @return The default (central) value.
#' @keywords internal
default_val <- function(x) {
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    mode_zelig2(x)
  } else {
    stats::median(x, na.rm = TRUE)
  }
}
