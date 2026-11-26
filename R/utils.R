#' Internal validation helpers
#'
#' Shared input checks for the exported functions. These helpers
#' stop with the same messages the exported functions have always
#' used; they exist so the checks live in exactly one place.
#'
#' @keywords internal
#' @noRd
NULL

check_probability <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      x <= 0 || x >= 1) {
    stop(name, " must be a single number strictly between 0 and 1",
         call. = FALSE)
  }
  invisible(x)
}

check_conf_level <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      x <= 0 || x >= 1) {
    stop("conf_level must be a single number between 0 and 1",
         call. = FALSE)
  }
  invisible(x)
}

check_positive_count <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x < 1 || x != floor(x)) {
    stop(name, " must be a positive whole number", call. = FALSE)
  }
  invisible(x)
}

#' Convert 0/1, logical, or two-level factor labels to integer 0/1.
#' @keywords internal
#' @noRd
as_binary_labels <- function(x, name) {
  if (is.factor(x)) {
    if (nlevels(x) != 2L) {
      stop(name, " factor must have exactly 2 levels", call. = FALSE)
    }
    return(as.integer(x) - 1L)
  }
  if (is.logical(x)) {
    return(as.integer(x))
  }
  if (is.numeric(x)) {
    if (any(!x %in% c(0, 1))) {
      stop(name, " numeric labels must be 0/1", call. = FALSE)
    }
    return(as.integer(x))
  }
  stop(name, " must be 0/1 numeric, logical, or a 2-level factor",
       call. = FALSE)
}

#' Wilson score interval for a binomial proportion.
#' @keywords internal
#' @noRd
wilson_interval <- function(successes, trials, z) {
  if (trials == 0) {
    return(data.frame(estimate = NA_real_, lower = NA_real_,
                      upper = NA_real_))
  }
  p <- successes / trials
  den <- 1 + z^2 / trials
  centre <- (p + z^2 / (2 * trials)) / den
  half <- (z / den) * sqrt(p * (1 - p) / trials +
                             z^2 / (4 * trials^2))
  data.frame(estimate = p,
             lower = max(0, centre - half),
             upper = min(1, centre + half))
}
