#' Analyse a human-gold-standard audit of an AI judge
#'
#' Given paired human gold labels and judge labels on the same
#' items, returns the confusion matrix, observed agreement,
#' Cohen's kappa with an asymptotic confidence interval,
#' sensitivity and specificity with Wilson score intervals, and
#' human versus judge (apparent) prevalence.
#'
#' A kappa-paradox warning is flagged when observed agreement
#' is high (>= 0.8), either margin's
#' prevalence is extreme (< 0.1 or > 0.9) and kappa is much lower
#' than observed agreement (agreement - kappa > 0.2): high
#' agreement with low kappa driven by skewed marginals, not
#' necessarily by a bad judge. The kappa standard error uses the
#' approximation \code{sqrt(po(1-po) / (n (1-pe)^2))}, which
#' ignores the sampling variability of the chance-agreement term
#' and can understate the true uncertainty; the reported interval
#' is a Wald interval on that approximate standard error, clamped
#' to \[-1, 1\], and should be treated as approximate. If one
#' class is absent from the human labels, the corresponding
#' sensitivity or specificity is undefined (`NA`) and a warning
#' is issued.
#'
#' @param human Human gold labels: numeric or logical 0/1, or a
#'   two-level factor (second level treated as positive). If both
#'   `human` and `judge` are factors, they must have identical
#'   levels in the same order; a mismatch is an error, not a
#'   silent recoding.
#' @param judge Judge labels, same coding and length as `human`.
#' @param conf_level Confidence level, default 0.95.
#' @return An object of class `"judge_audit"`: a list with
#'   components `n`, `confusion` (2x2 matrix, rows = human,
#'   columns = judge), `agreement` (point estimate),
#'   `kappa` (data frame: `estimate`, `se`, `lower`, `upper`),
#'   `sensitivity` and `specificity` (data frames: `estimate`,
#'   `lower`, `upper`, Wilson score intervals),
#'   `prevalence_human`, `prevalence_judge`,
#'   `kappa_paradox_warning` (logical), `warning_message`,
#'   `conf_level`, and `kappa_se_note`.
#' @family judge audit functions
#' @references
#' Cohen, J. (1960). A coefficient of agreement for nominal
#' scales. *Educational and Psychological Measurement*, 20(1),
#' 37-46.
#'
#' Wilson, E. B. (1927). Probable inference, the law of
#' succession, and statistical inference. *Journal of the
#' American Statistical Association*, 22(158), 209-212.
#' @examples
#' h <- c(1, 1, 0, 0, 1, 0, 1, 0, 0, 1)
#' j <- c(1, 0, 0, 0, 1, 0, 1, 1, 0, 1)
#' judge_audit(h, j)
#' @export
judge_audit <- function(human, judge, conf_level = 0.95) {
  if (length(human) != length(judge)) {
    stop("human and judge must have the same length", call. = FALSE)
  }
  if (length(human) < 4L) {
    stop("need at least 4 paired labels", call. = FALSE)
  }
  if (anyNA(human) || anyNA(judge)) {
    stop("missing labels are not supported in the prototype; ",
         "remove or impute first", call. = FALSE)
  }
  check_conf_level(conf_level)
  if (is.factor(human) && is.factor(judge) &&
      !identical(levels(human), levels(judge))) {
    stop("human and judge factors must have identical levels in ",
         "the same order (the second level is the positive ",
         "class); human levels are (",
         paste(levels(human), collapse = ", "),
         ") but judge levels are (",
         paste(levels(judge), collapse = ", "), ")", call. = FALSE)
  }
  h <- as_binary_labels(human, "human")
  j <- as_binary_labels(judge, "judge")
  n <- length(h)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  tp <- sum(h == 1 & j == 1)
  fn <- sum(h == 1 & j == 0)
  fp <- sum(h == 0 & j == 1)
  tn <- sum(h == 0 & j == 0)
  cm <- matrix(c(tn, fp, fn, tp), nrow = 2, byrow = TRUE,
               dimnames = list(human = c("0", "1"),
                               judge = c("0", "1")))
  po <- (tp + tn) / n
  prev_h <- mean(h)
  prev_j <- mean(j)
  pe <- prev_h * prev_j + (1 - prev_h) * (1 - prev_j)
  kappa <- if (pe < 1) (po - pe) / (1 - pe) else NA_real_
  # Asymptotic SE of kappa: the common large-sample approximation
  # var = po(1-po) / (n (1-pe)^2); documented as approximate.
  kappa_se <- if (!is.na(kappa)) {
    sqrt(po * (1 - po) / (n * (1 - pe)^2))
  } else {
    NA_real_
  }
  if (tp + fn == 0) {
    warning("no human-positive cases: sensitivity is undefined ",
            "(NA)", call. = FALSE)
  }
  if (tn + fp == 0) {
    warning("no human-negative cases: specificity is undefined ",
            "(NA)", call. = FALSE)
  }
  sens <- wilson_interval(tp, tp + fn, z)
  spec <- wilson_interval(tn, tn + fp, z)
  paradox <- is.finite(kappa) && po >= 0.8 &&
    (prev_h < 0.1 || prev_h > 0.9 ||
     prev_j < 0.1 || prev_j > 0.9) && (po - kappa > 0.2)
  out <- list(
    n = n, confusion = cm, agreement = po,
    kappa = data.frame(
      estimate = kappa, se = kappa_se,
      lower = if (!is.na(kappa)) {
        max(-1, kappa - z * kappa_se)
      } else {
        NA_real_
      },
      upper = if (!is.na(kappa)) {
        min(1, kappa + z * kappa_se)
      } else {
        NA_real_
      }),
    sensitivity = sens, specificity = spec,
    prevalence_human = prev_h, prevalence_judge = prev_j,
    kappa_paradox_warning = paradox,
    warning_message = if (paradox) {
      paste0("Kappa paradox likely: extreme prevalence with high ",
             "observed agreement but much lower kappa. Report ",
             "sensitivity/specificity and prevalence alongside ",
             "kappa.")
    } else {
      NA_character_
    },
    conf_level = conf_level,
    kappa_se_note = paste0(
      "Kappa SE uses the approximation ",
      "sqrt(po(1-po)/(n(1-pe)^2)), which ignores the sampling ",
      "variability of pe and can understate uncertainty; the ",
      "kappa CI is a Wald interval on that SE, clamped to ",
      "[-1, 1], and is approximate."))
  class(out) <- "judge_audit"
  return(out)
}

#' @export
print.judge_audit <- function(x, ...) {
  cat("judgeaudit audit, n =", x$n, "\n")
  cat("Confusion (rows=human, cols=judge):\n")
  print(x$confusion)
  if (is.na(x$kappa$estimate)) {
    cat(sprintf(paste0("Agreement=%.3f  Kappa=NA (undefined: ",
                       "no variation in one margin)\n"),
                x$agreement))
  } else {
    cat(sprintf("Agreement=%.3f  Kappa=%.3f [%.3f, %.3f]\n",
                x$agreement, x$kappa$estimate,
                x$kappa$lower, x$kappa$upper))
  }
  cat(sprintf(paste0("Sensitivity=%.3f [%.3f, %.3f]  ",
                     "Specificity=%.3f [%.3f, %.3f]\n"),
              x$sensitivity$estimate, x$sensitivity$lower,
              x$sensitivity$upper,
              x$specificity$estimate, x$specificity$lower,
              x$specificity$upper))
  cat(sprintf("Prevalence human=%.3f judge=%.3f\n",
              x$prevalence_human, x$prevalence_judge))
  if (isTRUE(x$kappa_paradox_warning)) {
    cat("WARNING:", x$warning_message, "\n")
  }
  invisible(x)
}
