#' Rogan-Gladen misclassification-adjusted prevalence
#'
#' Adjusts an apparent (judge-labelled) prevalence for the judge's
#' imperfect sensitivity and specificity estimated in an audit:
#' \code{adjusted = (apparent + Sp - 1) / (Se + Sp - 1)}
#' (Rogan & Gladen, 1978).
#'
#' Uncertainty is propagated by the delta method, accounting for
#' sampling uncertainty in all three inputs, assumed independent:
#' apparent prevalence from the main sample (`n_main`), sensitivity
#' from the audit positives (`n_audit_pos`) and specificity from
#' the audit negatives (`n_audit_neg`). The independence assumption
#' is violated if the audit is a subsample of the main sample; in
#' that case the interval will be wrong (typically too narrow in
#' the audit terms). A subsample whose audit total is at most
#' `n_main` cannot be detected from the counts alone and would
#' violate independence just the same. A warning is issued when
#' both audit sizes are supplied and their sum exceeds `n_main`:
#' an audit larger than the main sample can be neither a
#' subsample of it nor a separate sample consistent with the
#' stated sizes, so the counts themselves are inconsistent with
#' the setup the independence assumption requires. If
#' audit sizes are not supplied, sensitivity
#' and specificity are treated as fixed (their variance terms are
#' zero) and this is noted in the output. The reported interval is
#' clamped as a whole to \[0, 1\]: both bounds are clamped into
#' the range, so the lower bound never exceeds the upper bound
#' even when the unclamped interval lies entirely outside
#' \[0, 1\]. Like all delta-method intervals it can
#' undercover when the true prevalence is near a boundary or when
#' `Se + Sp` is close to 1.
#'
#' When \code{Se + Sp <= 1} the judge carries no information (or
#' worse than chance) and adjustment is impossible: the function
#' warns and returns `valid = FALSE` with an `NA` estimate.
#'
#' @param apparent Apparent (judge) prevalence in the main sample,
#'   in (0, 1).
#' @param sensitivity Audit-estimated sensitivity, in (0, 1).
#' @param specificity Audit-estimated specificity, in (0, 1).
#' @param n_main Main sample size used for `apparent`; a positive
#'   whole number.
#' @param n_audit_pos Number of human-positive cases in the audit
#'   (for the sensitivity variance), a positive whole number, or
#'   `NULL`.
#' @param n_audit_neg Number of human-negative cases in the audit
#'   (for the specificity variance), a positive whole number, or
#'   `NULL`.
#' @param conf_level Confidence level, default 0.95.
#' @return An object of class `"judge_adjust"`: a list with
#'   components `adjusted` (point estimate truncated to \[0, 1\]),
#'   `adjusted_raw` (untruncated), `se`, `lower`, `upper`
#'   (interval bounds clamped to \[0, 1\]), `valid` (logical),
#'   and `note`.
#' @family judge audit functions
#' @references
#' Rogan, W. J. and Gladen, B. (1978). Estimating prevalence from
#' the results of a screening test. *American Journal of
#' Epidemiology*, 107(1), 71-76.
#' @examples
#' judge_adjust(apparent = 0.35, sensitivity = 0.85,
#'              specificity = 0.90, n_main = 2000,
#'              n_audit_pos = 196, n_audit_neg = 139)
#' @export
judge_adjust <- function(apparent, sensitivity, specificity,
                         n_main, n_audit_pos = NULL,
                         n_audit_neg = NULL, conf_level = 0.95) {
  check_probability(apparent, "apparent")
  check_probability(sensitivity, "sensitivity")
  check_probability(specificity, "specificity")
  check_positive_count(n_main, "n_main")
  if (!is.null(n_audit_pos)) {
    check_positive_count(n_audit_pos, "n_audit_pos")
  }
  if (!is.null(n_audit_neg)) {
    check_positive_count(n_audit_neg, "n_audit_neg")
  }
  check_conf_level(conf_level)
  if (!is.null(n_audit_pos) && !is.null(n_audit_neg) &&
      n_audit_pos + n_audit_neg > n_main) {
    warning("n_audit_pos + n_audit_neg (",
            n_audit_pos + n_audit_neg, ") exceeds n_main (",
            n_main, "): an audit larger than the main sample ",
            "can be neither a subsample of it nor a separate ",
            "sample consistent with the stated sizes; the ",
            "counts are inconsistent with the independence ",
            "assumption behind the delta-method CI, so the ",
            "interval will typically be too narrow.",
            call. = FALSE)
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  j_index <- sensitivity + specificity - 1
  if (j_index <= 0) {
    warning("Se + Sp <= 1: judge is uninformative; Rogan-Gladen ",
            "adjustment is not identified.", call. = FALSE)
    out <- list(adjusted = NA_real_, adjusted_raw = NA_real_,
                se = NA_real_, lower = NA_real_, upper = NA_real_,
                valid = FALSE,
                note = "Se+Sp<=1: adjustment not identified.")
    class(out) <- "judge_adjust"
    return(out)
  }
  adj_raw <- (apparent + specificity - 1) / j_index
  # Delta method. theta = (p + Sp - 1)/J, J = Se + Sp - 1.
  # d/dp = 1/J; d/dSe = -(p+Sp-1)/J^2; d/dSp = (Se - p)/J^2.
  d_p <- 1 / j_index
  d_se <- -adj_raw / j_index
  d_sp <- (sensitivity - apparent) / j_index^2
  var_p <- apparent * (1 - apparent) / n_main
  var_se <- if (!is.null(n_audit_pos)) {
    sensitivity * (1 - sensitivity) / n_audit_pos
  } else {
    0
  }
  var_sp <- if (!is.null(n_audit_neg)) {
    specificity * (1 - specificity) / n_audit_neg
  } else {
    0
  }
  var_adj <- d_p^2 * var_p + d_se^2 * var_se + d_sp^2 * var_sp
  se <- sqrt(var_adj)
  note <- if (is.null(n_audit_pos) || is.null(n_audit_neg)) {
    paste0("One or both audit sizes missing: corresponding Se/Sp ",
           "treated as fixed, so the CI is too narrow. ",
           "Delta-method CI; audit and main samples assumed ",
           "independent. Adjusted estimate truncated to [0,1] in ",
           "$adjusted; $adjusted_raw is untruncated; CI bounds ",
           "clamped to [0,1].")
  } else {
    paste0("Delta-method CI; audit and main samples assumed ",
           "independent. Adjusted estimate truncated to [0,1] in ",
           "$adjusted; $adjusted_raw is untruncated; CI bounds ",
           "clamped to [0,1].")
  }
  # Clamp the interval as a whole: clamping each bound on one
  # side only (lower at 0, upper at 1) inverts the interval when
  # it lies entirely outside [0, 1] (raw upper < 0 gives
  # lower = 0 > upper; raw lower > 1 gives upper = 1 < lower).
  # Clamping both bounds into [0, 1] is monotone, so
  # lower <= upper always.
  clamp01 <- function(v) min(1, max(0, v))
  out <- list(adjusted = clamp01(adj_raw),
              adjusted_raw = adj_raw, se = se,
              lower = clamp01(adj_raw - z * se),
              upper = clamp01(adj_raw + z * se),
              valid = TRUE, note = note)
  class(out) <- "judge_adjust"
  return(out)
}

#' @export
print.judge_adjust <- function(x, ...) {
  cat("judgeaudit Rogan-Gladen adjustment\n")
  if (isTRUE(x$valid)) {
    cat(sprintf("Adjusted prevalence=%.3f [%.3f, %.3f]\n",
                x$adjusted, x$lower, x$upper))
  } else {
    cat("Adjusted prevalence=NA (not identified)\n")
  }
  cat("Note:", x$note, "\n")
  invisible(x)
}
