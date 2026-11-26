#' Design a human-gold-standard audit of an AI judge
#'
#' Computes how many human-labelled cases are needed so that the
#' sensitivity and/or specificity of a judge (estimated against
#' human gold labels) is measured to a target confidence-interval
#' half-width.
#'
#' Method: normal-approximation planning,
#' \code{n_pos = z^2 Se(1-Se)/w^2} positives are needed for
#' sensitivity and \code{n_neg = z^2 Sp(1-Sp)/w^2} negatives for
#' specificity, where \code{w} is the target half-width. With human
#' prevalence \code{prevalence}, the total audit size implied by
#' each target is \code{n_pos / prevalence} and
#' \code{n_neg / (1 - prevalence)}; the required total is the
#' maximum of the requested targets.
#'
#' Planning uses the normal (Wald) approximation throughout,
#' while \code{judge_audit()} reports Wilson score intervals for
#' sensitivity and specificity; the achieved Wilson width can
#' therefore differ slightly from the planned target.
#'
#' Kappa: if \code{target_kappa_halfwidth} is given, a simple
#' approximation is used: treating kappa roughly like a proportion
#' with variance \code{kappa(1 - kappa)/n},
#' \code{n = z^2 kappa(1 - kappa)/w^2}. This is labelled
#' APPROXIMATE in the output; it ignores the prevalence dependence
#' of kappa's true variance and should be used only for rough
#' planning.
#'
#' @param sensitivity Expected judge sensitivity, in (0, 1).
#' @param specificity Expected judge specificity, in (0, 1).
#' @param prevalence Expected human (gold) prevalence of the
#'   positive class, in (0, 1).
#' @param halfwidth_se Target CI half-width for sensitivity, a
#'   single number in (0, 0.5), or `NULL`.
#' @param halfwidth_sp Target CI half-width for specificity, a
#'   single number in (0, 0.5), or `NULL`.
#' @param width_se Target CI full width for sensitivity, a single
#'   number in (0, 1) (alternative to `halfwidth_se`).
#' @param width_sp Target CI full width for specificity, a single
#'   number in (0, 1) (alternative to `halfwidth_sp`).
#' @param kappa Expected Cohen's kappa, in (0, 1); only used with
#'   `target_kappa_halfwidth`.
#' @param target_kappa_halfwidth Target CI half-width for kappa, a
#'   single number in (0, 0.5), or `NULL` (approximate; see
#'   Details).
#' @param conf_level Confidence level, default 0.95.
#' @return An object of class `"judge_design"`: a list with
#'   components `table` (a data frame with one row per target and
#'   columns `target`, `halfwidth`, `n_pos_required`,
#'   `n_neg_required`, `total_n_required`), `n_pos`, `n_neg`,
#'   `total_n` (the required totals), `expected_pos`,
#'   `expected_neg`, `conf_level`, and `assumptions` (a character
#'   vector documenting the method).
#' @family judge audit functions
#' @references
#' Cohen, J. (1960). A coefficient of agreement for nominal
#' scales. *Educational and Psychological Measurement*, 20(1),
#' 37-46.
#' @examples
#' judge_design(sensitivity = 0.85, specificity = 0.90,
#'              prevalence = 0.30,
#'              halfwidth_se = 0.05, halfwidth_sp = 0.05)
#' @export
judge_design <- function(sensitivity, specificity, prevalence,
                         halfwidth_se = NULL, halfwidth_sp = NULL,
                         width_se = NULL, width_sp = NULL,
                         kappa = NULL,
                         target_kappa_halfwidth = NULL,
                         conf_level = 0.95) {
  check_probability(sensitivity, "sensitivity")
  check_probability(specificity, "specificity")
  check_probability(prevalence, "prevalence")
  check_conf_level(conf_level)
  resolve_hw <- function(hw, w, nm) {
    if (!is.null(hw) && !is.null(w)) {
      stop("give only one of halfwidth_/width_ for ", nm,
           call. = FALSE)
    }
    if (!is.null(w)) {
      if (!is.numeric(w) || length(w) != 1L || is.na(w) ||
          w <= 0 || w >= 1) {
        stop("target full width for ", nm,
             " must be a single number in (0, 1)", call. = FALSE)
      }
      hw <- w / 2
    }
    if (!is.null(hw) &&
        (!is.numeric(hw) || length(hw) != 1L || is.na(hw) ||
         hw <= 0 || hw >= 0.5)) {
      stop("target half-width for ", nm,
           " must be a single number in (0, 0.5)", call. = FALSE)
    }
    hw
  }
  halfwidth_se <- resolve_hw(halfwidth_se, width_se, "sensitivity")
  halfwidth_sp <- resolve_hw(halfwidth_sp, width_sp, "specificity")
  if (is.null(halfwidth_se) && is.null(halfwidth_sp) &&
      is.null(target_kappa_halfwidth)) {
    stop("supply at least one target: halfwidth_se, halfwidth_sp, ",
         "or target_kappa_halfwidth", call. = FALSE)
  }
  if (!is.null(target_kappa_halfwidth)) {
    if (is.null(kappa)) {
      stop("kappa is required when target_kappa_halfwidth is given",
           call. = FALSE)
    }
    check_probability(kappa, "kappa")
    if (!is.numeric(target_kappa_halfwidth) ||
        length(target_kappa_halfwidth) != 1L ||
        is.na(target_kappa_halfwidth) ||
        target_kappa_halfwidth <= 0 ||
        target_kappa_halfwidth >= 0.5) {
      stop("target_kappa_halfwidth must be a single number ",
           "in (0, 0.5)", call. = FALSE)
    }
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  rows <- list()
  n_pos_req <- 0
  n_neg_req <- 0
  total_req <- 0
  if (!is.null(halfwidth_se)) {
    np <- ceiling(z^2 * sensitivity * (1 - sensitivity) /
                    halfwidth_se^2)
    tot <- ceiling(np / prevalence)
    rows$se <- data.frame(
      target = "sensitivity", halfwidth = halfwidth_se,
      n_pos_required = np, n_neg_required = NA_integer_,
      total_n_required = tot, stringsAsFactors = FALSE)
    n_pos_req <- max(n_pos_req, np)
    total_req <- max(total_req, tot)
  }
  if (!is.null(halfwidth_sp)) {
    nn <- ceiling(z^2 * specificity * (1 - specificity) /
                    halfwidth_sp^2)
    tot <- ceiling(nn / (1 - prevalence))
    rows$sp <- data.frame(
      target = "specificity", halfwidth = halfwidth_sp,
      n_pos_required = NA_integer_, n_neg_required = nn,
      total_n_required = tot, stringsAsFactors = FALSE)
    n_neg_req <- max(n_neg_req, nn)
    total_req <- max(total_req, tot)
  }
  if (!is.null(target_kappa_halfwidth)) {
    nk <- ceiling(z^2 * kappa * (1 - kappa) /
                    target_kappa_halfwidth^2)
    rows$kappa <- data.frame(
      target = "kappa (APPROXIMATE)",
      halfwidth = target_kappa_halfwidth,
      n_pos_required = NA_integer_, n_neg_required = NA_integer_,
      total_n_required = nk, stringsAsFactors = FALSE)
    total_req <- max(total_req, nk)
  }
  tbl <- do.call(rbind, rows)
  rownames(tbl) <- NULL
  exp_pos <- ceiling(total_req * prevalence)
  exp_neg <- total_req - exp_pos
  assumptions <- c(
    sprintf(paste0("Normal-approximation planning: ",
                   "n = z^2 p(1-p)/w^2, z = %.4f ",
                   "(conf_level=%.2f)."), z, conf_level),
    paste0("Total n for sensitivity = ceil(n_pos / prevalence); ",
           "for specificity = ceil(n_neg / (1-prevalence)); ",
           "required total is the max over targets."),
    if (!is.null(target_kappa_halfwidth)) {
      paste0("Kappa row is APPROXIMATE: variance treated as ",
             "kappa(1-kappa)/n; true kappa variance depends on ",
             "prevalence and marginals.")
    } else {
      NULL
    },
    sprintf(paste0("At total_n=%d and prevalence=%.3f, expected ",
                   "positives=%d, negatives=%d."),
            total_req, prevalence, exp_pos, exp_neg),
    paste0("Planning uses the normal (Wald) approximation; ",
           "judge_audit() reports Wilson score intervals, so ",
           "the achieved interval width can differ slightly ",
           "from the target."),
    if (exp_pos == 0 || exp_neg == 0) {
      paste0("WARNING: at this total and prevalence the expected ",
             "count of one class is 0; the required total is ",
             "driven entirely by the other class's target.")
    } else {
      NULL
    }
  )
  out <- list(
    table = tbl,
    n_pos = if (n_pos_req > 0) n_pos_req else NA_integer_,
    n_neg = if (n_neg_req > 0) n_neg_req else NA_integer_,
    total_n = total_req, expected_pos = exp_pos,
    expected_neg = exp_neg, conf_level = conf_level,
    assumptions = assumptions)
  class(out) <- "judge_design"
  return(out)
}

#' @export
print.judge_design <- function(x, ...) {
  cat("judgeaudit design\n")
  print(x$table, row.names = FALSE)
  cat("Required total audit n:", x$total_n, "\n")
  cat("Assumptions:\n")
  for (a in x$assumptions) {
    cat(" -", a, "\n")
  }
  invisible(x)
}
