# Simple stopifnot test runner (no testthat dependency required to run).
# Run: Rscript tests/run_tests.R  from the package root, or via R CMD check.
# Load the code under test: source R/ when run from the package root;
# otherwise (e.g. under R CMD check, where tests run in a sandbox) load the
# installed package.
if (dir.exists("R")) {
  for (f in list.files("R", full.names = TRUE, pattern = "[.]R$")) source(f)
} else if (dir.exists("../R")) {
  for (f in list.files("../R", full.names = TRUE, pattern = "[.]R$")) source(f)
} else {
  library(judgeaudit)
}
pass <- 0L; check <- function(cond, label) {
  if (!isTRUE(cond)) stop("FAIL: ", label, call. = FALSE)
  pass <<- pass + 1L; cat("PASS:", label, "\n")
}
expect_error <- function(expr, label) {
  ok <- tryCatch({ expr; FALSE }, error = function(e) TRUE)
  check(ok, label)
}

## --- judge_design ---
d1 <- judge_design(0.85, 0.90, 0.30, halfwidth_se = 0.05, halfwidth_sp = 0.05)
check(is.data.frame(d1$table) && d1$total_n >= 1, "design returns table and total")
# n_pos for Se: ceil(1.96^2*.85*.15/.05^2) = ceil(195.9) = 196
check(d1$n_pos == 196L || d1$n_pos == 196, "design n_pos matches hand calculation (196)")
d_narrow <- judge_design(0.85, 0.90, 0.30, halfwidth_se = 0.025, halfwidth_sp = 0.05)
check(d_narrow$total_n > d1$total_n, "design monotonic: narrower width needs larger n")
d_wide <- judge_design(0.85, 0.90, 0.30, halfwidth_se = 0.10, halfwidth_sp = 0.05)
check(d_wide$total_n < d1$total_n, "design monotonic: wider width needs smaller n")
d_full <- judge_design(0.85, 0.90, 0.30, width_se = 0.10)
check(identical(d_full$n_pos, d1$n_pos) || d_full$n_pos == d1$n_pos, "full width 0.10 == halfwidth 0.05")
d_k <- judge_design(0.85, 0.90, 0.30, kappa = 0.7, target_kappa_halfwidth = 0.05)
check(any(grepl("APPROXIMATE", d_k$table$target)), "kappa row labelled approximate")
expect_error(judge_design(1.2, 0.9, 0.3, halfwidth_se = 0.05), "design rejects sensitivity > 1")
expect_error(judge_design(0.85, 0.9, 0.3), "design requires at least one target")

## --- judge_audit ---
h <- rep(c(1, 0), each = 50); perfect <- judge_audit(h, h)
check(perfect$agreement == 1 && perfect$kappa$estimate == 1, "perfect agreement: agreement=kappa=1")
check(perfect$sensitivity$estimate == 1 && perfect$specificity$estimate == 1, "perfect agreement: Se=Sp=1")
j2 <- h; j2[1:10] <- 0; j2[51:55] <- 1  # Se=0.8, Sp=0.9 by construction
a2 <- judge_audit(h, j2)
check(abs(a2$sensitivity$estimate - 0.8) < 1e-9, "audit recovers Se=0.8")
check(abs(a2$specificity$estimate - 0.9) < 1e-9, "audit recovers Sp=0.9")
check(a2$sensitivity$lower < 0.8 && a2$sensitivity$upper > 0.8, "Wilson CI covers point estimate")
# Kappa paradox: prevalence 0.05, agreement 0.95 but disagreements concentrated
set.seed(1); hh <- c(rep(1, 5), rep(0, 95)); jj <- hh; jj[1:3] <- 0; jj[6:7] <- 1
a3 <- judge_audit(hh, jj)
check(isTRUE(a3$kappa_paradox_warning), "kappa-paradox warning fires at extreme prevalence")
a4 <- judge_audit(factor(h, levels = c(0, 1)), factor(j2, levels = c(0, 1)))
check(abs(a4$kappa$estimate - a2$kappa$estimate) < 1e-9, "factor input matches numeric input")
expect_error(judge_audit(c(1, 0, 2, 1), c(1, 0, 1, 1)), "audit rejects non-0/1 labels")
expect_error(judge_audit(1:3, 1:4), "audit rejects length mismatch")

## --- judge_adjust ---
adj <- judge_adjust(apparent = 0.345, sensitivity = 0.85, specificity = 0.90,
                    n_main = 2000, n_audit_pos = 196, n_audit_neg = 139)
# True prevalence used to generate apparent: p_app = Se*p + (1-Sp)(1-p); p=0.30 -> 0.255+0.07=0.325
adj2 <- judge_adjust(apparent = 0.325, sensitivity = 0.85, specificity = 0.90,
                     n_main = 2000, n_audit_pos = 196, n_audit_neg = 139)
check(isTRUE(adj$valid) && abs(adj$adjusted_raw - 0.245/0.75) < 1e-9, "adjust at apparent 0.345 gives raw 0.3267 (0.245/0.75)")
check(isTRUE(adj2$valid) && abs(adj2$adjusted_raw - 0.30) < 1e-9, "Rogan-Gladen recovers true prevalence 0.30 exactly at true Se/Sp")
check(adj2$lower < 0.30 && adj2$upper > 0.30, "adjusted CI is non-degenerate around estimate")
bad <- suppressWarnings(judge_adjust(apparent = 0.4, sensitivity = 0.5, specificity = 0.5, n_main = 1000))
check(!isTRUE(bad$valid) && is.na(bad$adjusted), "Se+Sp<=1 returns valid=FALSE and NA")
warned <- tryCatch({ judge_adjust(0.4, 0.4, 0.5, n_main = 100); FALSE }, warning = function(w) TRUE)
check(warned, "Se+Sp<=1 issues a warning")
expect_error(judge_adjust(1.5, 0.85, 0.9, n_main = 100), "adjust rejects apparent > 1")

## --- fix-cycle additions (Gemini UI seat, 2026-11-24) ---
expect_error(judge_design(0.85, 0.9, 0.3, kappa = 0.7,
                          target_kappa_halfwidth = NA_real_),
             "design rejects NA target_kappa_halfwidth cleanly")
expect_error(judge_design(0.85, 0.9, 0.3, kappa = 0.7,
                          target_kappa_halfwidth = c(0.05, 0.05)),
             "design rejects vector target_kappa_halfwidth cleanly")
h1 <- rep(c(1, 0), each = 50); j1 <- h1; j1[1] <- 0
a_k <- judge_audit(h1, j1)
check(a_k$kappa$upper <= 1 && a_k$kappa$lower >= -1,
      "kappa CI clamped to [-1, 1]")
w_absent <- tryCatch({ judge_audit(rep(0, 20), rep(0, 20)); FALSE },
                     warning = function(w) TRUE)
check(w_absent, "audit warns when a class is absent (Se undefined)")
expect_error(judge_adjust(0.325, 0.85, 0.9, n_main = 2000,
                          n_audit_pos = 196.5),
             "adjust rejects non-integer audit counts")
adj_b <- judge_adjust(apparent = 0.02, sensitivity = 0.60,
                      specificity = 0.60, n_main = 100,
                      n_audit_pos = 20, n_audit_neg = 20)
check(adj_b$lower >= 0 && adj_b$upper <= 1,
      "adjust CI bounds clamped to [0, 1]")

## --- fix-cycle 2 additions (Gemini UI seat retest, 2026-11-24) ---
# Inverted interval: whole raw interval below 0 must give [0, 0],
# never lower = 0 > upper < 0.
adj_neg <- judge_adjust(0.0625, 0.85, 0.90, n_main = 1000000,
                        n_audit_pos = 100000, n_audit_neg = 100000)
check(adj_neg$adjusted_raw < 0 && adj_neg$lower == 0 &&
      adj_neg$upper == 0 && adj_neg$lower <= adj_neg$upper,
      "adjust interval never inverts below 0 (whole-interval clamp)")
# Symmetric case: whole raw interval above 1 must give [1, 1].
adj_hi <- judge_adjust(0.95, 0.85, 0.90, n_main = 1000000,
                       n_audit_pos = 100000, n_audit_neg = 100000)
check(adj_hi$adjusted_raw > 1 && adj_hi$lower == 1 &&
      adj_hi$upper == 1 && adj_hi$lower <= adj_hi$upper,
      "adjust interval never inverts above 1 (whole-interval clamp)")
# Audit-larger-than-main warning (independence / subsample hint).
w_big <- tryCatch({ judge_adjust(0.325, 0.85, 0.9, n_main = 300,
                                 n_audit_pos = 196, n_audit_neg = 139)
                    FALSE }, warning = function(w) TRUE)
check(w_big, "adjust warns when audit counts sum above n_main")
w_ok <- tryCatch({ judge_adjust(0.325, 0.85, 0.9, n_main = 2000,
                                n_audit_pos = 196, n_audit_neg = 139)
                   FALSE }, warning = function(w) TRUE)
check(!w_ok, "adjust silent when audit counts fit inside n_main")
# Factor alignment: mismatched levels error; one factor still works.
expect_error(judge_audit(factor(h, levels = c(0, 1)),
                         factor(j2, levels = c(1, 0))),
             "audit rejects mismatched factor level order")
expect_error(judge_audit(factor(ifelse(h == 1, "yes", "no")),
                         factor(j2, levels = c(0, 1))),
             "audit rejects different factor level sets")
a_one <- judge_audit(factor(h, levels = c(0, 1)), j2)
check(abs(a_one$kappa$estimate - a2$kappa$estimate) < 1e-9,
      "single factor input still matches numeric input")

## --- fix-cycle 3 additions (Gemini UI seat, cycle-2 build, 2026-11-24) ---
expect_error(judge_adjust(0.325, 0.85, 0.9, n_main = Inf),
             "adjust rejects infinite n_main")
expect_error(judge_adjust(0.325, 0.85, 0.9, n_main = 2000,
                          n_audit_pos = Inf),
             "adjust rejects infinite audit counts")
# Kappa paradox requires HIGH agreement: extreme prevalence with
# low agreement (~0.48) and negative kappa must NOT flag.
h_low <- c(rep(1, 10), rep(0, 190))
j_low <- c(rep(1, 2), rep(0, 8), rep(1, 97), rep(0, 93))
a_low <- judge_audit(h_low, j_low)
check(a_low$kappa$estimate < 0 && !isTRUE(a_low$kappa_paradox_warning),
      "no paradox flag at low agreement with negative kappa")
# Full width is validated in (0, 1) with its own message.
expect_error(judge_design(0.85, 0.9, 0.3, width_se = 1),
             "design rejects full width of 1")
msg_w <- tryCatch({ judge_design(0.85, 0.9, 0.3, width_se = 1); "" },
                  error = function(e) conditionMessage(e))
check(grepl("(0, 1)", msg_w, fixed = TRUE),
      "width error message cites the (0, 1) range")

cat(sprintf("\nALL TESTS PASSED: %d checks\n", pass))
