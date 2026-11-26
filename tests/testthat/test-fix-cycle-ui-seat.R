# Tests added 2026-11-24 for the Gemini UI seat fix cycle:
# edge cases and print methods the original suite did not cover.

test_that("design rejects NA or vector target_kappa_halfwidth cleanly", {
  expect_error(judge_design(0.85, 0.90, 0.30, kappa = 0.7,
                            target_kappa_halfwidth = NA_real_),
               "single number")
  expect_error(judge_design(0.85, 0.90, 0.30, kappa = 0.7,
                            target_kappa_halfwidth = c(0.05, 0.05)),
               "single number")
  expect_error(judge_design(0.85, 0.90, 0.30, kappa = 0.7,
                            target_kappa_halfwidth = "wide"),
               "single number")
})

test_that("design assumptions name the Wald-vs-Wilson width caveat", {
  d1 <- judge_design(0.85, 0.90, 0.30,
                     halfwidth_se = 0.05, halfwidth_sp = 0.05)
  expect_true(any(grepl("Wilson", d1$assumptions)))
})

test_that("design warns when an expected class count is zero", {
  d_small <- judge_design(0.85, 0.90, 0.99, halfwidth_se = 0.40)
  expect_equal(d_small$expected_neg, 0)
  expect_true(any(grepl("expected count of one class is 0",
                        d_small$assumptions)))
})

test_that("kappa CI is clamped to [-1, 1]", {
  h <- rep(c(1, 0), each = 50)
  j <- h
  j[1] <- 0 # one disagreement: raw Wald upper exceeds 1
  a <- judge_audit(h, j)
  expect_lte(a$kappa$upper, 1)
  expect_equal(a$kappa$upper, 1)
  expect_gte(a$kappa$lower, -1)
})

test_that("absent class warns and yields NA sensitivity", {
  expect_warning(a0 <- judge_audit(rep(0, 20), rep(0, 20)),
                 "sensitivity is undefined")
  expect_true(is.na(a0$sensitivity$estimate))
  expect_true(is.na(a0$kappa$estimate))
})

test_that("print.judge_audit handles NA kappa explicitly", {
  a0 <- suppressWarnings(judge_audit(rep(0, 20), rep(0, 20)))
  out <- capture.output(print(a0))
  expect_true(any(grepl("Kappa=NA \\(undefined", out)))
})

test_that("paradox check also fires on extreme judge prevalence", {
  # Updated in fix cycle 3: the paradox requires HIGH agreement
  # (po >= 0.8), which the original case (agreement 0.52) no
  # longer satisfies by design. This case keeps the intent -
  # extreme judge prevalence (0.09) with non-extreme human
  # prevalence (0.25) - at agreement 0.84.
  h <- c(rep(1, 250), rep(0, 750))
  j <- rep(0, 1000)
  j[1:90] <- 1 # TP = 90, judge prevalence 0.09
  a <- judge_audit(h, j)
  expect_gte(a$agreement, 0.8)
  expect_true(a$kappa_paradox_warning)
})

test_that("audit accepts logical input", {
  h <- rep(c(TRUE, FALSE), each = 10)
  a <- judge_audit(h, h)
  expect_equal(a$agreement, 1)
})

test_that("adjust rejects non-integer counts", {
  expect_error(judge_adjust(0.325, 0.85, 0.90, n_main = 2000,
                            n_audit_pos = 196.5),
               "whole number")
  expect_error(judge_adjust(0.325, 0.85, 0.90, n_main = 2000.5),
               "whole number")
})

test_that("adjust CI bounds are clamped to [0, 1]", {
  lo <- judge_adjust(apparent = 0.02, sensitivity = 0.60,
                     specificity = 0.60, n_main = 100,
                     n_audit_pos = 20, n_audit_neg = 20)
  expect_true(lo$valid)
  expect_equal(lo$adjusted, 0)
  expect_gte(lo$lower, 0)
  hi <- judge_adjust(apparent = 0.98, sensitivity = 0.60,
                     specificity = 0.60, n_main = 100,
                     n_audit_pos = 20, n_audit_neg = 20)
  expect_equal(hi$adjusted, 1)
  expect_lte(hi$upper, 1)
})

test_that("adjust with NULL audit counts notes fixed Se/Sp and clamping", {
  adj <- judge_adjust(0.325, 0.85, 0.90, n_main = 2000)
  expect_true(adj$valid)
  expect_true(grepl("treated as fixed", adj$note))
  expect_true(grepl("clamped to \\[0,1\\]", adj$note))
})

test_that("print methods produce their headers", {
  d1 <- judge_design(0.85, 0.90, 0.30,
                     halfwidth_se = 0.05, halfwidth_sp = 0.05)
  expect_true(any(grepl("Required total audit n",
                        capture.output(print(d1)))))
  h <- rep(c(1, 0), each = 50)
  expect_true(any(grepl("judgeaudit audit",
                        capture.output(print(judge_audit(h, h))))))
  adj <- judge_adjust(0.325, 0.85, 0.90, n_main = 2000,
                      n_audit_pos = 196, n_audit_neg = 139)
  expect_true(any(grepl("Adjusted prevalence",
                        capture.output(print(adj)))))
})
