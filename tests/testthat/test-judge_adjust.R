test_that("Rogan-Gladen recovers true prevalence exactly", {
  # apparent for p = 0.30: Se*p + (1-Sp)(1-p) = .255 + .07 = .325
  adj2 <- judge_adjust(apparent = 0.325, sensitivity = 0.85,
                       specificity = 0.90, n_main = 2000,
                       n_audit_pos = 196, n_audit_neg = 139)
  expect_true(adj2$valid)
  expect_equal(adj2$adjusted_raw, 0.30)
  expect_lt(adj2$lower, 0.30)
  expect_gt(adj2$upper, 0.30)
  expect_s3_class(adj2, "judge_adjust")
})

test_that("Se + Sp <= 1 returns valid = FALSE with a warning", {
  bad <- suppressWarnings(
    judge_adjust(apparent = 0.4, sensitivity = 0.5,
                 specificity = 0.5, n_main = 1000))
  expect_false(bad$valid)
  expect_true(is.na(bad$adjusted))
  expect_warning(
    judge_adjust(0.4, 0.4, 0.5, n_main = 100))
})

test_that("adjust rejects invalid inputs", {
  expect_error(judge_adjust(1.5, 0.85, 0.9, n_main = 100))
  expect_error(judge_adjust(0.4, 0.85, 0.9, n_main = 0))
})

# Mirrors the standalone runner's fix-cycle regression checks
# (tests/run_tests.R) so testthat::test_check runs them too.
test_that("adjust rejects non-integer audit counts", {
  expect_error(judge_adjust(0.325, 0.85, 0.9, n_main = 2000,
                            n_audit_pos = 196.5))
})

test_that("adjust CI bounds are clamped to [0, 1]", {
  adj_b <- judge_adjust(apparent = 0.02, sensitivity = 0.60,
                        specificity = 0.60, n_main = 100,
                        n_audit_pos = 20, n_audit_neg = 20)
  expect_gte(adj_b$lower, 0)
  expect_lte(adj_b$upper, 1)
})

# Fix cycle 2 (Gemini UI seat retest): whole-interval clamping.
test_that("adjust interval never inverts outside [0, 1]", {
  neg <- judge_adjust(0.0625, 0.85, 0.90, n_main = 1000000,
                      n_audit_pos = 100000, n_audit_neg = 100000)
  expect_lt(neg$adjusted_raw, 0)
  expect_equal(neg$lower, 0)
  expect_equal(neg$upper, 0)
  expect_lte(neg$lower, neg$upper)
  hi <- judge_adjust(0.95, 0.85, 0.90, n_main = 1000000,
                     n_audit_pos = 100000, n_audit_neg = 100000)
  expect_gt(hi$adjusted_raw, 1)
  expect_equal(hi$lower, 1)
  expect_equal(hi$upper, 1)
  expect_lte(hi$lower, hi$upper)
})

# Fix cycle 3 (Gemini UI seat, cycle-2 build): finite counts.
test_that("adjust rejects non-finite counts", {
  expect_error(judge_adjust(0.325, 0.85, 0.90, n_main = Inf),
               "positive whole number")
  expect_error(judge_adjust(0.325, 0.85, 0.90, n_main = 2000,
                            n_audit_pos = Inf),
               "positive whole number")
})

test_that("adjust warns when audit counts sum above n_main", {
  expect_warning(judge_adjust(0.325, 0.85, 0.90, n_main = 300,
                              n_audit_pos = 196, n_audit_neg = 139),
                 "exceeds n_main")
  expect_no_warning(judge_adjust(0.325, 0.85, 0.90, n_main = 2000,
                                 n_audit_pos = 196, n_audit_neg = 139))
})
