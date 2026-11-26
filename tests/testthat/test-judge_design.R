test_that("design returns a table and the hand-computed n_pos", {
  d1 <- judge_design(0.85, 0.90, 0.30,
                     halfwidth_se = 0.05, halfwidth_sp = 0.05)
  expect_true(is.data.frame(d1$table))
  expect_gte(d1$total_n, 1)
  # ceil(1.96^2 * .85 * .15 / .05^2) = ceil(195.9) = 196
  expect_equal(d1$n_pos, 196)
  expect_s3_class(d1, "judge_design")
})

test_that("design is monotonic in target width", {
  d1 <- judge_design(0.85, 0.90, 0.30,
                     halfwidth_se = 0.05, halfwidth_sp = 0.05)
  d_narrow <- judge_design(0.85, 0.90, 0.30,
                           halfwidth_se = 0.025,
                           halfwidth_sp = 0.05)
  d_wide <- judge_design(0.85, 0.90, 0.30,
                         halfwidth_se = 0.10, halfwidth_sp = 0.05)
  expect_gt(d_narrow$total_n, d1$total_n)
  expect_lt(d_wide$total_n, d1$total_n)
})

test_that("full width and halfwidth agree", {
  d1 <- judge_design(0.85, 0.90, 0.30,
                     halfwidth_se = 0.05, halfwidth_sp = 0.05)
  d_full <- judge_design(0.85, 0.90, 0.30, width_se = 0.10)
  expect_equal(d_full$n_pos, d1$n_pos)
})

test_that("kappa row is labelled approximate", {
  d_k <- judge_design(0.85, 0.90, 0.30, kappa = 0.7,
                      target_kappa_halfwidth = 0.05)
  expect_true(any(grepl("APPROXIMATE", d_k$table$target)))
})

test_that("design rejects invalid inputs", {
  expect_error(judge_design(1.2, 0.9, 0.3, halfwidth_se = 0.05))
  expect_error(judge_design(0.85, 0.9, 0.3))
  expect_error(judge_design(0.85, 0.9, 0.3, halfwidth_se = 0.05,
                            width_se = 0.10))
})

# Fix cycle 3 (Gemini UI seat, cycle-2 build): full widths are
# validated in (0, 1) with a message citing that range.
test_that("design validates full width in (0, 1)", {
  expect_error(judge_design(0.85, 0.9, 0.3, width_se = 1),
               "\\(0, 1\\)")
  expect_error(judge_design(0.85, 0.9, 0.3, width_sp = 1),
               "\\(0, 1\\)")
})

# Mirrors the standalone runner's fix-cycle regression checks
# (tests/run_tests.R) so testthat::test_check runs them too.
test_that("design rejects NA or vector target_kappa_halfwidth", {
  expect_error(judge_design(0.85, 0.9, 0.3, kappa = 0.7,
                            target_kappa_halfwidth = NA_real_))
  expect_error(judge_design(0.85, 0.9, 0.3, kappa = 0.7,
                            target_kappa_halfwidth = c(0.05, 0.05)))
})
