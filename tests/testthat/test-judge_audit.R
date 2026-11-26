test_that("perfect agreement gives agreement and kappa of 1", {
  h <- rep(c(1, 0), each = 50)
  perfect <- judge_audit(h, h)
  expect_equal(perfect$agreement, 1)
  expect_equal(perfect$kappa$estimate, 1)
  expect_equal(perfect$sensitivity$estimate, 1)
  expect_equal(perfect$specificity$estimate, 1)
  expect_s3_class(perfect, "judge_audit")
})

test_that("audit recovers constructed Se and Sp", {
  h <- rep(c(1, 0), each = 50)
  j2 <- h
  j2[1:10] <- 0
  j2[51:55] <- 1 # Se = 0.8, Sp = 0.9 by construction
  a2 <- judge_audit(h, j2)
  expect_equal(a2$sensitivity$estimate, 0.8)
  expect_equal(a2$specificity$estimate, 0.9)
  expect_lt(a2$sensitivity$lower, 0.8)
  expect_gt(a2$sensitivity$upper, 0.8)
})

test_that("kappa-paradox warning fires at extreme prevalence", {
  hh <- c(rep(1, 5), rep(0, 95))
  jj <- hh
  jj[1:3] <- 0
  jj[6:7] <- 1
  a3 <- judge_audit(hh, jj)
  expect_true(a3$kappa_paradox_warning)
})

test_that("factor input matches numeric input", {
  h <- rep(c(1, 0), each = 50)
  j2 <- h
  j2[1:10] <- 0
  j2[51:55] <- 1
  a2 <- judge_audit(h, j2)
  a4 <- judge_audit(factor(h, levels = c(0, 1)),
                    factor(j2, levels = c(0, 1)))
  expect_equal(a4$kappa$estimate, a2$kappa$estimate)
})

test_that("audit rejects invalid inputs", {
  expect_error(judge_audit(c(1, 0, 2, 1), c(1, 0, 1, 1)))
  expect_error(judge_audit(1:3, 1:4))
  expect_error(judge_audit(c(1, 0, NA, 1), c(1, 0, 1, 1)))
})

# Mirrors the standalone runner's fix-cycle regression checks
# (tests/run_tests.R) so testthat::test_check runs them too.
test_that("kappa CI is clamped to [-1, 1]", {
  h1 <- rep(c(1, 0), each = 50)
  j1 <- h1
  j1[1] <- 0
  a_k <- judge_audit(h1, j1)
  expect_lte(a_k$kappa$upper, 1)
  expect_gte(a_k$kappa$lower, -1)
})

test_that("audit warns when a class is absent", {
  expect_warning(judge_audit(rep(0, 20), rep(0, 20)))
})

# Fix cycle 3 (Gemini UI seat, cycle-2 build): the paradox is
# HIGH agreement with low kappa; low agreement must not flag.
test_that("no paradox flag at low agreement with negative kappa", {
  h_low <- c(rep(1, 10), rep(0, 190))
  j_low <- c(rep(1, 2), rep(0, 8), rep(1, 97), rep(0, 93))
  a_low <- judge_audit(h_low, j_low)
  expect_equal(a_low$agreement, 0.475)
  expect_lt(a_low$kappa$estimate, 0)
  expect_false(a_low$kappa_paradox_warning)
})

# Fix cycle 2 (Gemini UI seat retest): factor alignment.
test_that("mismatched factor levels error instead of silent recoding", {
  h <- rep(c(1, 0), each = 50)
  j2 <- h
  j2[1:10] <- 0
  j2[51:55] <- 1
  expect_error(judge_audit(factor(h, levels = c(0, 1)),
                           factor(j2, levels = c(1, 0))),
               "identical levels")
  expect_error(judge_audit(factor(ifelse(h == 1, "yes", "no")),
                           factor(j2, levels = c(0, 1))),
               "identical levels")
  one <- judge_audit(factor(h, levels = c(0, 1)), j2)
  a2 <- judge_audit(h, j2)
  expect_equal(one$kappa$estimate, a2$kappa$estimate)
})
