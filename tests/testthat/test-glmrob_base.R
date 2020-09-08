if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjmisc") &&
  require("robustbase")
)) {

  context("ggeffects, glmrob")

  data(efc)
  efc$neg_c_7d <- dicho(efc$neg_c_7)
  m1 <- glmrob(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = efc, family = binomial)

  test_that("ggpredict, lrm", {
    pr <- ggpredict(m1, "c12hour")
    expect_equal(pr$predicted[1], 0.4035267, tolerance = 1e-4)
  })

  test_that("ggemmeans, lrm", {
    expect_null(ggemmeans(m1, "c12hour"))
  })
}
