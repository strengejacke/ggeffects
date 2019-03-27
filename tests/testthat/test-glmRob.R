if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjmisc") &&
  require("robust")
)) {

  context("ggeffects, lmRob")

  data(efc)
  m1 <- lmRob(neg_c_7 ~ c12hour + e42dep + c161sex + c172code, data = efc)

  test_that("ggpredict, lrm", {
    pr <- ggpredict(m1, "c12hour")
    expect_equal(pr$predicted[1], 10.94927, tolerance = 1e-4)
  })

  test_that("ggeffect, lrm", {
    expect_error(ggeffect(m1, "c12hour"))
  })

  test_that("ggemmeans, lrm", {
    expect_error(ggemmeans(m1, "c12hour"))
  })
}
