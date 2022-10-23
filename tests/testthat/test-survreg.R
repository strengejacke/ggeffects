if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("emmeans") &&
  require("effects") &&
  require("survival") &&
  getRversion() >= "3.6.0" &&
  packageVersion("survival") >= "3.2.9"
)) {

  m1 <- survreg(Surv(futime, fustat) ~ ecog.ps + rx, data = ovarian, dist = "exponential")

  test_that("ggpredict, survreg", {
    pr <- ggpredict(m1, "ecog.ps")
    expect_equal(pr$predicted[1], 1637.551, tolerance = 1e-4)
  })

  test_that("ggeffect, survreg", {
    expect_null(ggeffect(m1, "ecog.ps"))
  })

  test_that("ggemmeans, survreg", {
    pr <- ggemmeans(m1, "ecog.ps")
    expect_equal(pr$predicted[1], 1637.551, tolerance = 1e-4)
  })
}
