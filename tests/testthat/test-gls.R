if (requiet("testthat") &&
    requiet("ggeffects") &&
    requiet("emmeans") &&
    requiet("effects") &&
    requiet("nlme")) {

  data(Ovary, package = "nlme")
  m1 <- nlme::gls(follicles ~ Time, Ovary, correlation = corAR1(form = ~ 1 | Mare))

  test_that("ggpredict", {
    p <- ggpredict(m1, "Time")
    expect_equal(p$predicted[1], 11.49246, tolerance = 1e-3)
  })

  test_that("ggeffect", {
    p <- ggeffect(m1, "Time")
    expect_equal(p$predicted[1], 11.49246, tolerance = 1e-3)
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, "Time")
    expect_equal(p$predicted[1], 11.49246, tolerance = 1e-3)
  })
}
