if (require("testthat") && require("ggeffects") && require("emmeans") && require("survival") && packageVersion("survival") >= "3.2.9") {

  data("lung")
  m1 <- survival::coxph(survival::Surv(time, status) ~ sex + age + ph.ecog, data = lung)

  test_that("ggpredict", {
    p <- ggpredict(m1, "sex")
    expect_equal(p$predicted[1], 1, tolerance = 1e-2)
    ggpredict(m1, c("sex", "age"))
  })

  test_that("ggemmeans", {
    if (packageVersion("emmeans") > "1.4.5") {
      p <- ggemmeans(m1, "sex")
      expect_equal(p$predicted[1], 0.7521603, tolerance = 1e-2)
      ggemmeans(m1, c("sex", "age"))
    }
  })

  test_that("ggpredict", {
    p <- ggpredict(m1, "sex", type = "surv")
    expect_equal(p$predicted[1], 0.9966796, tolerance = 1e-2)
    p <- ggpredict(m1, "sex", type = "cumhaz")
    expect_equal(p$predicted[1], 0.003325958, tolerance = 1e-2)
  })
}
