if (require("testthat") && require("ggeffects") && require("emmeans") && require("survival")) {

  data("cancer", package = "survival")
  lung <- as.data.frame(lung)
  # remove category 3 (outlier)
  lung <- subset(lung, subset = ph.ecog %in% 0:2)
  lung$sex <- factor(lung$sex, labels = c("male", "female"))
  lung$ph.ecog <- factor(lung$ph.ecog, labels = c("good", "ok", "limited"))

  m1 <- survival::coxph(survival::Surv(time, status) ~ sex + age + ph.ecog, data = lung)

  test_that("ggpredict", {
    p <- ggpredict(m1, "sex")
    expect_equal(p$predicted[1], 1, tolerance = 1e-3)
    ggpredict(m1, c("sex", "age"))
  })

  test_that("ggemmeans", {
    if (packageVersion("emmeans") > "1.4.5") {
      p <- ggemmeans(m1, "sex")
      expect_equal(p$predicted[1], 0.7521603, tolerance = 1e-3)
      ggemmeans(m1, c("sex", "age"))
    }
  })

  test_that("ggpredict", {
    p <- ggpredict(m1, "sex", type = "surv")
    expect_equal(p$predicted[1], 0.9966796, tolerance = 1e-3)
    p <- ggpredict(m1, "sex", type = "cumhaz")
    expect_equal(p$predicted[1], 0.003325958, tolerance = 1e-3)
  })
}
