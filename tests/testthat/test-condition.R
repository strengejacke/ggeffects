if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("lme4") &&
  require("sjmisc")
)) {
  context("ggeffects, condition")

  data(efc)
  efc$e42dep <- to_label(efc$e42dep)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + e42dep + c172code, data = efc)

  test_that("ggpredict, condition", {
    ggpredict(fit, "c172code")
    ggpredict(fit, "c172code", condition = c(c12hour = 40))
    ggpredict(fit, "c172code", condition = c(c12hour = 40, e42dep = "severely dependent"))
    ggpredict(fit, "c172code", condition = c(e42dep = "severely dependent"))
  })

  test_that("ggemmeans, condition", {
    ggemmeans(fit, "c172code")
    ggemmeans(fit, "c172code", condition = c(c12hour = 40))
    ggemmeans(fit, "c172code", condition = c(c12hour = 40, e42dep = "severely dependent"))
    ggemmeans(fit, "c172code", condition = c(e42dep = "severely dependent"))
  })


  efc$neg_c_7d <- dicho(efc$neg_c_7)

  m1 <- glm(
    neg_c_7d ~ c12hour + e42dep + c161sex + c172code,
    data = efc,
    family = binomial(link = "logit")
  )

  test_that("ggpredict, glm", {
    ggpredict(m1, "c12hour", condition = c(e42dep = "severely dependent"))
    ggpredict(m1, c("c12hour", "c161sex"), condition = c(e42dep = "severely dependent"))
    ggpredict(m1, c("c12hour", "c161sex", "c172code"), condition = c(e42dep = "severely dependent"))
  })

  test_that("ggpredict, glm", {
    ggemmeans(m1, "c12hour", condition = c(e42dep = "severely dependent"))
    ggemmeans(m1, c("c12hour", "c161sex"), condition = c(e42dep = "severely dependent"))
    ggemmeans(m1, c("c12hour", "c161sex", "c172code"), condition = c(e42dep = "severely dependent"))
  })


  efc$neg_c_7d <- dicho(efc$neg_c_7)

  m2 <- glm(
    neg_c_7d ~ c12hour + e42dep + c161sex + c172code,
    data = efc,
    family = binomial(link = "logit")
  )

  test_that("ggpredict, glm", {
    ggpredict(m2, "c12hour", condition = c(c172code = 1))
    ggpredict(m2, c("c12hour", "c161sex"), condition = c(c172code = 2))
  })


  data(efc)
  efc$grp <- to_label(efc$e15relat)
  efc$e42dep <- to_label(efc$e42dep)

  m3 <- lmer(neg_c_7 ~ c12hour + e42dep + c161sex + c172code + (1|grp), data = efc)

  test_that("ggpredict, condition-lmer", {
    pr <- ggpredict(m3, "c12hour", type = "re")
    expect_equal(pr$predicted[1], 8.962075, tolerance = 1e-3)
    expect_equal(pr$std.error[1], 0.7345163, tolerance = 1e-3)

    pr <- ggpredict(m3, "c12hour", type = "re", condition = c(c172code = 1))
    expect_equal(pr$predicted[1], 8.62045, tolerance = 1e-3)
    expect_equal(pr$std.error[1], 0.75549, tolerance = 1e-3)

    pr <- ggpredict(m3, "c12hour", type = "re", condition = c(e42dep = "severely dependent"))
    expect_equal(pr$predicted[1], 12.83257, tolerance = 1e-3)
    expect_equal(pr$std.error[1], 0.7345163, tolerance = 1e-3)

    pr <- ggpredict(m3, "c12hour", type = "re", condition = c(e42dep = "severely dependent", c172code = 3))
    expect_equal(pr$predicted[1], 13.19621, tolerance = 1e-3)
    expect_equal(pr$std.error[1], 0.7667454, tolerance = 1e-3)

    pr <- ggpredict(m3, "c12hour", type = "re", condition = c(e42dep = "severely dependent", c172code = 3, grp = "sibling"))
    expect_equal(pr$predicted[1], 13.13315, tolerance = 1e-3)
    expect_equal(pr$std.error[1], 0.76675, tolerance = 1e-3)

    pr <- ggpredict(m3, "c12hour", type = "re", condition = c(c172code = 3, grp = "sibling"))
    expect_equal(pr$predicted[1], 9.26265, tolerance = 1e-3)
    expect_equal(pr$std.error[1], 0.76675, tolerance = 1e-3)

    pr <- ggpredict(m3, "c12hour", type = "re", condition = c(grp = "sibling"))
    expect_equal(pr$predicted[1], 8.89902, tolerance = 1e-3)
    expect_equal(pr$std.error[1], 0.73452, tolerance = 1e-3)
  })
}
