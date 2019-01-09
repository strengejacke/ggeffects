if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("lme4") &&
  require("sjlabelled") &&
  require("sjmisc")
)) {

  context("ggeffects, logistic regression")

  # glm, logistic regression ----
  data(efc)
  efc$neg_c_7d <- dicho(efc$neg_c_7)
  fit <- glm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = efc, family = binomial(link = "logit"))

  m <- glm(
    cbind(incidence, size - incidence) ~ period,
    family = binomial,
    data = lme4::cbpp
  )

  test_that("ggpredict, glm", {
    ggpredict(fit, "c12hour")
    ggpredict(fit, c("c12hour", "c161sex"))
    ggpredict(fit, c("c12hour", "c161sex", "c172code"))
  })

  test_that("ggaverage, glm", {
    ggaverage(fit, "c12hour")
    ggaverage(fit, c("c12hour", "c161sex"))
    ggaverage(fit, c("c12hour", "c161sex", "c172code"))
  })

  test_that("ggeffect, glm", {
    ggeffect(fit, "c12hour")
    ggeffect(fit, c("c12hour", "c161sex"))
    ggeffect(fit, c("c12hour", "c161sex", "c172code"))
  })

  test_that("ggemmeans, glm", {
    ggemmeans(fit, "c12hour")
    ggemmeans(fit, c("c12hour", "c161sex"))
    ggemmeans(fit, c("c12hour", "c161sex", "c172code"))
  })

  test_that("ggeffects, glm", {
    p1 <- ggpredict(m, "period")
    p2 <- ggeffect(m, "period")
    p3 <- ggemmeans(m, "period")

    expect_equal(p1$predicted[1], 0.2194245, tolerance = 1e-5)
    expect_equal(p2$predicted[1], 0.2194245, tolerance = 1e-5)
    expect_equal(p3$predicted[1], 0.2194245, tolerance = 1e-5)
  })

  test_that("ggpredict, glm, robust", {
    ggpredict(fit, "c12hour", vcov.fun = "vcovHC", vcov.type = "HC1")
    ggpredict(fit, c("c12hour", "c161sex"), vcov.fun = "vcovHC", vcov.type = "HC1")
    ggpredict(fit, c("c12hour", "c161sex", "c172code"), vcov.fun = "vcovHC", vcov.type = "HC1")
  })

  test_that("ggeffects, glm, robust", {
    ggpredict(m, "period", vcov.fun = "vcovHC", vcov.type = "HC1")
  })


  data(cbpp)
  cbpp$trials <- cbpp$size - cbpp$incidence

  m1 <- glmer(cbind(incidence, trials) ~ period + (1 | herd), data = cbpp, family = binomial)
  m2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), data = cbpp, family = binomial)
  m3 <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
  m4 <- glm(cbind(incidence, size - incidence) ~ period, data = cbpp, family = binomial)

  test_that("ggeffects, glm-matrix-columns", {
    ggpredict(m1, "period")
    ggpredict(m2, "period")
    ggpredict(m3, "period")
    ggpredict(m4, "period")
    ggemmeans(m1, "period")
    ggemmeans(m2, "period")
    ggemmeans(m3, "period")
    ggemmeans(m4, "period")
  })
}
