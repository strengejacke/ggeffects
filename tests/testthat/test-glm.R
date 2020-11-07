if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("lme4") &&
  require("sjlabelled") &&
  require("sjmisc")
)) {

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
    expect_is(ggpredict(fit, "c12hour"), "data.frame")
    expect_is(ggpredict(fit, c("c12hour", "c161sex")), "data.frame")
    expect_is(ggpredict(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
  })

  test_that("ggeffect, glm", {
    expect_is(ggeffect(fit, "c12hour"), "data.frame")
    expect_is(ggeffect(fit, c("c12hour", "c161sex")), "data.frame")
    expect_is(ggeffect(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
  })

  test_that("ggemmeans, glm", {
    expect_is(ggemmeans(fit, "c12hour"), "data.frame")
    expect_is(ggemmeans(fit, c("c12hour", "c161sex")), "data.frame")
    expect_is(ggemmeans(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
  })

  test_that("ggeffects, glm", {
    p1 <- ggpredict(m, "period")
    p2 <- ggeffect(m, "period")
    p3 <- ggemmeans(m, "period")

    expect_equal(p1$predicted[1], 0.2194245, tolerance = 1e-3)
    expect_equal(p2$predicted[1], 0.2194245, tolerance = 1e-3)
    expect_equal(p3$predicted[1], 0.2194245, tolerance = 1e-3)
  })

  test_that("ggpredict, glm, robust", {
    expect_is(ggpredict(fit, "c12hour", vcov.fun = "vcovHC", vcov.type = "HC1"), "data.frame")
    expect_is(ggpredict(fit, c("c12hour", "c161sex"), vcov.fun = "vcovHC", vcov.type = "HC1"), "data.frame")
    expect_is(ggpredict(fit, c("c12hour", "c161sex", "c172code"), vcov.fun = "vcovHC", vcov.type = "HC1"), "data.frame")
  })

  test_that("ggeffects, glm, robust", {
    expect_is(ggpredict(m, "period", vcov.fun = "vcovHC", vcov.type = "HC1"), "data.frame")
  })


  data(cbpp)
  cbpp$trials <- cbpp$size - cbpp$incidence

  m1 <- glmer(cbind(incidence, trials) ~ period + (1 | herd), data = cbpp, family = binomial)
  m2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), data = cbpp, family = binomial)
  m3 <- glm(cbind(incidence, trials) ~ period, data = cbpp, family = binomial)
  m4 <- glm(cbind(incidence, size - incidence) ~ period, data = cbpp, family = binomial)

  test_that("ggeffects, glm-matrix-columns", {
    expect_is(ggpredict(m1, "period"), "data.frame")
    expect_is(ggpredict(m2, "period"), "data.frame")
    expect_is(ggpredict(m3, "period"), "data.frame")
    expect_is(ggpredict(m4, "period"), "data.frame")
    expect_is(ggemmeans(m1, "period"), "data.frame")
    expect_is(ggemmeans(m2, "period"), "data.frame")
    expect_is(ggemmeans(m3, "period"), "data.frame")
    expect_is(ggemmeans(m4, "period"), "data.frame")
  })
}
