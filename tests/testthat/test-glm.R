context("ggeffects, logistic regression")

library(ggeffects)
library(sjmisc)
library(sjlabelled)
library(lme4)

# glm, logistic regression ----
data(efc)
efc$neg_c_7d <- dicho(efc$neg_c_7)
fit <- glm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = efc, family = binomial(link = "logit"))

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

test_that("ggalleffects, glm", {
  ggalleffects(fit, "c12hour")
  ggalleffects(fit, c("c12hour", "c161sex"))
  ggalleffects(fit, c("c12hour", "c161sex", "c172code"))
  ggalleffects(fit)
})

test_that("ggeffect, glm", {
  ggeffect(fit, "c12hour")
  ggeffect(fit, c("c12hour", "c161sex"))
  ggeffect(fit, c("c12hour", "c161sex", "c172code"))
})

test_that("ggeffects, glm", {
  m <- glm(
    cbind(incidence, size - incidence) ~ period,
    family = binomial,
    data = lme4::cbpp
  )
  ggpredict(m, "period")
  ggeffect(m, "period")
  ggalleffects(m)
})
