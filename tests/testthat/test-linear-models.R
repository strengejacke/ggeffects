context("ggeffects, linear model")

library(ggeffects)
library(sjmisc)
library(sjlabelled)

# lm, linear regression ----

data(efc)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

test_that("ggpredict, lm", {
  ggpredict(fit, "c12hour")
  ggpredict(fit, c("c12hour", "c161sex"))
  ggpredict(fit, c("c12hour", "c161sex", "c172code"))
})

test_that("ggaverage, lm", {
  ggaverage(fit, "c12hour")
  ggaverage(fit, c("c12hour", "c161sex"))
  ggaverage(fit, c("c12hour", "c161sex", "c172code"))
})

test_that("ggalleffects, lm", {
  ggalleffects(fit, "c12hour")
  ggalleffects(fit, c("c12hour", "c161sex"))
  ggalleffects(fit, c("c12hour", "c161sex", "c172code"))
  ggalleffects(fit)
})

test_that("ggeffect, lm", {
  ggeffect(fit, "c12hour")
  ggeffect(fit, c("c12hour", "c161sex"))
  ggeffect(fit, c("c12hour", "c161sex", "c172code"))
})

data(efc)
efc$c172code <- to_label(efc$c172code)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

test_that("ggpredict, lm", {
  ggpredict(fit, "c12hour [20,30,40]")
  ggpredict(fit, "c12hour [30:60]")
  ggpredict(fit, c("c12hour  [30:60]", "c161sex", "c172code [high level of education,low level of education]"))
})

