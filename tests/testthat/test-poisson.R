context("ggeffects, poisson model")

library(ggeffects)
library(sjmisc)
library(sjlabelled)


# glm, poisson regression ----
data(efc)
fit <- glm(tot_sc_e ~ neg_c_7 + c12hour + e42dep + c161sex + c172code, data = efc, family = poisson(link = "log"))

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
