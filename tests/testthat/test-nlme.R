context("ggeffects, lme")

library(ggeffects)
library(sjmisc)
library(sjlabelled)


# lme ----

library(nlme)
fit <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)

test_that("ggpredict, lme", {
  ggpredict(fit, "age")
  ggpredict(fit, c("age", "Sex"))
})

test_that("ggaverage, lme", {
  ggaverage(fit, "age")
  ggaverage(fit, c("age", "Sex"))
})

test_that("ggalleffects, lme", {
  ggalleffects(fit, "age")
  ggalleffects(fit, c("age", "Sex"))
  ggalleffects(fit)
})

test_that("ggeffect, lme", {
  ggeffect(fit, "age")
  ggeffect(fit, c("age", "Sex"))
})


library(nlme)
data(Orthodont)

m5 <- lmer(distance ~ age * Sex + (age|Subject), data = Orthodont)
m6 <- lme(distance ~ age * Sex, data = Orthodont, random = ~ age | Subject)

test_that("ggpredict, lme", {
  ggpredict(m5, c("age", "Sex"))
  ggpredict(m6, c("age", "Sex"))
})
