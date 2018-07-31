stopifnot(require("testthat"),
          require("ggeffects"),
          require("nlme"),
          require("sjlabelled"),
          require("sjmisc"))

context("ggeffects, lme")

# lme ----

fit <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)

test_that("ggpredict, lme", {
  ggpredict(fit, "age")
  ggpredict(fit, c("age", "Sex"))
  ggpredict(fit, "age", type = "re")
  ggpredict(fit, c("age", "Sex"), type = "re")
})

test_that("ggaverage, lme", {
  ggaverage(fit, "age")
  ggaverage(fit, c("age", "Sex"))
})

test_that("ggeffect, lme", {
  ggeffect(fit, "age")
  ggeffect(fit, c("age", "Sex"))
})


data(Orthodont)

m5 <- lmer(distance ~ age * Sex + (age|Subject), data = Orthodont)
m6 <- lme(distance ~ age * Sex, data = Orthodont, random = ~ age | Subject)

test_that("ggpredict, lme", {
  ggpredict(m5, c("age", "Sex"))
  ggpredict(m6, c("age", "Sex"))

  ggpredict(m5, c("age", "Sex"), type = "re")
  ggpredict(m6, c("age", "Sex"), type = "re")
})
