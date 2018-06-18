context("ggeffects, polr")

library(ggeffects)
library(MASS)

options(contrasts = c("contr.treatment", "contr.poly"))
fit <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

test_that("ggpredict, polr", {
  ggpredict(fit, "Infl")
  ggpredict(fit, c("Infl", "Type"))
  ggpredict(fit, c("Infl", "Type", "Cont"))
})

test_that("ggpredict, polr", {
  ggpredict(fit, "Infl", pretty = TRUE)
  ggpredict(fit, c("Infl", "Type"), pretty = TRUE)
  ggpredict(fit, c("Infl", "Type", "Cont"), pretty = TRUE)
})

test_that("ggpredict, polr", {
  ggpredict(fit, "Infl [Low,High]", pretty = TRUE)
  ggpredict(fit, c("Infl [Low,High]", "Type [Tower]"), pretty = TRUE)
  ggpredict(fit, c("Infl [Medium,Low]", "Type [Terrace]", "Cont [Low]"), pretty = TRUE)
})

test_that("ggpredict, polr", {
  ggpredict(fit, "Infl [Low,High]", pretty = TRUE, x.as.factor = TRUE)
  ggpredict(fit, c("Infl [Low,High]", "Type [Tower]"), pretty = TRUE, x.as.factor = TRUE)
  ggpredict(fit, c("Infl [Medium,Low]", "Type [Terrace]", "Cont [Low]"), pretty = TRUE, x.as.factor = TRUE)
})

test_that("ggpredict, polr", {
  ggpredict(fit, "Infl [Low,High]", pretty = TRUE, x.as.factor = TRUE, condition = c(Type = "Tower"))
  ggpredict(fit, c("Infl [Low,High]", "Type [Tower]"), pretty = TRUE, x.as.factor = TRUE, condition = c(Cont = "Low"))
})
