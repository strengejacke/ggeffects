context("ggeffects, polr")

library(ggeffects)
library(sjmisc)
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
  ggpredict(fit, "Infl [Medium]", pretty = TRUE)
  ggpredict(fit, c("Infl [Low,High]", "Type [Tower]"), pretty = TRUE)
  ggpredict(fit, c("Infl [Medium,Low]", "Type [Terrace]", "Cont [Low]"), pretty = TRUE)
})
