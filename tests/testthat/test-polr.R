if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("MASS")
)) {
  context("ggeffects, polr")

  options(contrasts = c("contr.treatment", "contr.poly"))
  fit <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  test_that("ggpredict, polr", {
    ggpredict(fit, "Infl")
    ggpredict(fit, c("Infl", "Type"))
    ggpredict(fit, c("Infl", "Type", "Cont"))
  })

  test_that("ggemmeans, polr", {
    ggemmeans(fit, "Infl")
    ggemmeans(fit, c("Infl", "Type"))
    ggemmeans(fit, c("Infl", "Type", "Cont"))
  })

  test_that("ggpredict, polr", {
    ggpredict(fit, "Infl [Low,High]")
    ggpredict(fit, c("Infl [Low,High]", "Type [Tower]"))
    ggpredict(fit, c("Infl [Medium,Low]", "Type [Terrace]", "Cont [Low]"))
  })

  test_that("ggemmeans, polr", {
    ggemmeans(fit, "Infl [Low,High]")
    ggemmeans(fit, c("Infl [Low,High]", "Type [Tower]"))
    ggemmeans(fit, c("Infl [Medium,Low]", "Type [Terrace]", "Cont [Low]"))
  })

  test_that("ggpredict, polr", {
    ggpredict(fit, "Infl [Low,High]", x.as.factor = TRUE)
    ggpredict(fit, c("Infl [Low,High]", "Type [Tower]"), x.as.factor = TRUE)
    ggpredict(fit, c("Infl [Medium,Low]", "Type [Terrace]", "Cont [Low]"), x.as.factor = TRUE)
  })

  test_that("ggpredict, polr", {
    ggpredict(fit, "Infl [Low,High]", x.as.factor = TRUE, condition = c(Type = "Tower"))
    ggpredict(fit, c("Infl [Low,High]", "Type [Tower]"), x.as.factor = TRUE, condition = c(Cont = "Low"))
  })

  test_that("ggemmeans, polr", {
    ggemmeans(fit, "Infl [Low,High]", x.as.factor = TRUE, condition = c(Type = "Tower"))
    ggemmeans(fit, c("Infl [Low,High]", "Type [Tower]"), x.as.factor = TRUE, condition = c(Cont = "Low"))
  })

  test_that("ggemmeans, polr", {
    p1 <- ggemmeans(fit, "Infl", condition = c(Type = "Tower", Cont = "Low"))
    p2 <- ggpredict(fit, "Infl")
    expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-5)
  })

  test_that("ggeffect, polr", {
    ggeffect(fit, "Infl")
    ggeffect(fit, c("Infl", "Type"))
    ggeffect(fit, c("Infl", "Type", "Cont"))
  })

  test_that("ggeffect, polr", {
    ggeffect(fit, "Infl [Low,High]")
    ggeffect(fit, c("Infl [Low,High]", "Type [Tower]"))
    ggeffect(fit, c("Infl [Medium,Low]", "Type [Terrace]", "Cont [Low]"))
  })
}
