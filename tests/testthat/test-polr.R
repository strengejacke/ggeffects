if (suppressWarnings(
  requiet("testthat") &&
  requiet("ggeffects") &&
  requiet("emmeans") &&
  requiet("effects") &&
  requiet("MASS")
)) {
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
    ggpredict(fit, "Infl [Low,High]", condition = c(Type = "Tower"))
    ggpredict(fit, c("Infl [Low,High]", "Type [Tower]"), condition = c(Cont = "Low"))
  })

  test_that("ggemmeans, polr", {
    ggemmeans(fit, "Infl [Low,High]", condition = c(Type = "Tower"))
    ggemmeans(fit, c("Infl [Low,High]", "Type [Tower]"), condition = c(Cont = "Low"))
  })

  test_that("ggemmeans, polr", {
    p1 <- ggemmeans(fit, "Infl", condition = c(Type = "Tower", Cont = "Low"))
    p2 <- ggpredict(fit, "Infl")
    expect_equal(
      p1$predicted[p1$x == 1 & p1$response.level == "Low"],
      p2$predicted[p2$x == 1 & p2$response.level == "Low"],
      tolerance = 1e-3
    )
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
