skip_if_not_installed("MASS")
skip_if_not_installed("splines")

test_that("ggpredict, as.data.frame", {
  data(efc, package = "ggeffects")
  fit <- lm(barthtot ~ c12hour + bs(neg_c_7) * c161sex + e42dep, data = efc)
  p1 <- ggpredict(fit, terms = c("neg_c_7", "c161sex", "e42dep"))

  data(housing, package = "MASS")
  options(contrasts = c("contr.treatment", "contr.poly"))
  fit <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  p2 <- ggpredict(fit, c("Infl", "Type", "Cont"))

  expect_named(p1, c("x", "predicted", "std.error", "conf.low", "conf.high", "group", "facet"))
  expect_named(
    as.data.frame(p1),
    c("x", "predicted", "std.error", "conf.low", "conf.high", "group", "facet")
  )
  expect_named(
    as.data.frame(p1, terms_to_colnames = TRUE),
    c(
      "neg_c_7", "predicted", "std.error", "conf.low", "conf.high",
      "c161sex", "e42dep"
    )
  )
  expect_named(
    p2,
    c(
      "x", "predicted", "std.error", "conf.low", "conf.high", "response.level",
      "group", "facet"
    )
  )
  expect_named(
    as.data.frame(p2),
    c(
      "x", "predicted", "std.error", "conf.low", "conf.high", "response.level",
      "group", "facet"
    )
  )
  expect_named(
    as.data.frame(p2, terms_to_colnames = TRUE),
    c(
      "Infl", "predicted", "std.error", "conf.low", "conf.high",
      "Sat", "Type", "Cont"
    )
  )
})
