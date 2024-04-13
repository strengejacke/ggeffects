skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("lme4")
skip_if_not_installed("datawizard")

test_that("test_predictions, mixed models", {
  data(efc, package = "ggeffects")
  efc <- datawizard::to_factor(efc, c("e42dep", "c172code", "e16sex"))

  fit <- suppressWarnings(lme4::glmer(
    tot_sc_e ~ e42dep + c172code + e17age + e16sex + (1 | e15relat),
    data = efc,
    family = poisson()
  ))
  out <- test_predictions(fit, terms = "e16sex")
  expect_equal(out$Contrast, 0.04251535, tolerance = 1e-3)

  out <- test_predictions(fit, terms = "e16sex", margin = "marginalmeans")
  expect_equal(out$Contrast, 0.07125496, tolerance = 1e-3)

  out <- test_predictions(fit, terms = "e16sex", margin = "empirical")
  expect_equal(out$Contrast, 0.07457153, tolerance = 1e-3)
})

test_that("test_predictions, mixed models, print with conditioned values", {
  data(efc, package = "ggeffects")
  efc <- datawizard::to_factor(efc, c("e42dep", "c172code", "e16sex"))
  levels(efc$c172code) <- c("low", "medium", "high")

  fit <- suppressWarnings(lme4::glmer(
    tot_sc_e ~ e16sex * c172code + e17age + (1 | e15relat),
    data = efc,
    family = poisson()
  ))
  expect_snapshot(print(test_predictions(fit, terms = c("e16sex", "c172code"))))
  expect_snapshot(print(test_predictions(fit, terms = c("e16sex", "c172code [medium]"))))
})