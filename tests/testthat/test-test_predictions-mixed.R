skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("lme4")
skip_if_not_installed("datawizard")

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
})
