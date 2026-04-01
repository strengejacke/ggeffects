skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("brms")
skip_if_not_installed("nlme")

test_that("predict_response, brms with ar() autocorrelation term", {
  fit <- suppressWarnings(brms::brm(
    formula = protein ~ 1 + Diet + (1 | Cow) + ar(time = Time, gr = Cow, p = 1),
    data = nlme::Milk,
    chains = 2,
    iter = 500,
    refresh = 0
  ))

  # should not throw "Time points within groups must be unique"
  expect_no_error({
    p <- suppressWarnings(predict_response(fit, terms = "Diet", verbose = FALSE))
  })

  expect_s3_class(p, "data.frame")
  expect_equal(nrow(p), 3L)
  expect_true(all(is.finite(p$predicted)))
  expect_true(all(p$conf.low < p$predicted))
  expect_true(all(p$predicted < p$conf.high))
})
