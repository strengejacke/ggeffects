skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("gamlss")
skip_if_not_installed("insight", minimum_version = "0.19.7")

test_that("ggpredict", {
  data(iris)
  m1 <- gamlss::gamlss(
    Sepal.Length ~ Sepal.Width + gamlss::random(Species),
    sigma.formula = ~Sepal.Width,
    data = iris
  )

  p <- ggpredict(m1, "Sepal.Width")
  expect_equal(
    p$predicted,
    c(
      6.0235, 5.98971, 5.95592, 5.92213, 5.88834, 5.85455, 5.82076,
      5.78697, 5.75318, 5.71939, 5.6856, 5.65181, 5.61802
    ),
    tolerance = 1e-2
  )
  # validate against predict()
  expect_equal(
    p$predicted,
    predict(m1, newdata = data_grid(m1, "Sepal.Width")),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_named(p, c("x", "predicted", "std.error", "conf.low", "conf.high", "group"))
  expect_equal(
    p$conf.low,
    c(
      5.66308, 5.69061, 5.71514, 5.73386, 5.74045, 5.7232, 5.67382,
      5.60019, 5.51415, 5.42217, 5.32713, 5.23036, 5.13254
    ),
    tolerance = 1e-2
  )
})
