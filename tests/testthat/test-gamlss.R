skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("gamlss")

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
      3.66712, 3.85232, 4.03678, 4.22004, 4.40113, 4.5782, 4.74796,
      4.90714, 5.05622, 5.19881, 5.33789, 5.47507, 5.61117
    ),
    tolerance = 1e-2
  )
})
