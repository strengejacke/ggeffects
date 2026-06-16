skip_on_cran()
skip_on_os(c("mac", "solaris"))

skip_if_not_installed("fixest")
skip_if_not_installed("marginaleffects")

test_that("fixest", {
  # avoid warnings
  fixest::setFixest_nthreads(1)

  data(trade, package = "fixest")

  m1 <- fixest::femlm(
    Euros ~ log(dist_km) | Origin + Destination + Product,
    data = trade
  )
  m2 <- fixest::feols(
    Sepal.Width ~ Petal.Length | Species | Sepal.Length ~ Petal.Width,
    data = iris
  )

  pr <- ggpredict(m1, "dist_km", verbose = FALSE)
  expect_equal(
    pr$predicted,
    predict(m1, newdata = new_data(m1, "dist_km"), type = "response"),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_equal(
    pr$conf.low,
    c(
      NaN,
      24438196.27518,
      8475597.63776,
      4561910.58846,
      2939486.79798,
      2090341.12038,
      1582151.07907,
      1250172.49632,
      1019465.88368,
      851577.91446,
      724967.18099,
      626730.22357,
      548717.90853,
      485558.20672,
      433581.87885,
      390206.75598,
      353568.75516,
      322292.22021,
      295342.24535
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  pr <- ggpredict(m2, "Petal.Length", verbose = FALSE)
  expect_equal(
    pr$predicted,
    predict(m2, newdata = new_data(m2, "Petal.Length")),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_equal(
    pr$conf.low,
    c(
      -4990.47806,
      -7374.91679,
      -9759.35552,
      -12143.79426,
      -14528.23299,
      -16912.67172,
      -19297.11045,
      -21681.54918,
      -24065.98792,
      -26450.42665,
      -28834.86538,
      -31219.30411,
      -33603.74284
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  pr <- ggpredict(m2, "Petal.Length", verbose = FALSE, vcov = "iid")
  expect_equal(
    pr$conf.low,
    c(
      -4990.47806,
      -7374.91679,
      -9759.35552,
      -12143.79426,
      -14528.23299,
      -16912.67172,
      -19297.11045,
      -21681.54918,
      -24065.98792,
      -26450.42665,
      -28834.86538,
      -31219.30411,
      -33603.74284
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})

test_that("fixest, numeric cluster variable works", {
  set.seed(101) # For reproducibility
  dat <- data.frame(
    y = rnorm(200),
    year = sample(c(2010, 2011, 2012), 200, replace = TRUE),
    var = rnorm(200)
  )
  model <- fixest::feols(y ~ var | year, data = dat)
  out <- ggpredict(model, terms = "var")
  expect_equal(
    out$predicted,
    c(-0.10301, -0.05684, -0.01067, 0.03551, 0.08168, 0.12785, 0.17402),
    tolerance = 1e-4
  )
  expect_equal(
    out$conf.low,
    c(-0.48992, -0.31478, -0.13964, 0.03551, -0.04729, -0.13009, -0.21289),
    tolerance = 1e-4
  )
})
