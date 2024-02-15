skip_on_cran()
skip_on_os(c("mac", "solaris"))

skip_if_not_installed("fixest")

test_that("fixest", {
  # avoid warnings
  fixest::setFixest_nthreads(1)

  data(trade, package = "fixest")

  m1 <- fixest::femlm(Euros ~ log(dist_km) | Origin + Destination + Product, data = trade)
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

  pr <- ggpredict(m2, "Petal.Length", verbose = FALSE)
  expect_equal(
    pr$predicted,
    predict(m2, newdata = new_data(m2, "Petal.Length")),
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
  expect_equal(out$predicted, c(-0.10301, -0.05684, -0.01067, 0.03551, 0.08168, 0.12785, 0.17402), tolerance = 1e-4)
  expect_equal(out$conf.low, c(-0.98691, -0.64611, -0.3053, 0.03551, -0.21296, -0.46142, -0.70988), tolerance = 1e-4)
})
