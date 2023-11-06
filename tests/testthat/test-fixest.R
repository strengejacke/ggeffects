skip_on_cran()
skip_on_os(c("mac", "solaris"))

skip_if_not_installed("fixest")
skip_if_not_installed("carData")

test_that("fixest", {
  # avoid warnings
  fixest::setFixest_nthreads(1)

  data(trade, package = "fixest")
  data(Greene, package = "carData")

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
