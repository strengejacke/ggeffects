skip_on_os(c("mac", "solaris"))
skip_if_not_installed("betareg")
skip_if_not_installed("emmeans")
skip_if_not_installed("effects")

data("GasolineYield", package = "betareg")
m1 <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)

test_that("ggpredict", {
  p <- ggpredict(m1, "batch")
  expect_equal(p$predicted, unname(predict(m1, newdata = new_data(m1, "batch"))), tolerance = 1e-3)
  expect_s3_class(ggpredict(m1, c("batch", "temp")), "data.frame")
})

test_that("ggeffect", {
  p <- ggeffect(m1, "batch")
  expect_equal(p$predicted[1], 0.3122091, tolerance = 1e-3)
  expect_s3_class(ggeffect(m1, c("batch", "temp")), "data.frame")
})

test_that("ggemmeans", {
  p <- ggemmeans(m1, "batch")
  expect_equal(p$predicted[1], 0.3122091, tolerance = 1e-3)
  expect_s3_class(ggemmeans(m1, c("batch", "temp")), "data.frame")
})

test_that("ggpredict", {
  # create df
  df2 <- data.frame(
    ratio = c(0.5, 0.5, 0.6, 0.6, 0.7, 0.8, 0.9, 0.9),
    GD = c(0.5, 0.4, 0.6, 0.7, 0.8, 1.0, 1.0, 1.0),
    Source_Salinity = c(
      "Brackish",
      "Fresh",
      "Brackish",
      "Fresh",
      "Brackish",
      "Fresh",
      "Fresh",
      "Brackish"
    ),
    stringsAsFactors = FALSE
  )
  # run beta model
  m <- betareg::betareg(ratio ~ GD + Source_Salinity, data = df2)
  p <- ggemmeans(m, "GD")
  expect_equal(p$conf.low, c(0.35636, 0.45329, 0.54815, 0.63206, 0.701, 0.80427), tolerance = 1e-2)
  p <- ggemmeans(m, "Source_Salinity", verbose = FALSE)

  skip_if_not_installed("marginaleffects")
  out <- test_predictions(p)
  expect_equal(out$Contrast, -0.00123838580671709, tolerance = 1e-4)
})
