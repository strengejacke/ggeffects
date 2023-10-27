skip_on_cran()
skip_if_not_installed("ordinal")
skip_if_not_installed("MASS")
skip_if(getRversion() < "3.6.0")

test_that("ggpredict, ordinal", {
  data(wine, package = "ordinal")
  m1 <- ordinal::clm(rating ~ temp * contact, data = wine)

  # ggpredict
  p <- ggpredict(m1, "temp")
  expect_equal(p$predicted[1], 0.1960351, tolerance = 1e-3)
  expect_equal(p$conf.low[1], 0.0772626, tolerance = 1e-3)
  expect_equal(p$conf.high[1], 0.41522921, tolerance = 1e-3)
  ggpredict(m1, c("temp", "contact"))

  # ggeffect
  p <- ggeffect(m1, "temp")
  expect_equal(p$predicted[1], 0.110564082334497, tolerance = 1e-3)
  ggeffect(m1, c("temp", "contact"))

  # ggemmeans
  p <- ggemmeans(m1, "contact")
  expect_equal(p$predicted[1], 0.1097049, tolerance = 1e-3)
  ggemmeans(m1, c("temp", "contact"))
})
