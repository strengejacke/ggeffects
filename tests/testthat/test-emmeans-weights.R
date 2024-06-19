skip_if_not_installed("emmeans")

test_that("ggemmeans, weights", {
  data(mtcars)
  fit <- lm(mpg ~ hp + as.factor(cyl) + as.factor(gear) + as.factor(am), data = mtcars)
  out <- ggemmeans(fit, "cyl")
  expect_equal(out$predicted, c(22.6461, 19.0232, 20.2174), tolerance = 1e-3)
  out <- ggemmeans(fit, "cyl", weights = "equal")
  expect_equal(out$predicted, c(22.6461, 19.0232, 20.2174), tolerance = 1e-3)
  out <- ggemmeans(fit, "cyl", weights = "proportional")
  expect_equal(out$predicted, c(21.9457, 18.3228, 19.517), tolerance = 1e-3)
  out <- ggemmeans(fit, "cyl", weights = "flat")
  expect_equal(out$predicted, c(22.4982, 18.8754, 20.5131), tolerance = 1e-3)
})
