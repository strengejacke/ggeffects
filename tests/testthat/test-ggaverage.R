skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")
skip_if_not_installed("MASS")

test_that("ggaverage works with condition", {
  set.seed(123)
  newdata <- data.frame(
    y = c(602, 38, 616, 256, 21, 723, 245, 176, 89, 1614, 31, 27, 313, 251, 345),
    x = c(31, 35, 21, 30, 37, 26, 45, 21, 74, 27, 37, 37, 31, 37, 25),
    x2 = factor(sample(letters[1:3], size = 15, TRUE)),
    offset_1 = c(72, 50, 31, 30, 16, 25, 75, 16, 78, 40, 68, 25, 71, 52, 17)
  )
  moff <- MASS::glm.nb(y ~ x + x2 + offset(log(offset_1)), data = newdata)

  d <- data_grid(moff, "x2", condition = c(offset_1 = 1))
  out1 <- ggaverage(moff, "x2", condition = c(offset_1 = 1))
  out2 <- marginaleffects::avg_predictions(moff, variables = lapply(d, unique))
  out3 <- as.data.frame(emmeans::emmeans(moff, "x2", at = lapply(d, unique), type = "response"))
  expect_equal(out1$predicted, out2$estimate, tolerance = 1e-4)
  expect_equal(out1$conf.low, c(4.35203, 3.19893, 3.48993), tolerance = 1e-4)
  expect_equal(out1$conf.low, out3$asymp.LCL, tolerance = 1e-4)

  # test predictions
  out <- test_predictions(out1)
  expect_equal(out$Contrast[c(1, 3)], -diff(out1$predicted), tolerance = 1e-4)
  expect_equal(out$p.value, c(0.3571, 0.42296, 0.76791), tolerance = 1e-4)

  d <- data_grid(moff, "x2", condition = c(offset_1 = 2))
  out1 <- ggaverage(moff, "x2", condition = c(offset_1 = 2))
  out2 <- marginaleffects::avg_predictions(moff, variables = lapply(d, unique))
  out3 <- as.data.frame(emmeans::emmeans(moff, "x2", at = lapply(d, unique), type = "response"))
  expect_equal(out1$predicted, out2$estimate, tolerance = 1e-4)
  expect_equal(out1$conf.low, c(8.70405, 6.39786, 6.97987), tolerance = 1e-4)
  expect_equal(out1$conf.low, out3$asymp.LCL, tolerance = 1e-4)

  # test predictions
  out <- test_predictions(out1)
  expect_equal(out$Contrast[c(1, 3)], -diff(out1$predicted), tolerance = 1e-4)
  expect_equal(out$p.value, c(0.3571, 0.42296, 0.76791), tolerance = 1e-4)
})
