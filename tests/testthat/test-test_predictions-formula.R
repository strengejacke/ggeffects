skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")
skip_if_not_installed("datawizard")

test_that("test_predictions, formula", {
  data(coffee_data, package = "ggeffects")
  # Median split
  coffee_data$alertness_d <- datawizard::categorize(coffee_data$alertness, lowest = 0)
  coffee_data$treatment <- coffee_data$coffee
  m <- glm(alertness_d ~ time * treatment, data = coffee_data, family = binomial())
  pr <- predict_response(m, terms = c("time", "treatment"))
  out1 <- test_predictions(pr, test = "consecutive", by = "treatment")
  out2 <- test_predictions(pr, test = difference ~ sequential | treatment)
  expect_equal(out1$Contrast[1], out2$Difference[2], tolerance = 1e-4)
  expect_equal(out1$conf.low[1], out2$CI_low[2], tolerance = 1e-4)
})
