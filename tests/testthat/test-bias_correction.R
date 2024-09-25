skip_on_os(c("mac", "solaris"))
skip_if_not_installed("emmeans")
skip_if_not_installed("datawizard")

test_that("ggpredict, bias_correction", {
  data(coffee_data, package = "ggeffects")
  # dichotomize outcome variable
  coffee_data$alertness <- datawizard::categorize(coffee_data$alertness, lowest = 0)
  # rename variable
  coffee_data$treatment <- coffee_data$coffee

  # model
  model <- glm(alertness ~ treatment * time, data = coffee_data, family = binomial())

  out1 <- predict_response(model, c("treatment", "time"), bias_correction = TRUE)
  out2 <- predict_response(model, c("treatment", "time"), margin = "marginalmeans", bias_correction = TRUE, sigma = insight::get_sigma(model))
  out3 <- as.data.frame(emmeans::emmeans(model, c("treatment", "time"), bias.adjust = TRUE, type = "response", sigma = insight::get_sigma(model)))
  out3 <- out3[order(out3$treatment), ]

  expect_equal(out1$predicted, out2$predicted, tolerance = 1e-3)
  expect_equal(out1$predicted, out3$prob, tolerance = 1e-3)
  expect_message(
    predict_response(model, c("treatment", "time"), margin = "marginalmeans", bias_correction = TRUE),
    regex = "bias_correction = TRUE"
  )
})
