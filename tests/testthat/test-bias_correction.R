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

skip_if_not_installed("lme4")

set.seed(1234)

test_that("ggpredict, bias_correction, mixed", {
  set.seed(123)
  dat <- data.frame(
    outcome = rbinom(n = 100, size = 1, prob = 0.35),
    var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
    var_cont = rnorm(n = 100, mean = 10, sd = 7),
    group = sample(letters[1:4], size = 100, replace = TRUE)
  )

  dat$var_cont <- datawizard::standardize(dat$var_cont)
  m1 <- lme4::glmer(
    outcome ~ var_binom + var_cont + (1 | group),
    data = dat,
    family = binomial(link = "logit")
  )
  out1 <- predict_response(m1, "var_binom")
  out2 <- predict_response(m1, "var_binom", bias_correction = TRUE)
  out3 <- as.data.frame(emmeans::emmeans(m1, "var_binom", type = "response"))
  out4 <- as.data.frame(emmeans::emmeans(m1, "var_binom", type = "response", bias.adjust = TRUE, sigma = 1))

  expect_equal(out1$predicted, out3$prob, tolerance = 1e-3)
  expect_equal(out2$predicted, out4$prob, tolerance = 1e-3)
})
