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

  out4 <- predict_response(model, c("treatment", "time"), bias_correction = TRUE, interval = "prediction")
  out5 <- predict_response(model, c("treatment", "time"), interval = "prediction")
  expect_equal(out1$predicted, out4$predicted, tolerance = 1e-3)
  expect_equal(out4$conf.low, c(0.14164, 0.07946, 0.30262, 0.05329, 0.21638, 0.14164), tolerance = 1e-3)
  expect_equal(out5$conf.low, c(0.10461, 0.05599, 0.25616, 0.03685, 0.16983, 0.10461), tolerance = 1e-3)
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

skip_if_not_installed("glmmTMB")

test_that("ggpredict, bias_correction, glmmTMB", {
  data(Salamanders, package = "glmmTMB")

  m3 <- glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    ziformula = ~ spp + mined,
    family = glmmTMB::truncated_poisson(),
    data = Salamanders
  )

  out1 <- as.data.frame(emmeans::emmeans(m3, "mined", type = "response"))
  out2 <- as.data.frame(emmeans::emmeans(m3, "mined", type = "response", bias.adjust = TRUE, sigma = insight::get_sigma(model)))
  out3 <- predict_response(m3, "mined", margin = "marginalmeans")
  out4 <- predict_response(m3, "mined", margin = "marginalmeans", bias_correction = TRUE, sigma = insight::get_sigma(model))
  expect_equal(out1$rate, out3$predicted, tolerance = 1e-3)
  expect_equal(out2$rate, out4$predicted, tolerance = 1e-3)

  out5 <- predict_response(m3, "mined")
  expect_equal(out5$predicted, c(0.93517, 2.57911), tolerance = 1e-3)
  out6 <- predict_response(m3, "mined", bias_correction = TRUE, sigma = insight::get_sigma(model))
  expect_equal(out6$predicted, c(1.40276, 3.86866), tolerance = 1e-3)
})
