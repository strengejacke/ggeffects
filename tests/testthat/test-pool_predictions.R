skip_on_os(c("mac", "solaris"))
skip_if_not_installed("mice")
skip_if_not_installed("emmeans")

test_that("ggeffects, pool predictions with transformed response", {
  data("nhanes2", package = "mice")
  set.seed(123)
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  predictions <- lapply(1:5, function(i) {
    m <- lm(log(bmi) ~ age + hyp + chl, data = mice::complete(imp, action = i))
    ggemmeans(m, "age", back_transform = TRUE, verbose = FALSE)
  })
  expect_message({
    pool_pre <- pool_predictions(predictions)
  })
  expect_equal(
    pool_pre$predicted,
    c(29.6686, 24.9858, 23.1923),
    tolerance = 1e-3
  )
  expect_equal(
    pool_pre$conf.low,
    c(10.812922, 11.334209, 7.76223),
    tolerance = 1e-3
  )
})

test_that("pool_predictions correctly computes Barnard-Rubin degrees of freedom (#679)", {
  skip_if_not_installed("mice")

  data("nhanes2", package = "mice")
  set.seed(123)
  imp <- mice::mice(nhanes2, m = 5, printFlag = FALSE)

  predictions <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    predict_response(m, "age")
  })

  pooled <- pool_predictions(predictions)

  # Manual calculation per Barnard & Rubin (1999) for row 1
  m_imp <- length(predictions)
  dof_com <- attributes(predictions[[1]])$df
  preds_row1 <- vapply(predictions, function(x) x$predicted[1], numeric(1))
  ses_row1 <- vapply(predictions, function(x) x$std.error[1], numeric(1))

  ubar_1 <- mean(ses_row1^2)
  b_1 <- stats::var(preds_row1)
  t_1 <- ubar_1 + (1 + 1 / m_imp) * b_1

  lambda_1 <- (1 + 1 / m_imp) * b_1 / t_1
  df_old_1 <- (m_imp - 1) / lambda_1^2
  df_obs_1 <- ((dof_com + 1) / (dof_com + 3)) * dof_com * (1 - lambda_1)
  expected_df_1 <- (df_old_1 * df_obs_1) / (df_old_1 + df_obs_1)

  expected_ci_low_1 <- mean(preds_row1) -
    stats::qt(0.975, df = expected_df_1) * sqrt(t_1)
  expected_ci_high_1 <- mean(preds_row1) +
    stats::qt(0.975, df = expected_df_1) * sqrt(t_1)

  expect_equal(pooled$predicted[1], mean(preds_row1), tolerance = 1e-5)
  expect_equal(pooled$std.error[1], sqrt(t_1), tolerance = 1e-5)
  expect_equal(pooled$conf.low[1], expected_ci_low_1, tolerance = 1e-5)
  expect_equal(pooled$conf.high[1], expected_ci_high_1, tolerance = 1e-5)
})

test_that("pool_predictions CIs do not inflate with large between-level variance (#679)", {
  # Simulate two nearly identical imputations for a binary predictor with a huge effect.
  # Under the old bug, nrow = 2 (m = 2) and huge var(predicted) across levels
  # caused artificially massive CIs.
  set.seed(42)
  d1 <- data.frame(
    x = factor(rep(c("A", "B"), each = 50)),
    y = c(rnorm(50, mean = 0, sd = 1), rnorm(50, mean = 100, sd = 1))
  )
  d2 <- d1
  d2$y <- d2$y + rnorm(100, mean = 0, sd = 0.001)

  p1 <- predict_response(lm(y ~ x, data = d1), "x")
  p2 <- predict_response(lm(y ~ x, data = d2), "x")

  pooled <- pool_predictions(list(p1, p2))

  # Since between-imputation variance is near zero, pooled CIs should closely
  # match the single-model CIs
  expect_equal(pooled$conf.low, p1$conf.low, tolerance = 1e-2)
  expect_equal(pooled$conf.high, p1$conf.high, tolerance = 1e-2)
})

test_that("pool_comparisons correctly computes and applies pooled_df (#679)", {
  skip_if_not_installed("mice")

  data("nhanes2", package = "mice")
  set.seed(123)
  imp <- mice::mice(nhanes2, m = 5, printFlag = FALSE)

  comparisons <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    test_predictions(m, "age")
  })

  pooled_comp <- pool_comparisons(comparisons)

  # Manual check for first contrast row
  m_imp <- length(comparisons)
  dof_com <- attributes(comparisons[[1]])$df
  comp_row1 <- vapply(comparisons, function(x) x$Contrast[1], numeric(1))
  se_row1 <- vapply(
    comparisons,
    function(x) attributes(x)$standard_error[1],
    numeric(1)
  )

  ubar_1 <- mean(se_row1^2)
  b_1 <- stats::var(comp_row1)
  t_1 <- ubar_1 + (1 + 1 / m_imp) * b_1

  lambda_1 <- (1 + 1 / m_imp) * b_1 / t_1
  df_old_1 <- (m_imp - 1) / lambda_1^2
  df_obs_1 <- ((dof_com + 1) / (dof_com + 3)) * dof_com * (1 - lambda_1)
  expected_df_1 <- (df_old_1 * df_obs_1) / (df_old_1 + df_obs_1)

  expected_ci_low_1 <- mean(comp_row1) -
    stats::qt(0.975, df = expected_df_1) * sqrt(t_1)
  expected_ci_high_1 <- mean(comp_row1) +
    stats::qt(0.975, df = expected_df_1) * sqrt(t_1)

  expect_equal(pooled_comp$Contrast[1], mean(comp_row1), tolerance = 1e-5)
  expect_equal(pooled_comp$conf.low[1], expected_ci_low_1, tolerance = 1e-5)
  expect_equal(pooled_comp$conf.high[1], expected_ci_high_1, tolerance = 1e-5)
})
