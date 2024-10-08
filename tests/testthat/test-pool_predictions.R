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
  expect_equal(pool_pre$predicted, c(29.6686, 24.9858, 23.1923), tolerance = 1e-3)
  expect_equal(pool_pre$conf.low, c(10.812922, 11.334209, 7.76223), tolerance = 1e-3)
})
