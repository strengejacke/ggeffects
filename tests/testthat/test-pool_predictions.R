if (suppressWarnings(requiet("testthat") && requiet("ggeffects") && requiet("mice"))) {
  test_that("ggeffects, pool predictions with transformed response", {
    data("nhanes2")
    set.seed(123)
    imp <- mice(nhanes2, printFlag = FALSE)
    predictions <- lapply(1:5, function(i) {
      m <- lm(log(bmi) ~ age + hyp + chl, data = complete(imp, action = i))
      ggemmeans(m, "age", back.transform = TRUE, verbose = FALSE)
    })
    expect_message({
      pool_pre <- pool_predictions(predictions)
    })
    expect_equal(pool_pre$predicted, c(29.6686, 24.9858, 23.1923), tolerance = 1e-3)
    expect_equal(pool_pre$conf.low, c(25.391, 22.1176, 19.5893), tolerance = 1e-3)
  })
}
