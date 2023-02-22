if (suppressWarnings(requiet("testthat") && requiet("ggeffects") && requiet("insight"))) {

  data(mtcars)
  mtcars$cyl2 <- factor(mtcars$cyl)
  m1 <- lm(mpg ~ hp + factor(cyl) + gear, data = mtcars)
  m2 <- lm(mpg ~ hp + cyl2 + gear, data = mtcars)

  pr1 <- ggpredict(m1, "gear")
  pr2 <- ggpredict(m2, "gear")

  test_that("ggpredict, lm", {
    expect_equal(pr1$conf.high, c(24.7192, 25.9456, 28.2348), tolerance = 1e-3)
    expect_equal(pr2$conf.high, c(24.7192, 25.9456, 28.2348), tolerance = 1e-3)
    expect_equal(pr1$conf.high, pr2$conf.high, tolerance = 1e-3)
  })

  if (suppressWarnings(requiet("sandwich"))) {
    pr1 <- ggpredict(m1, "gear", vcov.fun = "vcovHC")
    pr2 <- ggpredict(m2, "gear", vcov.fun = "vcovHC")

    test_that("ggpredict, lm", {
      expect_equal(pr1$conf.high, c(24.3244, 26.0561, 28.7104), tolerance = 1e-3)
      expect_equal(pr2$conf.high, c(24.3244, 26.0561, 28.7104), tolerance = 1e-3)
      expect_equal(pr1$conf.high, pr2$conf.high, tolerance = 1e-3)
    })
  }
}
