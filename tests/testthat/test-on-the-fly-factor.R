if (suppressWarnings(require("testthat") && require("ggeffects") && require("insight"))) {

  data(mtcars)
  mtcars$cyl2 <- factor(mtcars$cyl)
  m1 <- lm(mpg ~ hp + factor(cyl) + gear, data = mtcars)
  m2 <- lm(mpg ~ hp + cyl2 + gear, data = mtcars)

  pr1 <- ggpredict(m1, "gear")
  pr2 <- ggpredict(m2, "gear")

  test_that("ggpredict, lm", {
    expect_equal(pr1$conf.high, c(24.5107, 25.8074, 28.1194), tolerance = 1e-3)
    expect_equal(pr2$conf.high, c(24.5107, 25.8074, 28.1194), tolerance = 1e-3)
    expect_equal(pr1$conf.high, pr2$conf.high, tolerance = 1e-3)
  })

  pr1 <- ggpredict(m1, "gear", vcov.fun = "vcovHC")
  pr2 <- ggpredict(m2, "gear", vcov.fun = "vcovHC")

  test_that("ggpredict, lm", {
    expect_equal(pr1$conf.high, c(24.1337, 25.913, 28.5737), tolerance = 1e-3)
    expect_equal(pr2$conf.high, c(24.1337, 25.913, 28.5737), tolerance = 1e-3)
    expect_equal(pr1$conf.high, pr2$conf.high, tolerance = 1e-3)
  })
}
