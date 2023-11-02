skip_on_os(c("mac", "solaris"))

data(mtcars)
mtcars$cyl2 <- factor(mtcars$cyl)
m1 <- lm(mpg ~ hp + factor(cyl) + gear, data = mtcars)
m2 <- lm(mpg ~ hp + cyl2 + gear, data = mtcars)

test_that("ggpredict, lm", {
  pr1 <- ggpredict(m1, "gear")
  pr2 <- ggpredict(m2, "gear")
  expect_equal(pr1$conf.high, c(24.7192, 25.9456, 28.2348), tolerance = 1e-3)
  expect_equal(pr2$conf.high, c(24.7192, 25.9456, 28.2348), tolerance = 1e-3)
  expect_equal(pr1$conf.high, pr2$conf.high, tolerance = 1e-3)
})

test_that("ggpredict, lm", {
  skip_if_not_installed("sandwich")
  pr1 <- ggpredict(m1, "gear", vcov_fun = "vcovHC")
  pr2 <- ggpredict(m2, "gear", vcov_fun = "vcovHC")
  expect_equal(pr1$conf.high, c(24.3244, 26.0561, 28.7104), tolerance = 1e-3)
  expect_equal(pr2$conf.high, c(24.3244, 26.0561, 28.7104), tolerance = 1e-3)
  expect_equal(pr1$conf.high, pr2$conf.high, tolerance = 1e-3)
})

test_that("ggpredict, on-the-fly factor", {
  data(mtcars)
  m <- lm(mpg ~ gear + as.factor(cyl) + wt, data = mtcars)
  dat_categorical <- ggpredict(m, terms = c("cyl", "wt"))
  expect_identical(attributes(dat_categorical)$x.is.factor, "1")
})

test_that("ggpredict, on-the-fly factor-2", {
  data(mtcars)
  mtcars$cyl <- as.factor(mtcars$cyl)
  m <- lm(mpg ~ gear + cyl + wt, data = mtcars)
  dat_categorical <- ggpredict(m, terms = c("cyl", "wt"))
  expect_identical(attributes(dat_categorical)$x.is.factor, "1")
})

test_that("ggpredict, on-the-fly factor-3", {
  data(mtcars)
  m <- lm(mpg ~ gear + cyl + wt, data = mtcars)
  dat_categorical <- ggpredict(m, terms = c("cyl", "wt"))
  expect_identical(attributes(dat_categorical)$x.is.factor, "0")
})
