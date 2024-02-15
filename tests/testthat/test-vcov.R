skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("sandwich")

test_that("ggpredict, vcov can be own function", {
  # example taken from "?clubSandwich::vcovCR"
  m <- 8
  cluster <- factor(rep(LETTERS[1:m], 3 + rpois(m, 5)))
  n <- length(cluster)
  X <- matrix(rnorm(3 * n), n, 3)
  nu <- rnorm(m)[cluster]
  e <- rnorm(n)
  y <- X %*% c(0.4, 0.3, -0.3) + nu + e
  dat <- data.frame(y, X, cluster, row = 1:n)

  # fit linear model
  model_vcov <- lm(y ~ X1 + X2 + X3, data = dat)
  out1 <- ggpredict(model_vcov, "X1", vcov_fun = "vcovHC", vcov_type = "HC0")
  out2 <- ggpredict(model_vcov, "X1", vcov_fun = sandwich::vcovHC, vcov_args = list(type = "HC0"))
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4)
})

test_that("ggpredict, CI based on robust SE", {
  data(iris)
  fit <- lm(Sepal.Length ~ Species, data = iris)
  out <- ggpredict(fit, terms = "Species", vcov_fun = "vcovHC", vcov_type = "HC1")
  expect_equal(out$conf.low, c(4.90749, 5.79174, 6.41028), tolerance = 1e-4)
})

skip_if_not_installed("marginaleffects")

test_that("ggaverage, CI based on robust SE", {
  data(iris)
  fit <- lm(Sepal.Length ~ Species, data = iris)
  out <- predict_response(fit, terms = "Species", marginalize = "empirical")
  expect_equal(out$conf.low, c(4.86213, 5.79213, 6.44413), tolerance = 1e-4)
  out <- predict_response(fit, terms = "Species", vcov_fun = "vcovHC", vcov_type = "HC1", marginalize = "empirical")
  expect_equal(out$conf.low, c(4.90749, 5.79174, 6.41028), tolerance = 1e-4)
  out <- ggaverage(fit, terms = "Species", vcov_fun = "vcovHC", vcov_type = "HC1")
  expect_equal(out$conf.low, c(4.90749, 5.79174, 6.41028), tolerance = 1e-4)
})

test_that("ggaverage, hypothesis test, robust SE", {
  data(iris)
  fit <- lm(Sepal.Length ~ Species, data = iris)
  # no robust vcov
  pr <- predict_response(fit, terms = "Species", marginalize = "empirical")
  out1 <- hypothesis_test(pr)
  out2 <- hypothesis_test(fit, "Species")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-4)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4)
  # robust vcov
  pr <- predict_response(fit, terms = "Species", vcov_fun = "HC1", marginalize = "empirical")
  out1 <- hypothesis_test(pr)
  out2 <- hypothesis_test(fit, "Species", vcov_fun = "HC1")
  out3 <- hypothesis_test(fit, "Species", vcov = "HC1")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-4)
  expect_equal(out1$Contrast, out3$Contrast, tolerance = 1e-4)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4)
  expect_equal(out1$conf.low, out3$conf.low, tolerance = 1e-4)
  # johnson-neymann
  data(efc, package = "ggeffects")
  efc$c172code <- as.factor(efc$c172code)
  fit <- lm(neg_c_7 ~ c12hour * barthtot * c172code, data = efc)
  pr <- predict_response(fit, c("c12hour", "barthtot"), marginalize = "empirical")
  out1 <- johnson_neyman(pr)
  expect_equal(attributes(out1)$intervals$pos_lower, 47, tolerance = 1e-3)
  # robust vcov
  pr <- predict_response(fit, c("c12hour", "barthtot"), vcov_fun = "HC1", marginalize = "empirical")
  out2 <- johnson_neyman(pr)
  out3 <- johnson_neyman(pr, vcov_fun = "HC1")
  expect_equal(attributes(out2)$intervals$pos_lower, 44.6, tolerance = 1e-3)
  expect_equal(attributes(out2)$intervals$pos_lower, attributes(out3)$intervals$pos_lower, tolerance = 1e-3)
})
