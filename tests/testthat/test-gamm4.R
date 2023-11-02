skip_on_cran()
skip_on_os(c("mac", "solaris"))
unloadNamespace("gam")
skip_if_not_installed("gamm4")
skip_if_not_installed("mgcv")

test_that("ggpredict", {
  set.seed(123)
  dat <- mgcv::gamSim(1, n = 400, scale = 2) ## simulate 4 term additive truth
  dat$fac <- fac <- as.factor(sample(1:20, 400, replace = TRUE))
  dat$y <- dat$y + model.matrix(~ fac - 1) %*% rnorm(20) * 0.5

  set.seed(123)
  m1 <- gamm4::gamm4(y ~ s(x0) + x1 + s(x2), data = dat, random = ~ (1 | fac))
  p <- ggpredict(m1, "x1")
  expect_equal(p$predicted[1], 5.885441, tolerance = 1e-4)
  expect_s3_class(ggpredict(m1, c("x1", "x2")), "data.frame")
})
