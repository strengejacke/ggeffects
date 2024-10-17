skip_on_os(c("mac", "solaris"))
skip_if_not_installed("pscl")
skip_if_not_installed("glmmTMB")

data(Salamanders, package = "glmmTMB")

m1 <- pscl::zeroinfl(count ~ mined | mined, dist = "poisson", data = Salamanders)
m2 <- pscl::hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "poisson", data = Salamanders)
m3 <- pscl::hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "binomial", data = Salamanders)
m4 <- pscl::hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "binomial", link = "log", data = Salamanders)
m5 <- suppressWarnings(pscl::zeroinfl(count ~ mined | mined, dist = "negbin", link = "log", data = Salamanders))

test_that("ggpredict, pscl", {
  expect_s3_class(ggpredict(m1, "mined", type = "fixed"), "data.frame")
  expect_s3_class(ggpredict(m1, "mined", type = "zero_inflated"), "data.frame")
  expect_s3_class(ggpredict(m2, "mined", type = "fixed"), "data.frame")
  expect_s3_class(ggpredict(m2, "mined", type = "zero_inflated"), "data.frame")
  expect_s3_class(ggpredict(m3, "mined", type = "fixed"), "data.frame")
  expect_s3_class(ggpredict(m3, "mined", type = "zero_inflated"), "data.frame")
  expect_s3_class(ggpredict(m4, "mined", type = "fixed"), "data.frame")
  expect_s3_class(ggpredict(m4, "mined", type = "zero_inflated"), "data.frame")
  expect_s3_class(ggpredict(m5, "mined", type = "fixed"), "data.frame")
  expect_s3_class(ggpredict(m5, "mined", type = "zero_inflated"), "data.frame")
})

test_that("ggpredict, pscl", {
  skip_on_cran()
  set.seed(123)
  pr <- ggpredict(m1, "mined", type = "zero_inflated")
  expect_equal(pr$conf.low, c(0.18731, 2.00199), tolerance = 1e-3)

  model <- pscl::zeroinfl(count ~ mined * spp | mined * spp, dist = "poisson", data = Salamanders)
  set.seed(123)
  pr <- ggpredict(model, c("mined", "spp"), type = "zero_inflated")
  expect_equal(
    pr$conf.low,
    c(1e-05, 1e-05, 0.0452, 1e-05, 1e-05, 0.19485, 0.06506, 1.64314,
      0.1003, 1.8247, 0.46168, 3.15703, 3.13494, 1.36434),
    tolerance = 1e-2
  )
})

test_that("ggemmeans, pscl", {
  skip_if_not_installed("emmeans")
  expect_s3_class(ggemmeans(m1, "mined", type = "fixed"), "data.frame")
  expect_s3_class(ggemmeans(m1, "mined", type = "zero_inflated"), "data.frame")
  expect_s3_class(ggemmeans(m2, "mined", type = "fixed"), "data.frame")
  expect_s3_class(ggemmeans(m2, "mined", type = "zero_inflated"), "data.frame")
  expect_s3_class(ggemmeans(m3, "mined", type = "fixed"), "data.frame")
  expect_s3_class(ggemmeans(m3, "mined", type = "zero_inflated"), "data.frame")
  expect_s3_class(ggemmeans(m4, "mined", type = "fixed"), "data.frame")
  expect_s3_class(ggemmeans(m4, "mined", type = "zero_inflated"), "data.frame")
  expect_s3_class(ggemmeans(m5, "mined", type = "fixed"), "data.frame")
  expect_s3_class(ggemmeans(m5, "mined", type = "zero_inflated"), "data.frame")
})

test_that("compare, pscl", {
  skip_if_not_installed("emmeans")
  p1 <- ggemmeans(m1, "mined", type = "fixed")
  p2 <- ggpredict(m1, "mined", type = "fixed")
  expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)

  p1 <- ggemmeans(m1, "mined", type = "zero_inflated")
  p2 <- ggpredict(m1, "mined", type = "zero_inflated")
  expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)

  p1 <- ggemmeans(m2, "mined", type = "fixed")
  p2 <- ggpredict(m2, "mined", type = "fixed")
  expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)

  p1 <- ggemmeans(m2, "mined", type = "zero_inflated")
  p2 <- ggpredict(m2, "mined", type = "zero_inflated")
  expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)

  p1 <- ggemmeans(m5, "mined", type = "fixed")
  p2 <- ggpredict(m5, "mined", type = "fixed")
  expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)

  p1 <- ggemmeans(m5, "mined", type = "zero_inflated")
  p2 <- ggpredict(m5, "mined", type = "zero_inflated")
  expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)
})

test_that("pscl, offset, interaction and CI", {
  # Generate some data
  set.seed(123)
  N <- 100 # Samples
  x <- runif(N, 0, 5) # Predictor 1
  z <- runif(N, 0, 5) # Predictor 2
  off <- rgamma(N, 3, 2) # Offset variable
  yhat <- -1 + x * 0.2 + z * -0.2 + z * x * 0.2 + log(off) # Prediction on log scale
  dat <- data.frame(y = NA, x, z, logOff = log(off)) # Storage dataframe

  dat$y <- rpois(N, exp(yhat)) # Poisson process
  dat$y <- ifelse(rbinom(N, 1, 0.3), 0, dat$y) # Zero-inflation process

  # Fit zeroinfl and glm model
  # Interaction b/w x and z
  model <- pscl::zeroinfl(y ~ offset(logOff) + x * z | 1, data = dat, dist = "poisson")

  pr <- ggpredict(model, c("x", "z"))
  expect_equal(
    pr$conf.low,
    c(0.10175, 0.10842, 0.07738, 0.15311, 0.17543, 0.14137, 0.2299,
      0.28352, 0.25811, 0.34404, 0.45742, 0.47084, 0.51189, 0.73575,
      0.85762, 0.75364, 1.17695, 1.55786, 1.08708, 1.86238, 2.81383,
      1.51007, 2.8848, 5.01418, 1.9918, 4.32397, 8.65455, 2.51388,
      6.28353, 14.25571, 3.09229, 8.96366, 22.76232),
    tolerance = 1e-3
  )
})


test_that("pscl, validate all functions against predict", {
  skip_if_not_installed("marginaleffects")
  data(Salamanders, package = "glmmTMB")
  m <- pscl::hurdle(count ~ spp | spp, data = Salamanders)
  nd <- new_data(m, "spp")

  out1 <- predict(m, newdata = nd, type = "count")
  out2 <- ggpredict(m, "spp", type = "fixed")
  out3 <- ggaverage(m, "spp", type = "count")
  out4 <- marginaleffects::avg_predictions(m, variables = "spp", type = "count")

  expect_equal(out1, out2$predicted, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1, out3$predicted, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1, out4$estimate, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- predict(m, newdata = nd, type = "response")
  out2 <- ggpredict(m, "spp", type = "zero_inflated")
  out3 <- ggaverage(m, "spp", type = "zero_inflated")
  out4 <- marginaleffects::avg_predictions(m, variables = "spp")

  expect_equal(out1, out2$predicted, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1, out3$predicted, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1, out4$estimate, tolerance = 1e-3, ignore_attr = TRUE)
})
