skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("brms")

test_that("ggpredict, brms-ppd", {
  x <- rnorm(10, 0)
  b <- runif(2)
  s <- ifelse(diag(2) == 0, 0.23, 1)
  er <- cbind(rnorm(10, 0, s), rnorm(10, 0, s))
  y <- apply(t(b), 2, `*`, x) + er
  d <- data.frame(y1 = y[, 1], y2 = y[, 2], x)
  m1 <- suppressWarnings(brms::brm(
    brms::bf(mvbind(y1, y2) ~ 1 + x) + brms::set_rescor(TRUE),
    data = d,
    chains = 2,
    iter = 500,
    refresh = 0
  ))
  m2 <- suppressWarnings(brms::brm(
    y1 ~ x,
    data = d,
    chains = 2,
    iter = 500,
    refresh = 0
  ))
  expect_s3_class(suppressWarnings(ggpredict(m1, interval = "prediction", verbose = FALSE)), c("ggalleffects", "list"))
  expect_s3_class(suppressWarnings(ggpredict(m1, "x", interval = "prediction", verbose = FALSE)), "data.frame")
  expect_s3_class(ggpredict(m1, interval = "prediction", verbose = FALSE), c("ggalleffects", "list"))
  expect_s3_class(ggpredict(m1, "x", interval = "prediction", verbose = FALSE), "data.frame")
  expect_s3_class(suppressWarnings(ggpredict(m2, interval = "prediction")), c("ggalleffects", "list"))
  expect_s3_class(suppressWarnings(ggpredict(m2, "x", interval = "prediction")), "data.frame")
  expect_s3_class(ggpredict(m1, interval = "confidence", verbose = FALSE), c("ggalleffects", "list"))
  expect_s3_class(ggpredict(m1, "x", interval = "confidence", verbose = FALSE), "data.frame")
  expect_s3_class(ggpredict(m2, interval = "confidence"), c("ggalleffects", "list"))
  expect_s3_class(ggpredict(m2, "x", interval = "confidence"), "data.frame")

  set.seed(123)
  out1 <- suppressWarnings(ggpredict(m1, "x", interval = "prediction"))
  set.seed(123)
  out2 <- ggpredict(m1, "x", interval = "prediction")
  expect_equal(out1$predicted, out2$predicted, tolerance = 1e-3)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-3)
})
