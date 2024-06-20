skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("rstanarm")

test_that("ggpredict, rstanarm-ppd", {
  x <- rnorm(30, 0)
  b <- runif(2)
  s <- ifelse(diag(2) == 0, 0.23, 1)
  er <- cbind(rnorm(30, 0, s), rnorm(30, 0, s))
  y <- apply(t(b), 2, `*`, x) + er
  d <- data.frame(y1 = y[,1], y2 = y[,2], x)
  d$group <- sample(c("a", "b", "c"), size = nrow(d), replace = TRUE)

  m1 <- suppressWarnings(rstanarm::stan_mvmer(
    list(
      y1 ~ x + (1 | group),
      y2 ~ x + (1 | group)
    ),
    data = d,
    chains = 2,
    iter = 500,
    refresh = 0
  ))

  m2 <- suppressWarnings(rstanarm::stan_glm(
    y1 ~ x,
    data = d,
    chains = 2,
    iter = 500,
    refresh = 0
  ))

  expect_s3_class(suppressWarnings(ggpredict(m1, interval = "prediction")), "ggalleffects")
  expect_s3_class(suppressWarnings(ggpredict(m1, "x", interval = "prediction")), "data.frame")
  expect_s3_class(suppressWarnings(ggpredict(m2, interval = "prediction")), "ggalleffects")
  expect_s3_class(suppressWarnings(ggpredict(m2, "x", interval = "prediction")), "data.frame")
  expect_error(ggpredict(m1, interval = "confidence"))
  expect_error(ggpredict(m1, "x", interval = "confidence"))
  expect_s3_class(ggpredict(m2, interval = "confidence"), "ggalleffects")
  expect_s3_class(ggpredict(m2, "x", interval = "confidence"), "data.frame")

  set.seed(123)
  out1 <- suppressWarnings(ggpredict(m1, "x", interval = "prediction"))
  set.seed(123)
  out2 <- suppressWarnings(ggpredict(m1, "x", interval = "prediction"))
  expect_equal(out1$predicted, out2$predicted, tolerance = 1e-3)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-3)
})
