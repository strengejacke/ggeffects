skip_on_os(c("mac", "solaris"))
skip_if_not_installed("survival", minimum_version = "3.2.9")
skip_if_not_installed("emmeans")
skip_if_not_installed("effects")

test_that("ggpredict, survreg", {
  m1 <- survival::survreg(
    survival::Surv(futime, fustat) ~ ecog.ps + rx,
    data = survival::ovarian, dist = "exponential"
  )
  pr <- ggpredict(m1, "ecog.ps")
  expect_equal(pr$predicted[1], 1637.551, tolerance = 1e-4)
  expect_null(ggeffect(m1, "ecog.ps"))
  pr <- ggemmeans(m1, "ecog.ps")
  expect_equal(pr$predicted[1], 1637.551, tolerance = 1e-4)
})


test_that("ggpredict, survreg, quantile", {
  data("lung2")
  fit <- survival::survreg(
    survival::Surv(time, status) ~ ph.ecog + age + sex,
    data = lung2,
    dist = "weibull"
  )
  out <- predict_response(fit, "sex", type = "quantile")
  out2 <- prdat <- predict(fit, newdata = data_grid(fit, "sex"), type = "quantile")

  expect_named(
    out,
    c("x", "predicted", "std.error", "conf.low", "conf.high", "response.level", "group")
  )
  expect_equal(
    out$predicted[order(out$response.level)],
    c(out2[, 1], out2[, 2]),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out$conf.low,
    c(63.80253, 662.8553, 91.94941, 904.90683),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out$predicted,
    c(93.02258, 891.08697, 138.31619, 1324.96601),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # muliple time points for quantiles
  out <- predict_response(fit, "sex", type = "quantile", p = 0.5)
  out2 <- predict(fit, newdata = data_grid(fit, "sex"), type = "quantile", p = 0.5)
  expect_equal(out$predicted, out2,  tolerance = 1e-4, ignore_attr = TRUE)

  out <- predict_response(fit, "sex", type = "quantile", p = c(0.1, 0.5, 0.8))
  out2 <- predict(fit, newdata = data_grid(fit, "sex"), type = "quantile", p = c(0.1, 0.5, 0.8))
  expect_equal(
    out$predicted[order(out$response.level)],
    c(out2[, 1], out2[, 2], out2[, 3]),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
