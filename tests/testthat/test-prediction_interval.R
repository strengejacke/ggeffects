skip_on_os(c("mac", "solaris"))

test_that("ggpredict prediction interval, glm", {
  skip_if_not_installed("glmmTMB")
  data(efc, package = "ggeffects")

  # linear
  m_pr0 <- lm(barthtot ~ c172code, data = efc)
  out <- ggpredict(m_pr0, "c172code")
  expect_equal(out$predicted, c(62.91021, 64.69758, 66.48495), tolerance = 1e-3)
  expect_equal(out$conf.high, c(66.62681, 66.72464, 70.35304), tolerance = 1e-3)
  set.seed(123)
  out <- ggpredict(m_pr0, "c172code", type = "simulate")
  expect_equal(out$predicted, c(63.01721, 64.71984, 66.49318), tolerance = 1e-3)
  expect_equal(out$conf.high, c(120.8162, 122.3524, 124.02301), tolerance = 1e-3)
  out <- ggpredict(m_pr0, "c172code", interval = "prediction")
  expect_equal(out$predicted, c(62.91021, 64.69758, 66.48495), tolerance = 1e-3)
  expect_equal(out$conf.high, c(121.12437, 122.82833, 124.70898), tolerance = 1e-3)
  d <- data_grid(m_pr0, "c172code")
  out2 <- predict(m_pr0, newdata = d, interval = "prediction")
  expect_equal(out$conf.high, out2[, 3], tolerance = 1e-3, ignore_attr = TRUE)

  # poisson
  data(Salamanders, package = "glmmTMB")
  m_pr1 <- glm(count ~ mined, family = poisson(link = "log"), data = Salamanders)
  set.seed(123)
  out <- ggpredict(m_pr1, "mined", type = "simulate")
  expect_equal(out$predicted, c(0.2949, 2.26118), tolerance = 1e-3)
  expect_equal(out$conf.high, c(1.96461, 5.65952), tolerance = 1e-3)
  out <- ggpredict(m_pr1, "mined")
  expect_equal(out$predicted, c(0.29545, 2.26488), tolerance = 1e-3)
  expect_equal(out$conf.high, c(0.36284, 2.43165), tolerance = 1e-3)
  out <- ggpredict(m_pr1, "mined", interval = "prediction")
  expect_equal(out$predicted, c(0.29545, 2.26488), tolerance = 1e-3)
  expect_equal(out$conf.high, c(2, 6), tolerance = 1e-3)

  # logisitc
  skip_if(getRversion() < "4.3.0")
  data(efc, package = "ggeffects")
  efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
  m_pr2 <- glm(neg_c_7d ~ as.factor(c161sex), data = efc, family = binomial)
  out <- ggpredict(m_pr2, "c161sex")
  expect_equal(out$predicted, c(0.34906, 0.47423), tolerance = 1e-3)
  expect_equal(out$conf.high, c(0.41562, 0.51186), tolerance = 1e-3)
  out <- ggpredict(m_pr2, "c161sex", interval = "prediction")
  expect_equal(out$predicted, c(0.34906, 0.47423), tolerance = 1e-3)
  expect_equal(out$conf.high, c(0.79527, 0.8656), tolerance = 1e-3)
  expect_error(
    ggpredict(m_pr2, "c161sex", type = "simulate"),
    regex = "Can't simulate"
  )
})
