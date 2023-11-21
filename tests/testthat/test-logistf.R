skip_on_os(c("mac", "solaris"))
skip_if_not_installed("logistf", minimum_version = "1.26.0")
skip_if_not_installed("effects")
skip_if_not_installed("emmeans")

test_that("ggpredict, logistf", {
  data(sex2, package = "logistf")
  m1 <- logistf::logistf(case ~ age + oc, data = sex2)
  pr <- ggpredict(m1, "age")
  expect_equal(pr$predicted[1], 0.5660724, tolerance = 1e-3)
  # confidence intervals
  expect_equal(pr$conf.low, c(0.48239, 0.22581), tolerance = 1e-3)
  pr <- ggeffect(m1, "age")
  expect_equal(pr$predicted[1], 0.5762638, tolerance = 1e-3)
  pr <- ggemmeans(m1, "age")
  expect_equal(pr$predicted[1], 0.5660724, tolerance = 1e-3)
})

test_that("ggpredict, flic amd flac", {
  data(sex2, package = "logistf")
  m1 <- logistf::flic(logistf::logistf(case ~ age + oc, data = sex2))
  pr <- ggpredict(m1, "age")
  expect_equal(pr$predicted[1], 0.5660881, tolerance = 1e-3)
  expect_equal(pr$conf.low, c(0.51046, 0.25062), tolerance = 1e-3)
  m2 <- logistf::flac(logistf::logistf(case ~ age + oc, data = sex2), data = sex2)
  pr <- ggpredict(m2, "age")
  expect_equal(pr$predicted[1], 0.5660816, tolerance = 1e-3)
  expect_equal(pr$conf.low, c(0.48229, 0.2253), tolerance = 1e-3)
})
