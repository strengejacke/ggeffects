skip_if_not_installed("logistf", minimum_version = "1.25.0")
skip_if_not_installed("effects")
skip_if_not_installed("emmeans")

test_that("ggpredict, logistf", {
  data(sex2, package = "logistf")
  m1 <- logistf::logistf(case ~ age + oc, data = sex2)
  pr <- ggpredict(m1, "age")
  expect_equal(pr$predicted[1], 0.5763746, tolerance = 1e-3)
  pr <- ggeffect(m1, "age")
  expect_equal(pr$predicted[1], 0.5762638, tolerance = 1e-3)
  pr <- ggemmeans(m1, "age")
  expect_equal(pr$predicted[1], 0.5660724, tolerance = 1e-3)
})
