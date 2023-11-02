skip_on_os(c("mac", "solaris"))
skip_if_not_installed("robustbase")
skip_if_not_installed("effects")
skip_if_not_installed("emmeans")

test_that("ggpredict, lmrob", {
  data(efc, package = "ggeffects")
  m1 <- robustbase::lmrob(neg_c_7 ~ c12hour + e42dep + c161sex + c172code, data = efc)
  pr <- ggpredict(m1, "c12hour")
  expect_equal(pr$predicted[1], 11.02581, tolerance = 1e-4)
  pr <- ggeffect(m1, "c12hour")
  expect_equal(pr$predicted[1], 11.02581, tolerance = 1e-4)
  expect_null(ggemmeans(m1, "c12hour", verbose = FALSE))
})
