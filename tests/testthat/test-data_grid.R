skip_on_os(c("mac", "solaris"))
test_that("data_grid", {
  data(efc, package = "ggeffects")
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

  # defaults
  nd <- new_data(fit, c("c12hour [meansd]", "c161sex"))
  pr <- ggpredict(fit, c("c12hour [meansd]", "c161sex"))
  expect_equal(
    predict(fit, type = "response", newdata = nd),
    pr$predicted[order(pr$group, pr$x)],
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  # typical = "median"
  nd <- new_data(fit, c("c12hour [meansd]", "c161sex"), typical = "median")
  pr <- ggpredict(fit, c("c12hour [meansd]", "c161sex"), typical = "median")
  expect_equal(
    predict(fit, type = "response", newdata = nd),
    pr$predicted[order(pr$group, pr$x)],
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  # condition
  nd <- new_data(fit, c("c12hour [meansd]", "c161sex"), condition = c(c172code = 1))
  pr <- ggpredict(fit, c("c12hour [meansd]", "c161sex"), condition = c(c172code = 1))
  expect_equal(
    predict(fit, type = "response", newdata = nd),
    pr$predicted[order(pr$group, pr$x)],
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})
