skip_on_os(c("mac", "solaris"))
skip_if_not_installed("gam")
skip_if_not_installed("emmeans")
skip_if_not_installed("effects")

test_that("ggpredict", {
  data(kyphosis, package = "gam")
  m1 <- gam::gam(
    Kyphosis ~ gam::s(Age, 4) + Number,
    family = binomial,
    data = kyphosis,
    trace = FALSE
  )
  # ggpredict
  p <- ggpredict(m1, "Age")
  expect_equal(p$predicted[1], 0.1040038, tolerance = 1e-3)
  # validate against predict()
  expect_equal(
    p$predicted[1],
    plogis(predict(m1, newdata = data_grid(m1, "Age"))[1]),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_s3_class(ggpredict(m1, c("Age", "Number")), "data.frame")
  # ggeffect
  p <- ggeffect(m1, "Age")
  expect_equal(p$predicted[1], 0.1059569, tolerance = 1e-3)
  expect_s3_class(ggeffect(m1, c("Age", "Number")), "data.frame")
  # ggemmeans
  p <- ggemmeans(m1, "Age")
  expect_equal(p$predicted[1], 0.1040038, tolerance = 1e-3)
  expect_s3_class(ggemmeans(m1, c("Age", "Number")), "data.frame")
})
