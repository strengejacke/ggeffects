skip_on_os(c("mac", "solaris"))
skip_if_not_installed("geepack")
skip_if_not_installed("emmeans")

test_that("ggpredict", {
  data(dietox, package = "geepack")

  m1 <- suppressWarnings(geepack::geeglm(
    Weight ~ Cu * Time + I(Time^2) + I(Time^3),
    data = dietox,
    id = Pig,
    family = poisson("identity"),
    corstr = "ar1"
  ))

  m2 <- suppressWarnings(geepack::geeglm(
    Weight ~ Cu * Time + I(Time^2) + I(Time^3),
    data = dietox,
    id = Pig,
    family = poisson()
  ))

  p <- ggpredict(m1, c("Cu", "Time"))
  expect_equal(p$predicted[1], 35.47711, tolerance = 1e-2)
  p <- ggemmeans(m1, c("Cu", "Time"))
  expect_equal(p$predicted[1], 35.47711, tolerance = 1e-2)
  p <- ggpredict(m2, c("Cu", "Time"))
  expect_equal(p$predicted[1], 35.63929, tolerance = 1e-2)
  p <- ggemmeans(m2, c("Cu", "Time"))
  expect_equal(p$predicted[1], 35.63929, tolerance = 1e-2)
})
