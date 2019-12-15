if (require("testthat") && require("ggeffects") && require("geepack")) {
  data(dietox)

  m1 <- geeglm(
    Weight ~ Cu * Time + I(Time ^ 2) + I(Time ^ 3),
    data = dietox,
    id = Pig,
    family = poisson("identity"),
    corstr = "ar1"
  )

  test_that("ggpredict", {
    p <- ggpredict(m1, c("Cu", "Time"))
    expect_equal(p$predicted[1], 35.47711, tolerance = 1e-2)
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, c("Cu", "Time"))
    expect_equal(p$predicted[1], 35.47711, tolerance = 1e-2)
  })
}
