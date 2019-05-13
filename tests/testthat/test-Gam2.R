if (require("testthat") && require("ggeffects") && require("gam")) {

  data(kyphosis)
  m1 <- gam::gam(
    Kyphosis ~ s(Age, 4) + Number,
    family = binomial,
    data = kyphosis,
    trace = TRUE
  )

  test_that("ggpredict", {
    p <- ggpredict(m1, "Age")
    expect_equal(p$predicted[1], 0.02099814, tolerance = 1e-3)
    ggpredict(m1, c("Age", "Number"))
  })

  test_that("ggeffect", {
    p <- ggeffect(m1, "Age")
    expect_equal(p$predicted[1], 0.106151, tolerance = 1e-3)
    ggeffect(m1, c("Age", "Number"))
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, "Age")
    expect_equal(p$predicted[1], 0.02099814, tolerance = 1e-3)
    ggemmeans(m1, c("Age", "Number"))
  })
}
