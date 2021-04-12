if (require("testthat") && require("ggeffects") && require("gam") && getRversion() >= "3.6.0") {

  data(kyphosis)
  m1 <- gam::gam(
    Kyphosis ~ s(Age, 4) + Number,
    family = binomial,
    data = kyphosis,
    trace = FALSE
  )

  test_that("ggpredict", {
    p <- ggpredict(m1, "Age")
    expect_equal(p$predicted[1], 0.02043849, tolerance = 1e-3)
    expect_s3_class(ggpredict(m1, c("Age", "Number")), "data.frame")
  })

  test_that("ggeffect", {
    p <- ggeffect(m1, "Age")
    expect_equal(p$predicted[1], 0.106151, tolerance = 1e-3)
    expect_s3_class(ggeffect(m1, c("Age", "Number")), "data.frame")
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, "Age")
    expect_equal(p$predicted[1], 0.02043849, tolerance = 1e-3)
    expect_s3_class(ggemmeans(m1, c("Age", "Number")), "data.frame")
  })
}
