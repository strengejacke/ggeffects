if (require("testthat") && require("ggeffects") && require("ordinal") && require("MASS")) {
  data(wine, package = "ordinal")
  m1 <- clmm(rating ~ temp + contact + (1|judge), data = wine)

  test_that("ggpredict", {
    p <- ggpredict(m1, "temp")
    expect_equal(p$predicted[1], 0.09761, tolerance = 1e-5)
    ggpredict(m1, c("temp", "contact"))
  })

  test_that("ggeffect", {
    p <- ggeffect(m1, "temp")
    expect_equal(p$predicted[1], 0.32018, tolerance = 1e-5)
    ggeffect(m1, c("temp", "contact"))
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, "contact")
    expect_equal(p$predicted[1], 0.08692, tolerance = 1e-5)
    ggemmeans(m1, c("temp", "contact"))
  })
}
