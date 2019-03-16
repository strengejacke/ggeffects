if (require("testthat") && require("ggeffects") && require("ordinal") && require("MASS")) {
  data(wine, package = "ordinal")
  m1 <- clm(rating ~ temp * contact, data = wine)

  test_that("ggpredict", {
    p <- ggpredict(m1, "temp")
    expect_equal(p$predicted[1], 0.19604, tolerance = 1e-5)
    ggpredict(m1, c("temp", "contact"))
  })

  test_that("ggeffect", {
    p <- ggeffect(m1, "temp")
    expect_equal(p$predicted[1], 0.34979, tolerance = 1e-5)
    ggeffect(m1, c("temp", "contact"))
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, "contact")
    expect_equal(p$predicted[1], 0.10971, tolerance = 1e-5)
    ggemmeans(m1, c("temp", "contact"))
  })
}
