if (require("testthat") && require("ggeffects") && require("betareg")) {
  data("GasolineYield")
  m1 <- betareg(yield ~ batch + temp, data = GasolineYield)

  test_that("ggpredict", {
    p <- ggpredict(m1, "batch")
    expect_equal(p$predicted, unname(predict(m1, newdata = new_data(m1, "batch"))), tolerance = 1e-3)
    ggpredict(m1, c("batch", "temp"))
  })

  test_that("ggeffect", {
    p <- ggeffect(m1, "batch")
    expect_equal(p$predicted[1], 0.3122091, tolerance = 1e-3)
    ggeffect(m1, c("batch", "temp"))
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, "batch")
    expect_equal(p$predicted[1], 0.3122091, tolerance = 1e-3)
    ggemmeans(m1, c("batch", "temp"))
  })
}
