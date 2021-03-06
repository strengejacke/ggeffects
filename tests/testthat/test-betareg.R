if (require("testthat") &&
    require("ggeffects") &&
    require("emmeans") &&
    require("effects") &&
    require("betareg")) {
  data("GasolineYield")
  m1 <- betareg(yield ~ batch + temp, data = GasolineYield)

  test_that("ggpredict", {
    p <- ggpredict(m1, "batch")
    expect_equal(p$predicted, unname(predict(m1, newdata = new_data(m1, "batch"))), tolerance = 1e-3)
    expect_s3_class(ggpredict(m1, c("batch", "temp")), "data.frame")
  })

  test_that("ggeffect", {
    p <- ggeffect(m1, "batch")
    expect_equal(p$predicted[1], 0.3122091, tolerance = 1e-3)
    expect_s3_class(ggeffect(m1, c("batch", "temp")), "data.frame")
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, "batch")
    expect_equal(p$predicted[1], 0.3122091, tolerance = 1e-3)
    expect_s3_class(ggemmeans(m1, c("batch", "temp")), "data.frame")
  })


  if (getRversion() >= "4.0.0") {
    #create df
    df2 <-
      data.frame(
        ratio = c(0.5, 0.5, 0.6, 0.6, 0.7, 0.8, 0.9, 0.9),
        GD = c(0.5, 0.4, 0.6, 0.7, 0.8, 1.0, 1.0, 1.0),
        Source_Salinity = c(
          "Brackish",
          "Fresh",
          "Brackish",
          "Fresh",
          "Brackish",
          "Fresh",
          "Fresh",
          "Brackish"
        )
      )
    #run beta model
    m <- betareg(ratio ~ GD + Source_Salinity, data = df2)
    test_that("ggpredict", {
      p <- ggemmeans(m, "GD")
      expect_equal(p$conf.low, c(0.34618, 0.43997, 0.53218, 0.61566, 0.68704, 0.79684), tolerance = 1e-2)
    })
  }
}
