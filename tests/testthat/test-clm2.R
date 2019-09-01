if (require("testthat") && require("ggeffects") && require("ordinal") && require("MASS")) {
  data(housing)
  m1 <- clm2(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  test_that("ggpredict", {
    expect_error(ggpredict(m1, "Infl"))
  })

  test_that("ggeffect", {
    p <- ggeffect(m1, "Infl")
    expect_equal(p$predicted[1], 0.457877729905463, tolerance = 1e-3)
    ggeffect(m1, c("Infl", "Type"))
  })

  test_that("ggemmeans", {
    expect_error(ggemmeans(m1, "Infl"))
  })
}
