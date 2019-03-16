unloadNamespace("gam")

if (require("testthat") && require("ggeffects") && require("VGAM")) {
  data("hunua")
  m1 <- vgam(agaaus ~ vitluc + s(altitude, df = 2), binomialff, data = hunua)

  test_that("ggpredict", {
    p <- ggpredict(m1, "vitluc")
    expect_equal(p$predicted[1], 0.2751634, tolerance = 1e-3)
  })
}
