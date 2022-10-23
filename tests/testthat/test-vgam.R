unloadNamespace("gam")

if (requiet("testthat") && requiet("ggeffects") && requiet("VGAM")) {
  data("hunua")
  m1 <- vgam(agaaus ~ vitluc + s(altitude, df = 2), binomialff, data = hunua)

  test_that("ggpredict", {
    p <- ggpredict(m1, "vitluc")
    expect_equal(p$predicted[1], 0.2745789, tolerance = 1e-3)
  })
}
