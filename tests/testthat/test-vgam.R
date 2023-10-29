skip_if_not_installed("VGAM")
unloadNamespace("gam")

test_that("ggpredict", {
  data("hunua", package = "VGAM")
  m1 <- VGAM::vgam(agaaus ~ vitluc + VGAM::s(altitude, df = 2), VGAM::binomialff, data = hunua)
  p <- ggpredict(m1, "vitluc")
  expect_equal(p$predicted[1], 0.2745789, tolerance = 1e-3)
})
