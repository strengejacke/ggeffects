unloadNamespace("gam")

if (require("testthat") && require("ggeffects") && require("mgcv")) {

  set.seed(0)
  dat <- gamSim(6, n = 200, scale = .2, dist = "poisson")
  m1 <-
    gamm(
      y ~ s(x0) + s(x1) + s(x2),
      family = poisson,
      data = dat,
      random = list(fac = ~ 1)
    )

  test_that("ggpredict", {
    p <- ggpredict(m1, "x1")
    expect_equal(p$predicted[1], 2.743739, tolerance = 1e-3)
    ggpredict(m1, c("x1", "x2"))
  })

}
