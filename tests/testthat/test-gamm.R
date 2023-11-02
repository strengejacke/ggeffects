skip_on_cran()
skip_on_os(c("mac", "solaris"))
unloadNamespace("gam")
skip_if_not_installed("mgcv")

test_that("ggpredict", {
  set.seed(0)
  dat <- mgcv::gamSim(6, n = 200, scale = 0.2, dist = "poisson")
  m1 <- mgcv::gamm(
    y ~ mgcv::s(x0) + mgcv::s(x1) + mgcv::s(x2),
    family = poisson,
    data = dat,
    random = list(fac = ~1),
    verbosePQL = FALSE
  )
  p <- ggpredict(m1, "x1")
  expect_equal(p$predicted[1], 15.5450060160087, tolerance = 1e-3)
  expect_s3_class(ggpredict(m1, c("x1", "x2")), "data.frame")
})
