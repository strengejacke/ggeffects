unloadNamespace("gam")

.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest &&
    requiet("testthat") &&
    requiet("ggeffects") &&
    requiet("mgcv") &&
    getRversion() > "3.5") {

  set.seed(0)
  dat <- gamSim(6, n = 200, scale = .2, dist = "poisson")
  m1 <-
    gamm(
      y ~ s(x0) + s(x1) + s(x2),
      family = poisson,
      data = dat,
      random = list(fac = ~ 1),
      verbosePQL = FALSE
    )

  test_that("ggpredict", {
    p <- ggpredict(m1, "x1")
    expect_equal(p$predicted[1], 15.5450060160087, tolerance = 1e-3)
    expect_s3_class(ggpredict(m1, c("x1", "x2")), "data.frame")
  })
}
