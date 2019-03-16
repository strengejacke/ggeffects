unloadNamespace("gam")

if (require("testthat") && require("ggeffects") && require("gamm4")) {

  set.seed(0)
  dat <- gamSim(1, n = 400, scale = 2) ## simulate 4 term additive truth
  dat$fac <- fac <- as.factor(sample(1:20, 400, replace = TRUE))
  dat$y <- dat$y + model.matrix( ~ fac - 1) %*% rnorm(20) * .5

  m1 <- gamm4(y ~ s(x0) + x1 + s(x2), data = dat, random =  ~ (1 | fac))

  test_that("ggpredict", {
    p <- ggpredict(m1, "x1")
    expect_equal(p$predicted[1], 4.91634, tolerance = 1e-3)
    ggpredict(m1, c("x1", "x2"))
  })

}
