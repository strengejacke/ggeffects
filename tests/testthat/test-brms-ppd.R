.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (FALSE && .runThisTest && suppressWarnings(requiet("testthat") && requiet("brms") && requiet("ggeffects"))) {

  ## TODO enable once rstan works w/o problems again...
  x <- rnorm(10, 0)
  b <- runif(2)
  s <- ifelse(diag(2) == 0, 0.23, 1)
  er <- cbind(rnorm(10, 0, s), rnorm(10, 0, s))
  y <- apply(t(b), 2, `*`, x) + er
  d <- data.frame(y1 = y[, 1], y2 = y[, 2], x)
  m1 <- suppressWarnings(brm(bf(mvbind(y1, y2) ~ 1 + x) + set_rescor(TRUE), data = d, chains = 2, iter = 500, refresh = 0))
  m2 <- suppressWarnings(brm(y1 ~ x, data = d, chains = 2, iter = 500, refresh = 0))

  test_that("ggpredict, brms-ppd", {
    expect_type(ggpredict(m1, ppd = TRUE), "ggalleffects")
    expect_s3_class(ggpredict(m1, "x", ppd = TRUE), "data.frame")
    expect_type(ggpredict(m2, ppd = TRUE), "ggalleffects")
    expect_s3_class(ggpredict(m2, "x", ppd = TRUE), "data.frame")
  })

  test_that("ggpredict, brms-ppd", {
    expect_type(ggpredict(m1, ppd = FALSE), "ggalleffects")
    expect_s3_class(ggpredict(m1, "x", ppd = FALSE), "data.frame")
    expect_type(ggpredict(m2, ppd = FALSE), "ggalleffects")
    expect_s3_class(ggpredict(m2, "x", ppd = FALSE), "data.frame")
  })
}
