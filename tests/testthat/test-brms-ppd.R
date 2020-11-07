.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest) {

  if (suppressWarnings(
    require("testthat") &&
    require("brms") &&
    require("ggeffects")
  )) {

    x <- rnorm(10, 0)
    b <- runif(2)
    s <- ifelse(diag(2) == 0, 0.23, 1)
    er <- cbind(rnorm(10, 0, s), rnorm(10, 0, s))
    y <- apply(t(b), 2, `*`, x) + er
    d <- data.frame(y1 = y[,1], y2 = y[,2], x)
    m1 <- brm(mvbind(y1, y2) ~ 1 + x, data = d, chains = 2, iter = 500)
    m2 <- brm(y1 ~ x, data = d, chains = 2, iter = 500)

    test_that("ggpredict, brms-ppd", {
      ggpredict(m1, ppd = TRUE)
      ggpredict(m1, "x", ppd = TRUE)
      ggpredict(m2, ppd = TRUE)
      ggpredict(m2, "x", ppd = TRUE)
    })

    test_that("ggpredict, brms-ppd", {
      ggpredict(m1, ppd = FALSE)
      ggpredict(m1, "x", ppd = FALSE)
      ggpredict(m2, ppd = FALSE)
      ggpredict(m2, "x", ppd = FALSE)
    })
  }
}
