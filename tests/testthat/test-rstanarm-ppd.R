.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest) {

  if (suppressWarnings(
    require("testthat") &&
    requireNamespace("rstanarm") &&
    require("ggeffects")
  )) {
    context("ggeffects, rstanarm-ppd")

    x <- rnorm(30, 0)
    b <- runif(2)
    s <- ifelse(diag(2) == 0, 0.23, 1)
    er <- cbind(rnorm(10, 0, s), rnorm(10, 0, s))
    y <- apply(t(b), 2, `*`, x) + er
    d <- data.frame(y1 = y[,1], y2 = y[,2], x)
    d$group <- sample(c("a", "b", "c"), size = nrow(d), replace = TRUE)
    m1 <- rstanarm::stan_mvmer(
      list(
        y1 ~ x + (1 | group),
        y2 ~ x + (1 | group)
      ),
      data = d,
      chains = 2,
      iter = 500
    )
    m2 <- rstanarm::stan_glm(y1 ~ x, data = d, chains = 2, iter = 500)

    test_that("ggpredict, rstanarm-ppd", {
      ggpredict(m1, ppd = TRUE)
      ggpredict(m1, "x", ppd = TRUE)
      ggpredict(m2, ppd = TRUE)
      ggpredict(m2, "x", ppd = TRUE)
    })

    test_that("ggpredict, rstanarm-ppd", {
      expect_error(ggpredict(m1, ppd = FALSE))
      expect_error(ggpredict(m1, "x", ppd = FALSE))
      ggpredict(m2, ppd = FALSE)
      ggpredict(m2, "x", ppd = FALSE)
    })
  }
}
