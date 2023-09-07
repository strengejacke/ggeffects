.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest && suppressWarnings(requiet("testthat") && requiet("ggeffects") && requiet("sandwich"))) {
  test_that("ggpredict, vcov can be own function", {
    # example taken from "?clubSandwich::vcovCR"
    m <- 8
    cluster <- factor(rep(LETTERS[1:m], 3 + rpois(m, 5)))
    n <- length(cluster)
    X <- matrix(rnorm(3 * n), n, 3)
    nu <- rnorm(m)[cluster]
    e <- rnorm(n)
    y <- X %*% c(0.4, 0.3, -0.3) + nu + e
    dat <- data.frame(y, X, cluster, row = 1:n)

    # fit linear model
    model_vcov <- lm(y ~ X1 + X2 + X3, data = dat)
    out1 <- ggpredict(model_vcov, "X1", vcov_fun = "vcovHC", vcov_type = "HC0")
    out2 <- ggpredict(model_vcov, "X1", vcov_fun = sandwich::vcovHC, vcov_args = list(type = "HC0"))
    expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4)
  })
}
