.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest && suppressWarnings(
  requiet("testthat") &&
  requiet("ggeffects") &&
  requiet("glmmTMB") &&
  requiet("lme4")
)) {
  # glmmTMB ----
  test_that("ggpredict, glmmTMB prediction intervals random effects", {
    data(iris)
    m <- glmmTMB(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
    out1 <- ggpredict(m, "Species", type = "random")
    out2 <- ggpredict(m, "Species", type = "random", interval = "confidence")
    expect_equal(out1$conf.low, c(3.85055, 5.29633, 5.78545), tolerance = 1e-3)
    expect_equal(out2$conf.low, c(4.57423, 6.02764, 6.52735), tolerance = 1e-3)
  })
}
