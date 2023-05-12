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
    out3 <- ggpredict(m, "Species", type = "random", verbose = FALSE)
    expect_equal(out1$conf.low, c(3.85055, 5.29633, 5.78545), tolerance = 1e-3)
    expect_equal(out2$conf.low, c(4.57423, 6.02764, 6.52735), tolerance = 1e-3)
    expect_message(print(out1), regex = "prediction")
    expect_no_message(print(out3))
    expect_message(
      print(hypothesis_test(out1)),
      regex = "Intervals"
    )
    expect_no_message(
      print(hypothesis_test(out1, verbose = FALSE))
    )
  })

  # lmer ----
  test_that("ggpredict, lmer prediction intervals random effects", {
    data(iris)
    m <- lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
    out1 <- ggpredict(m, "Species", type = "random")
    expect_null(out1$conf.low)

    out1 <- ggpredict(m, c("Sepal.Width [2:4]", "Species"), type = "random")
    out2 <- ggpredict(m, c("Sepal.Width [2:4]", "Species"), type = "random", interval = "confidence")
    expect_equal(
      out1$conf.low,
      c(
        2.41206, 3.86114, 4.34869, 3.22614, 4.67521, 5.16276, 4.00984,
        5.45891, 5.94646
      ),
      tolerance = 1e-3
    )
    expect_equal(
      out2$conf.low,
      c(
        2.69634, 4.14542, 4.63296, 3.51457, 4.96365, 5.45119, 4.29495, 
        5.74403, 6.23158
      ),
      tolerance = 1e-3
    )
  })
}
