.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest && require("testthat") && require("ggeffects") && require("gamlss")) {

  data(iris)
  m1 <- gamlss(Sepal.Length ~ Sepal.Width + random(Species),
                      sigma.formula = ~ Sepal.Width,
                      data = iris)

  test_that("ggpredict", {
    p <- ggpredict(m1, "Sepal.Width")
    expect_equal(p$predicted,
                 c(3.84881, 4.00918, 4.16956, 4.32994, 4.49031, 4.65069, 4.81107,
                   4.97144, 5.13182, 5.2922, 5.45257, 5.61295, 5.77333),
                 tolerance = 1e-2)
    expect_equal(colnames(p), c("x", "predicted", "std.error", "conf.low", "conf.high", "group"))
    expect_equal(p$predicted,
                 c(3.66712, 3.85232, 4.03678, 4.22004, 4.40113, 4.5782, 4.74796,
                   4.90714, 5.05622, 5.19881, 5.33789, 5.47507, 5.61117),
                 tolerance = 1e-2)
  })
}
