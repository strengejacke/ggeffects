.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest && getRversion() >= "3.6.0") {

  if (requiet("testthat") && requiet("ggeffects") && requiet("ordinal") && requiet("MASS")) {

    data(housing)
    m1 <- clm2(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

    test_that("ggpredict", {
      expect_error(ggpredict(m1, "Infl"))
    })

    test_that("ggeffect", {
      p <- ggeffect(m1, "Infl")
      expect_equal(p$predicted[1], 0.457877729905463, tolerance = 1e-3)
      expect_s3_class(ggeffect(m1, c("Infl", "Type")), "data.frame")
    })

    test_that("ggemmeans", {
      expect_error(ggemmeans(m1, "Infl"))
    })
  }

}
