.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest && Sys.getenv("USER") != "travis") {
  if (suppressWarnings(
    require("testthat") &&
    require("brms") &&
    require("ggeffects") &&
    require("insight")
  )) {
    m1 <- insight::download_model("brms_mixed_6")
    m2 <- insight::download_model("brms_mv_4")
    m3 <- insight::download_model("brms_2")

    test_that("ggpredict, brms-trial", {
      ggpredict(m1, c("Base", "Trt"))
      ggpredict(m2, "Species")
      ggpredict(m3, c("treat", "c2"))
    })

    test_that("ggpredict, brms-trial", {
      p1 <- ggpredict(m1, c("Base", "Trt"))
      p2 <- ggemmeans(m1, c("Base", "Trt"))
      expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)
    })

  }
}
