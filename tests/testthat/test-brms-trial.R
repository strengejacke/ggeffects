.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest &&
  suppressWarnings(
    requiet("testthat") &&
    requiet("brms") &&
    requiet("ggeffects") &&
    requiet("insight")
  )) {
  m1 <- insight::download_model("brms_mixed_6")
  m2 <- insight::download_model("brms_mv_4")
  m3 <- insight::download_model("brms_2")

  test_that("ggpredict, brms-trial", {
    ggpredict(m1, c("Base", "Trt"))
    ggpredict(m2, "Species")
    ggpredict(m3, c("treat", "c2"))
  })
}
