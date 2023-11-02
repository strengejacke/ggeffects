skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("brms")

test_that("ggpredict, brms-trial", {
  m1 <- suppressWarnings(insight::download_model("brms_mixed_6"))
  m2 <- insight::download_model("brms_mv_4")
  m3 <- insight::download_model("brms_2")

  skip_if(is.null(m1) || is.null(m2) || is.null(m3))
  ggpredict(m1, c("Base", "Trt"))
  ggpredict(m2, "Species")
  ggpredict(m3, c("treat", "c2"))
})
