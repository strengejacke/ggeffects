.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest &&
    suppressWarnings(
    require("testthat") &&
    require("brms") &&
    require("ggeffects") &&
    require("insight")
  )) {
  m1 <- insight::download_model("brms_ordinal_1")
  m2 <- insight::download_model("brms_ordinal_1_wt")

  m3 <- insight::download_model("brms_categorical_1_num")
  m4 <- insight::download_model("brms_categorical_1_fct")
  m5 <- insight::download_model("brms_categorical_1_wt")

  test_that("ggpredict, brms-categ-cum", {
    p1 <- ggpredict(m1, c("mpg"))
    p2 <- ggpredict(m2, c("mpg"))

    p3 <- ggpredict(m3, c("mpg"))
    p4 <- ggpredict(m4, c("mpg"))
    p5 <- ggpredict(m5, c("mpg"))

    # m3/m4 are the same, except response is numeric/factor, so predictions should be the same
    p4$response.level <- as.numeric(p4$response.level)
    for (resp.level in c(3:5)) {
      expect_equal(
        p3[p3$response.level == resp.level, ],
        p4[p4$response.level == resp.level, ],
        ignore_attr = TRUE, tolerance = 0.05
      )
    }
  })
}
