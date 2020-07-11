.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest && Sys.getenv("USER") != "travis") {
  if (suppressWarnings(
    require("testthat") &&
    require("brms") &&
    require("ggeffects") &&
    require("insight")
  )) {
    m1 <- insight::download_model("brms_ordinal_1")
    m2 <- insight::download_model("brms_ordinal_1_wt")
    m3 <- insight::download_model("brms_categorical_1")
    m4 <- insight::download_model("brms_categorical_1_wt")

    test_that("ggpredict, brms-categ-cum", {
      ggpredict(m1, c("mpg"))
      ggpredict(m2, c("mpg"))
      ggpredict(m3, c("mpg"))
      ggpredict(m4, c("mpg"))
    })
  }
}
