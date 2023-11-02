skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("brms")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("ggpredict, brms-categ-cum", {
    data(mtcars)
    m1 <- insight::download_model("brms_ordinal_1")
    m2 <- insight::download_model("brms_ordinal_1_wt")

    m3 <- insight::download_model("brms_categorical_1_num")
    m4 <- insight::download_model("brms_categorical_1_fct")
    m5 <- insight::download_model("brms_categorical_1_wt")

    p1 <- ggpredict(m1, "mpg")
    p2 <- ggpredict(m2, "mpg")

    p3 <- ggpredict(m3, "mpg")
    p4 <- ggpredict(m4, "mpg")
    p5 <- ggpredict(m5, "mpg")

    # m3/m4 are the same, except response is numeric/factor, so predictions should be the same
    p4$response.level <- as.numeric(p4$response.level)
    for (resp.level in 3:5) {
      expect_equal(
        p3[p3$response.level == resp.level, ],
        p4[p4$response.level == resp.level, ],
        ignore_attr = TRUE, tolerance = 0.05
      )
    }
  })
)
