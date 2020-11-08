if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjmisc") &&
  require("MASS")
)) {
  data(efc)
  efc$e42dep <- to_label(efc$e42dep)
  fit <-
    glm.nb(
      tot_sc_e ~ neg_c_7 + I(neg_c_7 ^ 2) + neg_c_7:e42dep + I(neg_c_7 ^ 2):e42dep + c12hour + c172code,
      data = efc,
      init.theta = 1.133641349,
      link = log
    )

  test_that("ggpredict, negbin", {
    expect_s3_class(ggpredict(fit, "neg_c_7"), "data.frame")
    expect_s3_class(ggeffect(fit, "neg_c_7"), "data.frame")
    # still fails on windows old-rel, so re-activate once emmeans is built on all platforms
    # expect_null(ggemmeans(fit, "neg_c_7"))
    expect_s3_class(ggpredict(fit, c("neg_c_7", "e42dep")), "data.frame")
    expect_s3_class(ggeffect(fit, c("neg_c_7", "e42dep")), "data.frame")
    # expect_null(ggemmeans(fit, c("neg_c_7", "e42dep")))
  })

  data(efc)
  fit <-
    glm.nb(
      tot_sc_e ~ neg_c_7 + I(neg_c_7 ^ 2) + neg_c_7:e42dep + I(neg_c_7 ^ 2):e42dep + c12hour + c172code,
      data = efc,
      init.theta = 1.133641349,
      link = log
    )

  test_that("ggpredict, negbin", {
    expect_s3_class(ggpredict(fit, "neg_c_7"), "data.frame")
    expect_s3_class(ggeffect(fit, "neg_c_7"), "data.frame")
    # expect_null(ggemmeans(fit, "neg_c_7"))
    expect_s3_class(ggpredict(fit, c("neg_c_7", "e42dep")), "data.frame")
    expect_s3_class(ggeffect(fit, c("neg_c_7", "e42dep")), "data.frame")
    # expect_null(ggemmeans(fit, c("neg_c_7", "e42dep")))
  })
}
