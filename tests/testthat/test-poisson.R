if (suppressWarnings(
  requiet("testthat") &&
  requiet("haven") &&
  requiet("sjlabelled") &&
  requiet("ggeffects")
)) {
  # glm, poisson regression ----
  data(efc, package = "ggeffects")
  fit <- glm(tot_sc_e ~ neg_c_7 + c12hour + e42dep + c161sex + c172code, data = efc, family = poisson(link = "log"))

  test_that("ggpredict, glm", {
    expect_s3_class(ggpredict(fit, "c12hour"), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex")), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
  })

  test_that("ggeffect, glm", {
    expect_s3_class(ggeffect(fit, "c12hour"), "data.frame")
    expect_s3_class(ggeffect(fit, c("c12hour", "c161sex")), "data.frame")
    expect_s3_class(ggeffect(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
  })
}
