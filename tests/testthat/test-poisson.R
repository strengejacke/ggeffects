skip_on_os(c("mac", "solaris"))
skip_if_not_installed("effects")

test_that("ggpredict and ggeffect, glm", {
  data(efc, package = "ggeffects")
  fit <- glm(tot_sc_e ~ neg_c_7 + c12hour + e42dep + c161sex + c172code, data = efc, family = poisson(link = "log"))
  expect_s3_class(ggpredict(fit, "c12hour"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex")), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
  expect_s3_class(ggeffect(fit, "c12hour"), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c161sex")), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
})
