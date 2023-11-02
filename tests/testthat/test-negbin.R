skip_on_os(c("mac", "solaris"))
skip_if_not_installed("MASS")

test_that("ggpredict, negbin", {
  data(efc, package = "ggeffects")
  efc$e42dep <- to_label(efc$e42dep)
  fit <- MASS::glm.nb(
    tot_sc_e ~ neg_c_7 + I(neg_c_7^2) + neg_c_7:e42dep + I(neg_c_7^2):e42dep + c12hour + c172code,
    data = efc,
    init.theta = 1.133641349,
    link = log
  )
  expect_s3_class(ggpredict(fit, "neg_c_7"), "data.frame")
  expect_s3_class(ggeffect(fit, "neg_c_7"), "data.frame")
  # still fails on windows old-rel, so re-activate once emmeans is built on all platforms
  expect_null(ggemmeans(fit, "neg_c_7"))
  expect_s3_class(ggpredict(fit, c("neg_c_7", "e42dep")), "data.frame")
  expect_s3_class(ggeffect(fit, c("neg_c_7", "e42dep")), "data.frame")
  expect_null(ggemmeans(fit, c("neg_c_7", "e42dep")))
})

test_that("ggpredict, negbin", {
  data(efc, package = "ggeffects")
  fit <- MASS::glm.nb(
    tot_sc_e ~ neg_c_7 + I(neg_c_7^2) + neg_c_7:e42dep + I(neg_c_7^2):e42dep + c12hour + c172code,
    data = efc,
    init.theta = 1.133641349,
    link = log
  )
  expect_s3_class(ggpredict(fit, "neg_c_7"), "data.frame")
  expect_s3_class(ggeffect(fit, "neg_c_7"), "data.frame")
  expect_null(ggemmeans(fit, "neg_c_7"))
  expect_s3_class(ggpredict(fit, c("neg_c_7", "e42dep")), "data.frame")
  expect_s3_class(ggeffect(fit, c("neg_c_7", "e42dep")), "data.frame")
  expect_null(ggemmeans(fit, c("neg_c_7", "e42dep")))
})
