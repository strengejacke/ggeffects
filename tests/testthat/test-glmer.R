skip_on_os(c("mac", "solaris"))
skip_on_cran()

skip_if_not_installed("lme4")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("emmeans")
skip_if_not_installed("effects")
skip_if_not_installed("withr")


test_that("ggpredict, lme4::glmer", {
  data(efc_test)
  fit <- lme4::glmer(
    negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
    data = efc_test,
    family = binomial(link = "logit")
  )
  pr <- ggpredict(fit, "c12hour", verbose = FALSE)
  expect_equal(
    pr$predicted,
    c(0.34217, 0.34406, 0.34596, 0.34787, 0.34978, 0.3517, 0.35362,
      0.35554, 0.35747, 0.35941, 0.36135, 0.36329, 0.36524, 0.36719,
      0.36915, 0.37111, 0.37307, 0.37504, 0.37702, 0.37899, 0.38098,
      0.38296, 0.38495, 0.38694, 0.38894, 0.39094, 0.39295, 0.39496,
      0.39697, 0.39898, 0.401, 0.40302, 0.40505, 0.40708, 0.40911),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_message(ggpredict(fit, "c12hour"), "prettified")
  expect_silent(ggpredict(fit, "c12hour", verbose = FALSE))
  expect_s3_class(ggpredict(fit, "c12hour", verbose = FALSE), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), verbose = FALSE), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), verbose = FALSE), "data.frame")
  expect_s3_class(ggpredict(fit, "c12hour", type = "random", verbose = FALSE), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), type = "random", verbose = FALSE), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "random", verbose = FALSE), "data.frame")
})


test_that("ggpredict, lme4::glmer, conf int, validate against predict", {
  data(efc_test)
  fit <- lme4::glmer(
    negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
    data = efc_test,
    family = binomial(link = "logit")
  )
  nd <- data_grid(fit, "c12hour")
  pr <- ggpredict(fit, "c12hour", verbose = FALSE)
  pr2 <- suppressWarnings(predict(
    fit,
    newdata = nd,
    se.fit = TRUE,
    re.form = NA,
    allow.new.levels = TRUE,
    type = "link"
  ))
  expect_equal(
    pr$predicted,
    plogis(pr2$fit),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    pr$conf.low,
    plogis(pr2$fit - qt(0.975, Inf) * pr2$se.fit),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})


test_that("ggeffect, lme4::glmer", {
  data(efc_test)
  fit <- lme4::glmer(
    negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
    data = efc_test,
    family = binomial(link = "logit")
  )
  pr <- ggeffect(fit, "c12hour")
  expect_equal(
    pr$predicted,
    c(0.34217, 0.34406, 0.34596, 0.34787, 0.34978, 0.3517, 0.35362,
      0.35554, 0.35747, 0.35941, 0.36135, 0.36329, 0.36524, 0.36719,
      0.36915, 0.37111, 0.37307, 0.37504, 0.37702, 0.37899, 0.38098,
      0.38296, 0.38495, 0.38694, 0.38894, 0.39094, 0.39295, 0.39496,
      0.39697, 0.39898, 0.401, 0.40302, 0.40505, 0.40708, 0.40911),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    pr$conf.low,
    c(
      0.24901, 0.25138, 0.25363, 0.25576, 0.25777, 0.25965, 0.2614,
      0.26302, 0.2645, 0.26585, 0.26706, 0.26814, 0.26909, 0.2699,
      0.27059, 0.27115, 0.2716, 0.27192, 0.27214, 0.27225, 0.27226,
      0.27217, 0.272, 0.27173, 0.27139, 0.27096, 0.27047, 0.26991,
      0.26928, 0.2686, 0.26786, 0.26707, 0.26623, 0.26534, 0.26441
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_s3_class(ggeffect(fit, "c12hour"), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c161sex")), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
})


test_that("ggemmeans, lme4::glmer", {
  data(efc_test)
  fit <- lme4::glmer(
    negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
    data = efc_test,
    family = binomial(link = "logit")
  )
  pr <- ggemmeans(fit, "c12hour", verbose = FALSE)
  expect_equal(
    pr$predicted,
    c(0.34217, 0.34406, 0.34596, 0.34787, 0.34978, 0.3517, 0.35362,
      0.35554, 0.35747, 0.35941, 0.36135, 0.36329, 0.36524, 0.36719,
      0.36915, 0.37111, 0.37307, 0.37504, 0.37702, 0.37899, 0.38098,
      0.38296, 0.38495, 0.38694, 0.38894, 0.39094, 0.39295, 0.39496,
      0.39697, 0.39898, 0.401, 0.40302, 0.40505, 0.40708, 0.40911),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_s3_class(ggemmeans(fit, "c12hour", verbose = FALSE), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex"), verbose = FALSE), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex", "c172code"), verbose = FALSE), "data.frame")
})


withr::with_environment(
  new.env(),
  test_that("ggpredict, lme4::glmer.nb", {
    m <- insight::download_model("merMod_5")
    dd <- insight::get_data(m, source = "frame")
    expect_s3_class(ggpredict(m, "f1"), "data.frame")
    expect_s3_class(ggpredict(m, "f1", type = "random"), "data.frame")
    expect_s3_class(ggpredict(m, c("f1", "f2")), "data.frame")
    expect_s3_class(ggpredict(m, c("f1", "f2"), type = "random"), "data.frame")
    expect_message(ggemmeans(m, "f1"))
    expect_s3_class(ggemmeans(m, c("f1", "f2")), "data.frame")
    expect_s3_class(ggpredict(m, c("f1", "f2"), type = "simulate"), "data.frame")
  })
)


test_that("ggpredict, lme4::glmer, cbind", {
  data(cbpp, package = "lme4")
  cbpp$trials <- cbpp$size - cbpp$incidence
  m1 <- lme4::glmer(cbind(incidence, trials) ~ period + (1 | herd), data = cbpp, family = binomial)
  m2 <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), data = cbpp, family = binomial)

  expect_s3_class(ggpredict(m1, "period"), "data.frame")
  expect_s3_class(ggpredict(m2, "period"), "data.frame")
  expect_s3_class(ggpredict(m1, "period", type = "random"), "data.frame")
  expect_s3_class(ggpredict(m2, "period", type = "random"), "data.frame")
  expect_s3_class(ggemmeans(m1, "period"), "data.frame")
  expect_s3_class(ggemmeans(m2, "period"), "data.frame")

  p1 <- ggpredict(m1, "period")
  p2 <- ggemmeans(m1, "period")
  expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)
})
