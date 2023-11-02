skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("lme4")
skip_if_not_installed("datawizard")
skip_if_not_installed("marginaleffects")

# lmer ----

data(efc, package = "ggeffects")
efc$grp <- datawizard::to_factor(efc$e15relat)
fit <- lme4::lmer(neg_c_7 ~ c12hour + e42dep + c161sex + c172code + (1 | grp), data = efc)

test_that("validate ggpredict lmer against predict", {
  nd <- data_grid(fit, "e42dep")
  pr <- predict(fit, newdata = nd, re.form = NA)
  predicted <- ggpredict(fit, "e42dep")
  expect_equal(predicted$predicted, pr, tolerance = 1e-3, ignore_attr = TRUE)
})

test_that("validate ggpredict lmer against marginaleffects", {
  out1 <- marginaleffects::predictions(
    fit,
    variables = "e42dep",
    newdata = marginaleffects::datagrid(fit)
  )
  out1 <- out1[order(out1$e42dep), ]
  out2 <- ggpredict(
    fit,
    "e42dep",
    condition = c(grp = "child"),
    type = "random",
    interval = "confidence"
  )
  expect_equal(
    out1$estimate,
    out2$predicted,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out1$estimate - stats::qt(0.975, df = 826) * out1$std.error,
    out2$conf.low,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("ggpredict, lmer", {
  expect_s3_class(ggpredict(fit, "c12hour"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex")), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
  expect_s3_class(ggpredict(fit, "c12hour", type = "re"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), type = "re"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "re"), "data.frame")
})

test_that("ggpredict, lmer", {
  pr <- ggpredict(fit, "c12hour")
  expect_equal(pr$std.error[1:5], c(0.2911, 0.2852, 0.2799, 0.2752, 0.2713), tolerance = 1e-3)
  pr <- ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "re")
  expect_equal(pr$std.error[1:5], c(3.5882, 3.58185, 3.58652, 3.58162, 3.57608), tolerance = 1e-3)
})

test_that("ggpredict, lmer-simulate", {
  expect_s3_class(ggpredict(fit, "c12hour", type = "sim"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), type = "sim"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "sim"), "data.frame")
})

test_that("ggeffect, lmer", {
  expect_s3_class(ggeffect(fit, "c12hour"), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c161sex")), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
})


test_that("ggeffect, lmer", {
  data(efc, package = "ggeffects")
  efc$cluster <- as.factor(efc$e15relat)
  efc <- datawizard::standardise(efc, c("c160age", "e42dep"))
  m <- lme4::lmer(
    neg_c_7 ~ c160age_z * e42dep_z + c161sex + (1 | cluster),
    data = efc
  )
  p1 <- ggpredict(m, terms = c("c160age_z", "e42dep_z [-1.17,2.03]"))
  p2 <- ggemmeans(m, terms = c("c160age_z", "e42dep_z [-1.17,2.03]"))
  expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)
})


test_that("ggeffect, lmer", {
  data(efc, package = "ggeffects")
  efc$cluster <- as.factor(efc$e15relat)
  efc <- datawizard::to_factor(efc, c("e42dep", "c172code", "c161sex"))
  efc$c172code[efc$c172code == "intermediate level of education"] <- NA
  m <- lme4::lmer(
    neg_c_7 ~ c172code + e42dep + c161sex + (1 | cluster),
    data = efc
  )
  expect_s3_class(ggpredict(m, terms = "e42dep"), "data.frame")
  expect_s3_class(ggemmeans(m, terms = "e42dep"), "data.frame")
})


test_that("ggeffect, lmer", {
  p1 <- ggpredict(m, terms = "e42dep")
  p2 <- ggemmeans(m, terms = "e42dep")
  p3 <- ggemmeans(m, terms = "e42dep", condition = c(c161sex = "Male", c172code = "low level of education"))
  expect_equal(p1$predicted[1], 8.902934, tolerance = 1e-3)
  expect_equal(p2$predicted[1], 9.742945, tolerance = 1e-3)
  expect_equal(p1$predicted[1], p3$predicted[1], tolerance = 1e-3)
})


test_that("ggeffect, lmer", {
  data(sleepstudy, package = "lme4")
  m <- suppressWarnings(lme4::lmer(
    log(Reaction) ~ Days + I(Days^2) + (1 + Days + exp(Days) | Subject),
    data = sleepstudy
  ))
  p1 <- ggpredict(m, terms = "Days")
  p2 <- ggemmeans(m, terms = "Days")
  p3 <- ggeffect(m, terms = "Days")
  expect_equal(p1$predicted[1], 253.5178, tolerance = 1e-3)
  expect_equal(p2$predicted[1], 253.5178, tolerance = 1e-3)
  expect_equal(p3$predicted[1], 5.535434, tolerance = 1e-3)
  expect_s3_class(
    ggpredict(m, terms = c("Days", "Subject [sample=5]"), type = "re"),
    "data.frame"
  )
})
