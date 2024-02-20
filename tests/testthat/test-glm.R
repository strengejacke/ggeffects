skip_on_os(c("mac", "solaris"))
skip_if_not_installed("lme4")
skip_if_not_installed("effects")
skip_if_not_installed("emmeans")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("validate ggpredict glm against predict", {
    data(efc, package = "ggeffects")
    efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
    d <- efc
    fit <- glm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = d, family = binomial(link = "logit"))
    m <- glm(
      cbind(incidence, size - incidence) ~ period,
      family = binomial,
      data = lme4::cbpp
    )
    nd <- data_grid(fit, "c12hour [10, 50, 100]")
    pr <- predict(fit, newdata = nd, se.fit = TRUE, type = "link")
    expected <- stats::plogis(pr$fit + stats::qnorm(0.975) * pr$se.fit)
    predicted <- ggpredict(fit, "c12hour [10, 50, 100]")
    expect_equal(predicted$conf.high, expected, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(predicted$predicted, stats::plogis(pr$fit), tolerance = 1e-3, ignore_attr = TRUE)
  })
)


withr::with_environment(
  new.env(),
  test_that("validate ggpredict glm against predict 2", {
    data(efc, package = "ggeffects")
    efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
    d <- efc
    fit <- glm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = d, family = binomial(link = "logit"))
    m <- glm(
      cbind(incidence, size - incidence) ~ period,
      family = binomial,
      data = lme4::cbpp
    )
    nd <- data_grid(m, "period")
    pr <- predict(m, newdata = nd, se.fit = TRUE, type = "link")
    expected <- stats::plogis(pr$fit + stats::qnorm(0.975) * pr$se.fit)
    predicted <- ggpredict(m, "period")
    expect_equal(predicted$conf.high, expected, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(predicted$predicted, stats::plogis(pr$fit), tolerance = 1e-3, ignore_attr = TRUE)
  })
)


withr::with_environment(
  new.env(),
  test_that("ggpredict, glm", {
    data(efc, package = "ggeffects")
    efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
    d <- efc
    fit <- glm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = d, family = binomial(link = "logit"))
    m <- glm(
      cbind(incidence, size - incidence) ~ period,
      family = binomial,
      data = lme4::cbpp
    )
    expect_s3_class(ggpredict(fit, "c12hour", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), verbose = FALSE), "data.frame")
    expect_s3_class(ggeffect(fit, "c12hour", verbose = FALSE), "data.frame")
    expect_s3_class(ggeffect(fit, c("c12hour", "c161sex"), verbose = FALSE), "data.frame")
    expect_s3_class(ggeffect(fit, c("c12hour", "c161sex", "c172code"), verbose = FALSE), "data.frame")
    expect_s3_class(ggemmeans(fit, "c12hour", verbose = FALSE), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex"), verbose = FALSE), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex", "c172code"), verbose = FALSE), "data.frame")

    p1 <- ggpredict(m, "period", verbose = FALSE)
    p2 <- ggeffect(m, "period", verbose = FALSE)
    p3 <- ggemmeans(m, "period", verbose = FALSE)
    expect_equal(p1$predicted[1], 0.2194245, tolerance = 1e-3)
    expect_equal(p2$predicted[1], 0.2194245, tolerance = 1e-3)
    expect_equal(p3$predicted[1], 0.2194245, tolerance = 1e-3)
  })
)


withr::with_environment(
  new.env(),
  test_that("ggpredict, glm, robust", {
    data(efc, package = "ggeffects")
    efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
    d <- efc
    fit <- glm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = d, family = binomial(link = "logit"))
    m <- glm(
      cbind(incidence, size - incidence) ~ period,
      family = binomial,
      data = lme4::cbpp
    )
    expect_s3_class(
      ggpredict(fit, "c12hour", vcov_fun = "vcovHC", vcov_type = "HC1", verbose = FALSE),
      "data.frame"
    )
    expect_s3_class(
      ggpredict(fit, c("c12hour", "c161sex"), vcov_fun = "vcovHC", vcov_type = "HC1", verbose = FALSE),
      "data.frame"
    )
    expect_s3_class(
      ggpredict(fit, c("c12hour", "c161sex", "c172code"), vcov_fun = "vcovHC", vcov_type = "HC1", verbose = FALSE),
      "data.frame"
    )
    expect_s3_class(ggpredict(m, "period", vcov_fun = "vcovHC", vcov_type = "HC1"), "data.frame")
  })
)


withr::with_environment(
  new.env(),
  test_that("ggeffects, glm-matrix-columns", {
    data(cbpp, package = "lme4")
    cbpp$trials <- cbpp$size - cbpp$incidence
    d2 <- cbpp

    m1 <- lme4::glmer(cbind(incidence, trials) ~ period + (1 | herd), data = d2, family = binomial)
    m2 <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), data = d2, family = binomial)
    m3 <- glm(cbind(incidence, trials) ~ period, data = d2, family = binomial)
    m4 <- glm(cbind(incidence, size - incidence) ~ period, data = d2, family = binomial)

    expect_s3_class(ggpredict(m1, "period"), "data.frame")
    expect_s3_class(ggpredict(m2, "period"), "data.frame")
    expect_s3_class(ggpredict(m3, "period"), "data.frame")
    expect_s3_class(ggpredict(m4, "period"), "data.frame")
    expect_s3_class(ggemmeans(m1, "period"), "data.frame")
    expect_s3_class(ggemmeans(m2, "period"), "data.frame")
    expect_s3_class(ggemmeans(m3, "period"), "data.frame")
    expect_s3_class(ggemmeans(m4, "period"), "data.frame")
  })
)


test_that("ggaverage, invlink", {
  skip_if_not_installed("marginaleffects")
  dat2 <- data.frame(
    sex = c("m", "w", "w", "m", "w", "w", "m", "w", "w", "m", "m", "w", "w"),
    smoking = c(0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1),
    age = c(10, 45, 50, 40, 45, 12, 14, 55, 60, 10, 14, 50, 40),
    stringsAsFactors = FALSE
  )
  m3 <- glm(smoking ~ sex, data = dat2, family = binomial("logit"))
  out1 <- ggaverage(m3, "sex")
  out2 <- marginaleffects::avg_predictions(m3, variables = "sex", type = "invlink(link)")
  expect_equal(out1$predicted, out2$estimate, tolerance = 1e-4)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4)
  expect_equal(out1$conf.high, out2$conf.high, tolerance = 1e-4)
})
