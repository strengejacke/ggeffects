if (suppressWarnings(
  requiet("testthat") &&
  requiet("ggeffects") &&
  requiet("lme4") &&
  requiet("sjlabelled") &&
  requiet("sjmisc")
)) {

  # glm, logistic regression ----
  data(efc, package = "ggeffects")
  efc$neg_c_7d <- dicho(efc$neg_c_7)

  d <<- efc
  fit <- glm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = d, family = binomial(link = "logit"))

  m <- glm(
    cbind(incidence, size - incidence) ~ period,
    family = binomial,
    data = lme4::cbpp
  )

  test_that("validate ggpredict glm against predict", {
    nd <- data_grid(fit, "c12hour [10, 50, 100]")
    pr <- predict(fit, newdata = nd, se.fit = TRUE, type = "link")
    expected <- stats::plogis(pr$fit + stats::qnorm(0.975) * pr$se.fit)
    predicted <- ggpredict(fit, "c12hour [10, 50, 100]")
    expect_equal(predicted$conf.high, expected, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(predicted$predicted, stats::plogis(pr$fit), tolerance = 1e-3, ignore_attr = TRUE)
  })

  test_that("validate ggpredict glm against predict 2", {
    nd <- data_grid(m, "period")
    pr <- predict(m, newdata = nd, se.fit = TRUE, type = "link")
    expected <- stats::plogis(pr$fit + stats::qnorm(0.975) * pr$se.fit)
    predicted <- ggpredict(m, "period")
    expect_equal(predicted$conf.high, expected, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(predicted$predicted, stats::plogis(pr$fit), tolerance = 1e-3, ignore_attr = TRUE)
  })

  test_that("ggpredict, glm", {
    expect_s3_class(ggpredict(fit, "c12hour", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), verbose = FALSE), "data.frame")
  })

  test_that("ggeffect, glm", {
    expect_s3_class(ggeffect(fit, "c12hour", verbose = FALSE), "data.frame")
    expect_s3_class(ggeffect(fit, c("c12hour", "c161sex"), verbose = FALSE), "data.frame")
    expect_s3_class(ggeffect(fit, c("c12hour", "c161sex", "c172code"), verbose = FALSE), "data.frame")
  })

  test_that("ggemmeans, glm", {
    expect_s3_class(ggemmeans(fit, "c12hour", verbose = FALSE), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex"), verbose = FALSE), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex", "c172code"), verbose = FALSE), "data.frame")
  })

  p1 <- ggpredict(m, "period", verbose = FALSE)
  p2 <- ggeffect(m, "period", verbose = FALSE)
  p3 <- ggemmeans(m, "period", verbose = FALSE)

  test_that("ggeffects, glm", {
    expect_equal(p1$predicted[1], 0.2194245, tolerance = 1e-3)
    expect_equal(p2$predicted[1], 0.2194245, tolerance = 1e-3)
    expect_equal(p3$predicted[1], 0.2194245, tolerance = 1e-3)
  })

  test_that("ggpredict, glm, robust", {
    expect_s3_class(ggpredict(fit, "c12hour", vcov.fun = "vcovHC", vcov.type = "HC1", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), vcov.fun = "vcovHC", vcov.type = "HC1", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), vcov.fun = "vcovHC", vcov.type = "HC1", verbose = FALSE), "data.frame")
  })

  test_that("ggeffects, glm, robust", {
    expect_s3_class(ggpredict(m, "period", vcov.fun = "vcovHC", vcov.type = "HC1"), "data.frame")
  })


  data(cbpp)
  cbpp$trials <- cbpp$size - cbpp$incidence
  d2 <<- cbpp

  m1 <- glmer(cbind(incidence, trials) ~ period + (1 | herd), data = d2, family = binomial)
  m2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), data = d2, family = binomial)
  m3 <- glm(cbind(incidence, trials) ~ period, data = d2, family = binomial)
  m4 <- glm(cbind(incidence, size - incidence) ~ period, data = d2, family = binomial)

  test_that("ggeffects, glm-matrix-columns", {
    expect_s3_class(ggpredict(m1, "period"), "data.frame")
    expect_s3_class(ggpredict(m2, "period"), "data.frame")
    expect_s3_class(ggpredict(m3, "period"), "data.frame")
    expect_s3_class(ggpredict(m4, "period"), "data.frame")
    expect_s3_class(ggemmeans(m1, "period"), "data.frame")
    expect_s3_class(ggemmeans(m2, "period"), "data.frame")
    expect_s3_class(ggemmeans(m3, "period"), "data.frame")
    expect_s3_class(ggemmeans(m4, "period"), "data.frame")
  })
}
