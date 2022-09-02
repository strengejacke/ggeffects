if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjlabelled") &&
  require("nlme") &&
  require("lme4") &&
  require("sjmisc")
)) {
  # lme ----

  data(Orthodont)
  fit <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1 | Subject)

  test_that("ggpredict, lme", {
    expect_s3_class(ggpredict(fit, "age"), "data.frame")
    expect_s3_class(ggpredict(fit, c("age", "Sex")), "data.frame")
    expect_s3_class(ggpredict(fit, "age", type = "re"), "data.frame")
    expect_s3_class(ggpredict(fit, c("age", "Sex"), type = "re"), "data.frame")
  })

  test_that("ggeffect, lme", {
    expect_s3_class(ggeffect(fit, "age"), "data.frame")
    expect_s3_class(ggeffect(fit, c("age", "Sex")), "data.frame")
  })

  m5 <- lmer(distance ~ age * Sex + (age|Subject), data = Orthodont)
  m6 <- lme(distance ~ age * Sex, data = Orthodont, random = ~ age | Subject)

  test_that("ggpredict, lme", {
    expect_s3_class(ggpredict(m5, c("age", "Sex")), "data.frame")
    expect_s3_class(ggpredict(m6, c("age", "Sex")), "data.frame")

    expect_s3_class(ggpredict(m5, c("age", "Sex"), type = "re"), "data.frame")
    expect_s3_class(ggpredict(m6, c("age", "Sex"), type = "re"), "data.frame")
  })

  data(sleepstudy)
  m7 <- lme(Reaction ~ Days, random = ~ Days | Subject, sleepstudy, method = "REML")
  out1 <- ggpredict(m7, "Days", type = "fe")
  out2 <- ggpredict(m7, "Days", type = "re")

  test_that("ggpredict, lme, type=re", {
    expect_equal(
      out1$predicted,
      c(251.4051, 261.87239, 272.33968, 282.80696, 293.27425, 303.74153,
        314.20882, 324.67611, 335.14339, 345.61068),
      tolerance = 1e-3
    )
    expect_equal(
      out1$conf.low,
      c(238.0293, 248.57039, 258.43529, 267.70468, 276.50572, 284.96266,
        293.17393, 301.21028, 309.12068, 316.93882),
      tolerance = 1e-3
    )
    expect_equal(
      out2$predicted,
      c(
        251.4051, 261.87239, 272.33968, 282.80696, 293.27425, 303.74153,
        314.20882, 324.67611, 335.14339, 345.61068
      ),
      tolerance = 1e-3
    )
    expect_equal(
      out2$conf.low,
      c(199.49319, 209.97945, 220.28906, 230.42363, 240.38646, 250.1824,
        259.81764, 269.2994, 278.63573, 287.83516),
      tolerance = 1e-3
    )
  })
}
