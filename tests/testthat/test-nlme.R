if (suppressWarnings(
  requiet("testthat") &&
  requiet("ggeffects") &&
  requiet("sjlabelled") &&
  requiet("nlme") &&
  requiet("lme4") &&
  requiet("sjmisc")
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
      c(237.928, 248.46965, 258.32998, 267.5903, 276.37872, 284.82043, 
        293.01462, 301.03256, 308.92359, 316.72167),
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
      c(199.10003, 209.58643, 219.89485, 230.0269, 239.98591, 249.77677, 
        259.4057, 268.88, 278.20776, 287.39759),
      tolerance = 1e-3
    )
  })
}
