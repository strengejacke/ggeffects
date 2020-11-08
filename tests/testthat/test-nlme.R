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
}
