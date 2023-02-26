if (suppressWarnings(
  requiet("testthat") &&
  requiet("ggeffects") &&
  requiet("sjlabelled") &&
  requiet("survey") &&
  requiet("sjstats") &&
  requiet("sjmisc")
)) {
  # svyglm -----

  data(nhanes_sample)

  nhanes_sample$total <- dicho(nhanes_sample$total)

  # create survey design
  des <- svydesign(
    id = ~SDMVPSU,
    strat = ~SDMVSTRA,
    weights = ~WTINT2YR,
    nest = TRUE,
    data = nhanes_sample
  )

  # fit negative binomial regression
  fit <- suppressWarnings(svyglm(total ~ RIAGENDR + age + RIDRETH1, des, family = binomial(link = "logit")))

  test_that("ggpredict, svyglm", {
    expect_s3_class(ggpredict(fit, "age", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(fit, c("age", "RIAGENDR"), verbose = FALSE), "data.frame")
  })

  test_that("ggeffect, svyglm", {
    expect_s3_class(ggeffect(fit, "age", verbose = FALSE), "data.frame")
    expect_s3_class(ggeffect(fit, c("age", "RIAGENDR"), verbose = FALSE), "data.frame")
  })
}
