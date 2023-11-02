skip_on_os(c("mac", "solaris"))
skip_if_not_installed("survey")
skip_if_not_installed("sjstats")
skip_if_not_installed("sjlabelled")
skip_if_not_installed("sjmisc")

test_that("ggpredict, svyglm.nb", {
  data(nhanes_sample, package = "sjstats")

  # create survey design
  des <- survey::svydesign(
    id = ~SDMVPSU,
    strat = ~SDMVSTRA,
    weights = ~WTINT2YR,
    nest = TRUE,
    data = nhanes_sample
  )

  # fit negative binomial regression
  fit <- sjstats::svyglm.nb(total ~ RIAGENDR + age + RIDRETH1, des)

  expect_s3_class(ggpredict(fit, "age"), "data.frame")
  expect_s3_class(ggpredict(fit, c("age", "RIAGENDR")), "data.frame")
})
