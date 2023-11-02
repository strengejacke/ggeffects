skip_on_os(c("mac", "solaris"))
skip_if_not_installed("survey")
skip_if_not_installed("sjstats")
skip_if_not_installed("effects")

test_that("ggpredict and ggeffect, svyglm", {
  data(nhanes_sample, package = "sjstats")
  nhanes_sample$total <- as.numeric(nhanes_sample$total > median(nhanes_sample$total, na.rm = TRUE))

  # create survey design
  des <- survey::svydesign(
    id = ~SDMVPSU,
    strat = ~SDMVSTRA,
    weights = ~WTINT2YR,
    nest = TRUE,
    data = nhanes_sample
  )

  # fit negative binomial regression
  fit <- suppressWarnings(survey::svyglm(
    total ~ RIAGENDR + age + RIDRETH1,
    des,
    family = binomial(link = "logit")
  ))
  expect_s3_class(ggpredict(fit, "age", verbose = FALSE), "data.frame")
  expect_s3_class(ggpredict(fit, c("age", "RIAGENDR"), verbose = FALSE), "data.frame")
  expect_s3_class(ggeffect(fit, "age", verbose = FALSE), "data.frame")
  expect_s3_class(ggeffect(fit, c("age", "RIAGENDR"), verbose = FALSE), "data.frame")
})
