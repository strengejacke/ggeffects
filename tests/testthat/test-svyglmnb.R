if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjlabelled") &&
  require("survey") &&
  require("sjstats") &&
  require("sjmisc")
)) {
  context("ggeffects, svyglm.nb")

  # svyglm.nb -----

  data(nhanes_sample)

  # create survey design
  des <- svydesign(
    id = ~SDMVPSU,
    strat = ~SDMVSTRA,
    weights = ~WTINT2YR,
    nest = TRUE,
    data = nhanes_sample
  )

  # fit negative binomial regression
  fit <- svyglm.nb(total ~ RIAGENDR + age + RIDRETH1, des)

  test_that("ggpredict, svyglm.nb", {
    ggpredict(fit, "age")
    ggpredict(fit, c("age", "RIAGENDR"))
  })

  test_that("ggaverage, svyglm.nb", {
    ggaverage(fit, "age")
    ggaverage(fit, c("age", "RIAGENDR"))
  })
}
