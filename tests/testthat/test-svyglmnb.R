if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjlabelled") &&
  require("survey") &&
  require("sjstats") &&
  require("sjmisc")
)) {
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
    expect_is(ggpredict(fit, "age"), "data.frame")
    expect_is(ggpredict(fit, c("age", "RIAGENDR")), "data.frame")
  })
}
