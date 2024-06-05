skip_on_os(c("mac", "solaris"))
skip_if_not_installed("emmeans", minimum_version = "1.5.0")
skip_if_not_installed("survival", minimum_version = "3.2.9")

data("lung2")
m1 <- survival::coxph(survival::Surv(time, status) ~ sex + age + ph.ecog, data = lung2)

test_that("ggpredict", {
  p <- ggpredict(m1, "sex")
  expect_equal(p$predicted[1], 1, tolerance = 1e-2)
  ggpredict(m1, c("sex", "age"))
})

test_that("ggemmeans", {
  p <- ggemmeans(m1, "sex")
  expect_equal(p$predicted[1], 0.7521603, tolerance = 1e-2)
  ggemmeans(m1, c("sex", "age"))
})

test_that("ggpredict", {
  p <- ggpredict(m1, "sex", type = "survival")
  expect_equal(p$predicted[1], 0.9966796, tolerance = 1e-2)
  p <- ggpredict(m1, "sex", type = "cumulative_hazard")
  expect_equal(p$predicted[1], 0.003325958, tolerance = 1e-2)
})

test_that("ggpredict", {
  # test start of time
  df1 <- data.frame(
    production = c(15, 12, 10, 9, 6, 8, 9, 5, 3, 3, 2, 1, 0, 0, 0, 0),
    Treatment_Num = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4),
    Genotype = c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2),
    Source_Salinity = c(
      "Fresh", "Fresh", "Brackish", "Brackish", "Fresh", "Fresh",
      "Brackish", "Brackish", "Fresh", "Fresh", "Brackish", "Brackish",
      "Fresh", "Fresh", "Brackish", "Brackish"
    ),
    Days_to_death = c(
      500, 500, 500, 500, 400, 350, 300, 500, 200, 202, 260,
      280, 150, 150, 160, 140
    ),
    censored = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  m <- survival::coxph(
    survival::Surv(time = Days_to_death, event = censored) ~ Treatment_Num + Source_Salinity,
    data = df1
  )
  p <- ggpredict(m, c("Treatment_Num [all]", "Source_Salinity [all]"), type = "survival")
  expect_equal(head(p$x, 10), c(1, 1, 1, 1, 1, 1, 1, 1, 140, 140), tolerance = 1e-2)
  expect_equal(head(p$predicted, 10), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), tolerance = 1e-2)

  p <- ggpredict(m, c("Treatment_Num [all]", "Source_Salinity [all]"), type = "cumulative_hazard")
  expect_equal(head(p$x, 10), c(1, 1, 1, 1, 1, 1, 1, 1, 140, 140), tolerance = 1e-2)
  expect_equal(head(p$predicted, 10), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-2)
})
