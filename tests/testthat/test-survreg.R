skip_on_os(c("mac", "solaris"))
skip_if_not_installed("survival", minimum_version = "3.2.9")
skip_if_not_installed("emmeans")
skip_if_not_installed("effects")

test_that("ggpredict, survreg", {
  m1 <- survival::survreg(
    survival::Surv(futime, fustat) ~ ecog.ps + rx,
    data = survival::ovarian, dist = "exponential"
  )
  pr <- ggpredict(m1, "ecog.ps")
  expect_equal(pr$predicted[1], 1637.551, tolerance = 1e-4)
  expect_null(ggeffect(m1, "ecog.ps"))
  pr <- ggemmeans(m1, "ecog.ps")
  expect_equal(pr$predicted[1], 1637.551, tolerance = 1e-4)
})
