skip_if_not_installed("survival", minimum_version = "3.2.9")
skip_if_not_installed("emmeans")
skip_if_not_installed("effects")

m1 <- survival::survreg(
  survival::Surv(futime, fustat) ~ ecog.ps + rx,
  data = survival::ovarian, dist = "exponential"
)

test_that("ggpredict, survreg", {
  pr <- ggpredict(m1, "ecog.ps")
  expect_equal(pr$predicted[1], 1637.551, tolerance = 1e-4)
})

test_that("ggeffect, survreg", {
  expect_null(ggeffect(m1, "ecog.ps"))
})

test_that("ggemmeans, survreg", {
  pr <- ggemmeans(m1, "ecog.ps")
  expect_equal(pr$predicted[1], 1637.551, tolerance = 1e-4)
})
