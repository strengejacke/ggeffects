skip_on_os(c("mac", "solaris"))
skip_if_not_installed("rms")

test_that("ggpredict, orm", {
  # example data
  set.seed(123)
  d <- data.frame(y = as.numeric(rnorm(100) > 0), x = rnorm(100))
  m <- rms::orm(y ~ x, data = d)

  pr <- ggpredict(m, "x [-2:2 by=1]")
  expect_equal(pr$predicted, c(0.55423, 0.5362, 0.51807, 0.49989, 0.48171), tolerance = 1e-2)

  pr <- ggemmeans(m, "x [-2:2 by=1]")
  expect_equal(pr$predicted, c(0.55423, 0.5362, 0.51807, 0.49989, 0.48171), tolerance = 1e-2)
})
