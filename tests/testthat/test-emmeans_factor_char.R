if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("emmeans")
)) {

set.seed(235)
dat <- data.frame(
  y = exp(rnorm(100)),
  z1 = c("a", "b"),
  z2 = factor(c("a", "b")),
  x = rnorm(100)
)

fit_char <- lm(log(y) ~ 1 + z1 * x, dat)
fit_fac <- lm(log(y) ~ 1 + z2 * x, dat)

out1 <- ggemmeans(fit_char, terms = "x")
out2 <- ggemmeans(fit_fac, terms = "x")

test_that("ggpredict, character and factor produce same results", {
  expect_equal(out1$predicted, out2$predicted)
  expect_equal(
    out1$predicted,
    c(
      0.84883, 0.88773, 0.92841, 0.97096, 1.01546, 1.062, 1.11067,
      1.16157, 1.2148
    ),
    tolerance = 1e-3
  )
})

out3 <- ggpredict(fit_char, terms = "x")
out4 <- ggpredict(fit_fac, terms = "x")

test_that("ggpredict, character and factor produce same results", {
  expect_equal(out3$predicted, out4$predicted)
  expect_equal(
    out3$predicted,
    c(
      0.533, 0.61868, 0.71812, 0.83355, 0.96754, 1.12306, 1.30357,
      1.51311, 1.75632
    ),
    tolerance = 1e-3
  )
})
}
