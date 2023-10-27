skip_if_not_installed("lme4")
test_that("ggpredict-backticks-and-CI", {
  data(sleepstudy, package = "lme4")
  d <- sleepstudy
  set.seed(123)
  d$`test var` <- runif(nrow(d), 10, 40)

  m_backtick <- lme4::lmer(Reaction ~ Days + `test var` + (1 | Subject), data = d)
  out <- ggpredict(m_backtick, terms = c("Days", "test var"))
  expect_equal(
    out$conf.low,
    c(
      230.83799, 232.08395, 232.08449, 241.86576, 243.10055, 243.05508,
      252.77507, 253.99324, 253.903, 263.55779, 264.75351, 264.62099,
      274.20771, 275.37512, 275.20405, 284.72119, 285.85477, 285.64991,
      295.09749, 296.19246, 295.95932, 305.33884, 306.3915, 306.13594,
      315.45026, 316.45811, 316.18596, 325.43903, 326.40081, 326.11753
    ),
    tolerance = 1e-3
  )
})
