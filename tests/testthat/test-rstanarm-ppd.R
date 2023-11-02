skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("rstanarm")

test_that("ggpredict, rstanarm-ppd", {
  x <- rnorm(30, 0)
  b <- runif(2)
  s <- ifelse(diag(2) == 0, 0.23, 1)
  er <- cbind(rnorm(30, 0, s), rnorm(30, 0, s))
  y <- apply(t(b), 2, `*`, x) + er
  d <- data.frame(y1 = y[,1], y2 = y[,2], x)
  d$group <- sample(c("a", "b", "c"), size = nrow(d), replace = TRUE)

  m1 <- suppressWarnings(rstanarm::stan_mvmer(
    list(
      y1 ~ x + (1 | group),
      y2 ~ x + (1 | group)
    ),
    data = d,
    chains = 2,
    iter = 500,
    refresh = 0
  ))

  m2 <- suppressWarnings(rstanarm::stan_glm(
    y1 ~ x,
    data = d,
    chains = 2,
    iter = 500,
    refresh = 0
  ))

  expect_s3_class(ggpredict(m1, ppd = TRUE), "ggalleffects")
  expect_s3_class(ggpredict(m1, "x", ppd = TRUE), "data.frame")
  expect_s3_class(ggpredict(m2, ppd = TRUE), "ggalleffects")
  expect_s3_class(ggpredict(m2, "x", ppd = TRUE), "data.frame")
  expect_error(ggpredict(m1, ppd = FALSE))
  expect_error(ggpredict(m1, "x", ppd = FALSE))
  expect_s3_class(ggpredict(m2, ppd = FALSE), "ggalleffects")
  expect_s3_class(ggpredict(m2, "x", ppd = FALSE), "data.frame")
})
