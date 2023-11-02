skip_on_os(c("mac", "solaris"))
skip_if_not_installed("gee")
skip_if_not_installed("emmeans")

data(warpbreaks)
m1 <- suppressMessages(gee::gee(breaks ~ tension, id = wool, data = warpbreaks, silent = TRUE))

test_that("ggpredict", {
  p <- ggpredict(m1, "tension")
  expect_equal(p$predicted[1], 36.38889, tolerance = 1e-3)
})

test_that("ggemmeans", {
  p <- ggemmeans(m1, "tension")
  expect_equal(p$predicted[1], 36.38889, tolerance = 1e-3)
})

test_that("ggemmeans, binomial", {
  skip_if_not_installed("insight", "0.19.2")
  set.seed(123)
  n <- 600
  dat <- data.frame(
    depression = rbinom(n, 1, prob = 0.15),
    drug = rbinom(n, 1, prob = 0.5),
    time = rep(1:3, n / 3),
    id = rep(1:200, each = 3)
  )
  junk <- capture.output({
    dep_gee <- suppressMessages(gee::gee(depression ~ drug * time,
      data = dat,
      id = id,
      family = binomial,
      corstr = "independence"
    ))
  })
  out <- ggemmeans(dep_gee, c("drug", "time"))
  expect_equal(out$predicted, c(0.165, 0.1421, 0.1219, 0.1511, 0.1622, 0.1738), tolerance = 1e-2)
  out <- ggpredict(dep_gee, c("drug", "time"))
  expect_equal(out$predicted, c(0.165, 0.1421, 0.1219, 0.1511, 0.1622, 0.1738), tolerance = 1e-2)
})
