skip_on_os(c("mac", "solaris"))
skip_if_not_installed("emmeans")

test_that("ggpredict-log-/srqtresponse", {
  reprex <- data.frame(
    time = as.factor(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)),
    group2 = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    id = c(10, 10, 10, 10, 10, 15, 15, 15, 15, 15, 20, 20, 20, 20, 20, 25, 25, 25, 25, 25, 30, 30, 30, 30, 30, 35, 35, 35, 35, 35), # nolint
    factor1 = as.factor(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
    factor2 = as.factor(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
    score = c(7, 7, 7, 4, 2, 2, 2, 2, 1, 1, 5, 5, 7, 8, 6, 0, 2, 3, 3, 3, 6, 8, 8, 8, 8, 2, 4, 4, 3, 2)
  )
  m1 <-  lm(sqrt(score + 5) ~ as.numeric(time) * group2, data = reprex)
  m2 <-  lm(log(score + 1) ~ as.numeric(time) * group2, data = reprex)

  # log-response
  p1 <- suppressMessages(ggpredict(m1, c("time", "group2")))
  p2 <- as.data.frame(emmeans::emmeans(m1, c("time", "group2"), at = list(time = 1:5), type = "response"))
  p3 <- suppressMessages(ggaverage(m1, c("time", "group2")))
  expect_equal(p1$predicted[1], p2$response[1], tolerance = 1e-3)
  expect_equal(p1$predicted[1], 6.677575, tolerance = 1e-3)
  expect_equal(p1$predicted[1], p3$predicted[1], tolerance = 1e-3)
  expect_equal(p1$conf.low, p3$conf.low, tolerance = 1e-3)

  # sqrt-response
  p1 <- suppressMessages(ggpredict(m2, c("time", "group2")))
  p2 <- as.data.frame(emmeans::emmeans(m2, c("time", "group2"), at = list(time = 1:5), type = "response"))
  p3 <- suppressMessages(ggaverage(m2, c("time", "group2")))
  expect_equal(p1$predicted[1], p2$response[1], tolerance = 1e-3)
  expect_equal(p1$predicted[1], 6.743365, tolerance = 1e-3)
  expect_equal(p1$predicted[1], p3$predicted[1], tolerance = 1e-3)
  expect_equal(p1$conf.low, p3$conf.low, tolerance = 1e-3)
})

test_that("ggpredict-sqrt-response", {
  data(mtcars)
  m1 <- lm(log(mpg) ~ factor(cyl), mtcars)
  m2 <- lm(log10(mpg) ~ factor(cyl), mtcars)
  expect_message({
    out1 <- ggemmeans(m1, "cyl")
  })
  expect_message({
    out2 <- ggemmeans(m2, "cyl")
  })
  expect_equal(out1$predicted[1], out2$predicted[1], tolerance = 1e-3)
})
