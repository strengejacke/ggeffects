skip_on_cran()
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
  p1 <- ggpredict(m1, c("time", "group2"), verbose = FALSE)
  p2 <- as.data.frame(emmeans::emmeans(m1, c("time", "group2"), at = list(time = 1:5), type = "response"))
  p3 <- ggaverage(m1, c("time", "group2"), verbose = FALSE)
  p4 <- predict_response(m1, c("time", "group2"), margin = "average", verbose = FALSE)
  expect_equal(p1$predicted[1], p2$response[1], tolerance = 1e-3)
  expect_equal(p1$predicted[1], 6.677575, tolerance = 1e-3)
  expect_equal(p1$predicted[1], p3$predicted[1], tolerance = 1e-3)
  expect_equal(p1$predicted[1], p4$predicted[1], tolerance = 1e-3)
  expect_equal(p1$conf.low, p3$conf.low, tolerance = 1e-3)
  expect_equal(p1$conf.low, p4$conf.low, tolerance = 1e-3)

  # sqrt-response
  p1 <- ggpredict(m2, c("time", "group2"), verbose = FALSE)
  p2 <- as.data.frame(emmeans::emmeans(m2, c("time", "group2"), at = list(time = 1:5), type = "response"))
  p3 <- ggaverage(m2, c("time", "group2"), verbose = FALSE)
  p4 <- predict_response(m2, c("time", "group2"), margin = "average", verbose = FALSE)
  expect_equal(p1$predicted[1], p2$response[1], tolerance = 1e-3)
  expect_equal(p1$predicted[1], 6.743365, tolerance = 1e-3)
  expect_equal(p1$predicted[1], p3$predicted[1], tolerance = 1e-3)
  expect_equal(p1$predicted[1], p4$predicted[1], tolerance = 1e-3)
  expect_equal(p1$conf.low, p3$conf.low, tolerance = 1e-3)
  expect_equal(p1$conf.low, p4$conf.low, tolerance = 1e-3)
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

skip_if_not_installed("withr")
skip_if_not_installed("vdiffr")

withr::with_environment(
  new.env(),
  test_that("ggpredict-backtransformed raw data", {
    skip_if_not_installed("lme4")
    skip_if_not_installed("ggplot2")
    data(sleepstudy, package = "lme4")
    model <- lme4::lmer(log(Reaction) ~ Days + (1 | Subject), data = sleepstudy)

    expect_message(
      {
        pr <- ggpredict(model, "Days", back_transform = FALSE)
      },
      regex = "Model has log transformed response"
    )
    d <- attr(pr, "rawdata")
    expect_equal(
      d$response[1:10],
      c(
        5.5197, 5.55569, 5.52466, 5.77281, 5.87732, 6.02753, 5.94595,
        5.67039, 6.06515, 6.14494
      ),
      tolerance = 1e-4
    )
    set.seed(123)
    vdiffr::expect_doppelganger(
      "show_data, back-transformed-FALSE",
      suppressWarnings(plot(pr, show_data = TRUE, verbose = FALSE))
    )

    pr <- ggpredict(model, "Days", back_transform = TRUE, verbose = FALSE)
    d <- attr(pr, "rawdata")
    expect_equal(
      d$response[1:10],
      c(
        249.56, 258.7047, 250.8006, 321.4398, 356.8519, 414.6901, 382.2038,
        290.1486, 430.5853, 466.3535
      ),
      tolerance = 1e-4
    )
    set.seed(123)
    vdiffr::expect_doppelganger(
      "show_data, back-transformed-TRUE",
      suppressWarnings(plot(pr, show_data = TRUE, verbose = FALSE))
    )
  })
)
