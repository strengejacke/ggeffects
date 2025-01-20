skip_on_os(c("mac", "solaris"))
skip_if_not_installed("logitr")
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("ggpredict, logistr", {
  data(yogurt, package = "logitr")
  m <- logitr::logitr(
    data    = yogurt,
    outcome = "choice",
    obsID   = "obsID",
    pars    = c("price", "feat", "brand")
  )
  pr <- predict_response(m, "brand")
  expect_equal(pr$predicted, c(0.27506, 0.0067, 0.14487, 0.57337), tolerance = 1e-2)
  expect_equal(pr$conf.low, c(0.25002, 0.00487, 0.12856, 0.53566), tolerance = 1e-2)

  expect_error(ggemmeans(m, "brand"))
  expect_error(ggaverage(m, "brand"))
})
