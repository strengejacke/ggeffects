skip_on_os(c("mac", "solaris"))

test_that("ggpredict, percentiles", {
  data(mtcars)
  m <- lm(mpg ~ hp, data = mtcars)
  out <- predict_response(m, "hp [percentile90]")
  expect_equal(
    out$x,
    c(
      63.6, 66, 82.2, 93.4, 96.5, 106.2, 109.8, 110, 112.9, 123,
      150, 165, 175, 178.5, 180, 200, 220.2, 243.5, 253.5
    ),
    tolerance = 1e-4
  )
  out <- predict_response(m, "hp [percentile50]")
  expect_equal(
    out$x,
    c(96.5, 106.2, 109.8, 110, 112.9, 123, 150, 165, 175, 178.5, 180),
    tolerance = 1e-4
  )
  expect_message(expect_message(expect_warning(expect_warning(
    {
      out <- predict_response(m, "hp [percentileab]")
    }
  ))))
  expect_equal(out$x, c(52, 335), tolerance = 1e-4)
})
