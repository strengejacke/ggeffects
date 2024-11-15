skip_on_os(c("mac", "solaris"))
skip_if_not_installed("plm")

test_that("ggpredict, plm", {
  data(mtcars)
  m <- plm::plm(mpg ~ cyl * disp, index = "am", data = mtcars, model = "within")
  out <- ggpredict(m, c("cyl", "disp"))
  d <- data_grid(m, c("cyl", "disp"))
  out2 <- suppressWarnings(predict(m, newdata = d))
  expect_equal(out[order(out$group), ]$predicted, out2, tolerance = 1e-4, ignore_attr = TRUE)
  expect_true(all(out$conf.low < out$predicted))
  expect_true(all(out$conf.high > out$predicted), tolerance = 1e-4, ignore_attr = TRUE)
})
