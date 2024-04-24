skip_on_os(c("mac", "linux", "solaris"))
skip_if_not_installed("datawizard")

test_that("print subsetting 1", {
  m <- lm(mpg ~ gear * factor(cyl), mtcars)
  gge <- ggpredict(m, c("gear", "cyl"))
  expect_snapshot(print(gge[c(1:2, 4:6)]))
})
test_that("print subsetting 1", {
  m <- lm(mpg ~ factor(cyl), mtcars)
  gge <- ggpredict(m, "cyl")
  expect_snapshot(print(gge[c(1:2, 4:6)]))
})
