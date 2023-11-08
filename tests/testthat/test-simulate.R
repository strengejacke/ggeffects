skip_on_cran()
skip_on_os(c("mac", "solaris"))

test_that("ggpredict, simulate", {
  data(iris)
  set.seed(123)
  iris$x <- as.factor(sample(1:4, nrow(iris), replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.4)))
  m <- lm(Sepal.Width ~ Species + x, data = iris)
  set.seed(123)
  out <- ggpredict(m, "x", type = "simulate")
  expect_snapshot(print(out))
})
