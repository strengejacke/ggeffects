skip_on_cran()
skip_on_os(c("mac", "solaris"))

test_that("ggpredict, print digits", {
  data(iris)
  m <- lm(Sepal.Length ~ Petal.Length, data = iris)
  expect_snapshot(print(ggpredict(m, "Petal.Length"), digits = 5))
  expect_snapshot(print(ggpredict(m, "Petal.Length"), digits = 4, n = 3))
})

test_that("ggpredict, print digits and labels", {
  skip_if_not_installed("sjlabelled")
  data(efc, package = "ggeffects")
  fit <- lm(barthtot ~ c12hour + e42dep, data = efc)

  expect_snapshot(print(ggpredict(fit, "e42dep"), value_labels = FALSE))
  expect_snapshot(print(ggpredict(fit, "e42dep"), value_labels = TRUE))
  expect_snapshot(print(ggpredict(fit, "e42dep"), value_labels = TRUE, digits = 4))
})
