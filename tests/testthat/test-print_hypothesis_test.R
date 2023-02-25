if (suppressWarnings(requiet("testthat") && requiet("ggeffects") && requiet("marginaleffects") && requiet("ggplot2"))) {
  set.seed(123)
  dat <- data.frame(
    outcome = rbinom(n = 100, size = 1, prob = 0.35),
    var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
    var_cont = rnorm(n = 100, mean = 10, sd = 7),
    groups = sample(letters[1:4], size = 100, replace = TRUE)
  )

  m <- glm(outcome ~ var_binom * var_cont + groups,
    data = dat, family = binomial()
  )

  test_that("print hypothesis_test simple contrast link scale", {
    out <- hypothesis_test(m, "var_binom")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test simple predictions link scale", {
    out <- hypothesis_test(m, "var_binom", test = NULL)
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test simple contrast response scale", {
    out <- hypothesis_test(m, "var_binom", transform_post = "exp")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test simple predictions response scale", {
    out <- hypothesis_test(m, "var_binom", test = NULL, transform_post = "exp")
    expect_snapshot(print(out))
  })

  test_that("print hypothesis_test contrasts link scale", {
    out <- hypothesis_test(m, c("var_binom", "var_cont"))
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test predictions link scale", {
    out <- hypothesis_test(m, c("var_binom", "var_cont"), test = NULL)
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test contrasts response scale", {
    out <- hypothesis_test(m, c("var_binom", "var_cont"), transform_post = "exp")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test predictions response scale", {
    out <- hypothesis_test(m, c("var_binom", "var_cont"), test = NULL, transform_post = "exp")
    expect_snapshot(print(out))
  })
}
