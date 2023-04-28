skip_on_os(c("mac", "linux"))

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

  test_that("print hypothesis_test many rows", {
    dat <- mtcars
    dat$gear <- factor(dat$gear)
    dat$vs <- factor(dat$vs)
    dat$cyl <- factor(dat$cyl)
    mod <- lm(mpg ~ cyl * vs * gear, data = dat)
    ht <- suppressWarnings(hypothesis_test(
      mod,
      terms = c("cyl", "vs", "gear"),
      test = "(b1 - b13) = (b3 - b15)"
    ))
    out <- capture.output(print(ht))
    expect_identical(
      out,
      c(
        "Hypothesis        | Contrast |         95% CI |     p",
        "-----------------------------------------------------",
        "(b1-b13)=(b3-b15) |    -8.55 | [-19.86, 2.76] | 0.131",
        "",
        "Tested hypothesis: (cyl[4],vs[0],gear[3] - cyl[4],vs[0],gear[5]) =",
        "  (cyl[8],vs[0],gear[3] - cyl[8],vs[0],gear[5])"
      )
    )
    expect_identical(
      attributes(ht)$hypothesis_label,
      "(cyl[4],vs[0],gear[3] - cyl[4],vs[0],gear[5]) = (cyl[8],vs[0],gear[3] - cyl[8],vs[0],gear[5])"
    )
  })
}
