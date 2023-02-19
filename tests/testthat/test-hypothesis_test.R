if (suppressWarnings(
  requiet("testthat") &&
  requiet("ggeffects") &&
  requiet("ggplot2")
)) {
  set.seed(123)
  n <- 200
  d <- data.frame(
    outcome = rnorm(n),
    group = as.factor(sample(c("treatment", "control"), n, TRUE)),
    episode = as.factor(sample(1:3, n, TRUE)),
    sex = as.factor(sample(c("female", "male"), n, TRUE, prob = c(.4, .6)))
  )
  model1 <- lm(outcome ~ group * episode, data = d)  
  test_that("hypothesis_test, categorical, pairwise", {
    out <- hypothesis_test(model1, c("group", "episode"))
    expect_identical(colnames(out), c("group", "episode", "Contrast", "conf.low", "conf.high", "p.value"))
    expect_equal(
      out$Contrast,
      c(
        0.4183, -0.2036, -0.1482, 0.0709, 0.1211, -0.6219, -0.5666,
        -0.3475, -0.2972, 0.0554, 0.2745, 0.3247, 0.2191, 0.2694, 0.0503
      ),
      tolerance = 1e-3,
      ignore_attr = FALSE
    )
    expect_identical(
      out$group,
      c(
        "control-treatment", "control-control", "control-treatment",
        "control-control", "control-treatment", "treatment-control",
        "treatment-treatment", "treatment-control", "treatment-treatment",
        "control-treatment", "control-control", "control-treatment",
        "treatment-control", "treatment-treatment", "control-treatment"
      )
    )
  })
  test_that("hypothesis_test, categorical, NULL", {
    out <- hypothesis_test(model1, c("group", "episode"), test = NULL)
    expect_identical(colnames(out), c("group", "episode", "Predicted", "conf.low", "conf.high", "p.value"))
    expect_equal(out$Predicted, c(0.028, -0.3903, 0.2316, 0.1763, -0.0428, -0.0931),
      tolerance = 1e-3,
      ignore_attr = FALSE
    )
    expect_equal(
      out$group,
      structure(c(1L, 2L, 1L, 2L, 1L, 2L), levels = c("control", "treatment"), class = "factor")
    )
  })

  data(iris)
  model2 <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
  test_that("ggpredict, lm", {
    out <- hypothesis_test(model2, c("Sepal.Length", "Species"))
    expect_identical(colnames(out), c(
      "Sepal.Length", "Species", "Contrast", "conf.low", "conf.high",
      "p.value"
    ))
    expect_equal(out$Contrast, c(0.4788, 0.5666, 0.0878),
      tolerance = 1e-3,
      ignore_attr = FALSE
    )
    expect_identical(out$group, c("slope", "slope", "slope"))
  })
}
