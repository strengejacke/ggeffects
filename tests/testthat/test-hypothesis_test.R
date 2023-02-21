if (suppressWarnings(requiet("testthat") && requiet("ggeffects") && requiet("ggplot2"))) {
  set.seed(123)
  n <- 200
  d <- data.frame(
    outcome = rnorm(n),
    groups = as.factor(sample(c("treatment", "control"), n, TRUE)),
    episode = as.factor(sample(1:3, n, TRUE)),
    ID = as.factor(rep(1:10, n / 10)),
    sex = as.factor(sample(c("female", "male"), n, TRUE, prob = c(.4, .6)))
  )
  model1 <- lm(outcome ~ groups * episode, data = d)
  test_that("hypothesis_test, categorical, pairwise", {
    out <- hypothesis_test(model1, c("groups", "episode"))
    expect_identical(colnames(out), c("groups", "episode", "Contrast", "conf.low", "conf.high", "p.value"))
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
      out$groups,
      c(
        "control-treatment", "control-control", "control-treatment",
        "control-control", "control-treatment", "treatment-control",
        "treatment-treatment", "treatment-control", "treatment-treatment",
        "control-treatment", "control-control", "control-treatment",
        "treatment-control", "treatment-treatment", "control-treatment"
      )
    )
  })
  test_that("hypothesis_test, categorical, pairwise, p_adjust", {
    out1 <- hypothesis_test(model1, c("groups", "episode"))
    out2 <- hypothesis_test(model1, c("groups", "episode"), p_adjust = "tukey")
    expect_equal(
      out1$p.value,
      c(
        0.0724, 0.3491, 0.5288, 0.7501, 0.5722, 0.0083, 0.0247, 0.148,
        0.2018, 0.8161, 0.2232, 0.1353, 0.3665, 0.2523, 0.8212
      ),
      tolerance = 1e-3,
      ignore_attr = FALSE
    )
    expect_equal(
      out2$p.value,
      c(
        0.4679, 0.9372, 0.9888, 0.9996, 0.9932, 0.0878, 0.2164, 0.6984,
        0.7981, 0.9999, 0.8282, 0.6686, 0.9458, 0.8627, 0.9999
      ),
      tolerance = 1e-3,
      ignore_attr = FALSE
    )
  })
  test_that("hypothesis_test, categorical, NULL", {
    out <- hypothesis_test(model1, c("groups", "episode"), test = NULL)
    expect_identical(colnames(out), c("groups", "episode", "Predicted", "conf.low", "conf.high", "p.value"))
    expect_equal(out$Predicted, c(0.028, -0.3903, 0.2316, 0.1763, -0.0428, -0.0931),
      tolerance = 1e-3,
      ignore_attr = FALSE
    )
    expect_equal(
      out$groups,
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
    expect_identical(out$Sepal.Length, c("slope", "slope", "slope"))
  })

  if (suppressWarnings(requiet("lme4"))) {
    model3 <- lmer(outcome ~ groups * episode + sex + (1 | ID), data = d)
    test_that("hypothesis_test, categorical, pairwise", {
      out <- hypothesis_test(model3, c("groups", "episode"))
      expect_identical(colnames(out), c("groups", "episode", "Contrast", "conf.low", "conf.high", "p.value"))
      expect_equal(
        out$Contrast,
        c(
          -0.2051, 0.0666, 0.4199, -0.1528, 0.1187, 0.2718, 0.6251, 0.0524,
          0.3239, 0.3533, -0.2194, 0.0521, -0.5727, -0.3012, 0.2715
        ),
        tolerance = 1e-3,
        ignore_attr = FALSE
      )
      expect_identical(
        out$groups,
        c(
          "control-control", "control-control", "control-treatment",
          "control-treatment", "control-treatment", "control-control",
          "control-treatment", "control-treatment", "control-treatment",
          "control-treatment", "control-treatment", "control-treatment",
          "treatment-treatment", "treatment-treatment", "treatment-treatment"
        )
      )
    })
    test_that("hypothesis_test, categorical, NULL", {
      out <- hypothesis_test(model3, c("groups", "episode"), test = NULL)
      expect_identical(colnames(out), c("groups", "episode", "Predicted", "conf.low", "conf.high", "p.value"))
      expect_equal(out$Predicted, c(0.0559, 0.2611, -0.0107, -0.364, 0.2087, -0.0628),
        tolerance = 1e-3,
        ignore_attr = FALSE
      )
      expect_equal(
        out$groups,
        structure(c(1L, 1L, 1L, 2L, 2L, 2L), levels = c("control", "treatment"), class = "factor")
      )
    })
  }
}
