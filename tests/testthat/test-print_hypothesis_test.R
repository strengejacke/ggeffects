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

  if (suppressWarnings(requiet("lme4"))) {
    test_that("print hypothesis_test comma and dash levels", {
      data(iris)
      d <- iris
      set.seed(123)
      d$f1 <- as.factor(sample(c("no comma", "with, comma", "and, another, comma"), nrow(d), replace = TRUE))
      d$f2 <- as.factor(sample(letters[1:2], nrow(d), replace = TRUE))

      m <- lmer(Sepal.Length ~ Sepal.Width + f1 + f2 + (1 | Species), data = d)
      ht <- hypothesis_test(m, c("f1", "f2"))
      expect_identical(nrow(ht), 15L)
      expect_snapshot(print(ht))

      d <- iris
      set.seed(123)
      d$f1 <- as.factor(sample(c("no comma", "with, comma", "and, another, comma"), nrow(d), replace = TRUE))
      d$f2 <- as.factor(sample(c("comma, here", "nothere"), nrow(d), replace = TRUE))

      m <- lmer(Sepal.Length ~ Sepal.Width + f1 + f2 + (1 | Species), data = d)
      ht <- hypothesis_test(m, c("f1", "f2"))
      expect_identical(nrow(ht), 15L)
      expect_snapshot(print(ht))

      d <- iris
      set.seed(1234)
      d$f1 <- as.factor(sample(c("no comma", "with, comma", "and, another, comma"), nrow(d), replace = TRUE))
      set.seed(123)
      d$f2 <- as.factor(sample(letters[1:2], nrow(d), replace = TRUE))

      m <- lmer(Sepal.Length ~ Sepal.Width + f1 + f2 + (1 | Species), data = d)
      ht <- hypothesis_test(m, c("Sepal.Width", "f1", "f2"))
      expect_snapshot(print(ht))

      d <- iris
      set.seed(123)
      d$f1 <- as.factor(sample(c("no dash", "with, comma", "and-dash"), nrow(d), replace = TRUE))
      d$f2 <- as.factor(sample(c("comma, here", "dash-there"), nrow(d), replace = TRUE))

      m <- lmer(Sepal.Length ~ Sepal.Width + f1 + f2 + (1 | Species), data = d)
      ht <- hypothesis_test(m, c("f1", "f2"), collapse_levels = TRUE)
      expect_identical(nrow(ht), 15L)
      expect_snapshot(print(ht))
    })
  }

  test_that("print hypothesis_test collapse levels", {
    data(efc)
    efc$c172code <- as.factor(efc$c172code)
    efc$c161sex <- as.factor(efc$c161sex)
    levels(efc$c161sex) <- c("male", "female")
    m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex * c172code, data = efc)

    out <- hypothesis_test(m, c("c172code", "c161sex"), collapse_levels = TRUE)
    expect_snapshot(print(out))
  })
}
