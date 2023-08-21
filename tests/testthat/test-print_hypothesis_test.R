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
    out <- hypothesis_test(m, "var_binom", scale = "link")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test simple predictions link scale", {
    out <- hypothesis_test(m, "var_binom", test = NULL, scale = "link")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test simple contrast exp scale", {
    out <- hypothesis_test(m, "var_binom", scale = "exp")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test simple contrast response scale", {
    out <- hypothesis_test(m, "var_binom", scale = "response")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test simple predictions exp scale", {
    out <- hypothesis_test(m, "var_binom", test = NULL, scale = "exp")
    expect_snapshot(print(out))
  })

  test_that("print hypothesis_test contrasts link scale", {
    out <- hypothesis_test(m, c("var_binom", "var_cont"), scale = "link")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test predictions link scale", {
    out <- hypothesis_test(m, c("var_binom", "var_cont"), test = NULL, scale = "link")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test contrasts exp scale", {
    out <- hypothesis_test(m, c("var_binom", "var_cont"), scale = "exp")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test contrasts response scale", {
    out <- hypothesis_test(m, c("var_binom", "var_cont"), scale = "response")
    expect_snapshot(print(out))
  })
  test_that("print hypothesis_test predictions exp scale", {
    out <- hypothesis_test(m, c("var_binom", "var_cont"), test = NULL, scale = "exp")
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
    # check that operators are not replaced if inside brackets
    dat <- mtcars
    dat$gear <- factor(dat$gear)
    dat$vs <- factor(dat$vs)
    dat$cyl <- factor(dat$cyl)
    levels(dat$gear) <- c("-40", "41-64", "65+")
    levels(dat$vs) <- c("a=1", "b=2")
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
        "Tested hypothesis: (cyl[4],vs[a=1],gear[-40] - cyl[4],vs[a=1],gear[65+])",
        "  = (cyl[8],vs[a=1],gear[-40] - cyl[8],vs[a=1],gear[65+])"
      )
    )
    expect_identical(
      attributes(ht)$hypothesis_label,
      "(cyl[4],vs[a=1],gear[-40] - cyl[4],vs[a=1],gear[65+]) = (cyl[8],vs[a=1],gear[-40] - cyl[8],vs[a=1],gear[65+])"
    )
    # check that collapse_levels works
    ht1 <- suppressWarnings(hypothesis_test(
      mod,
      terms = c("cyl", "vs", "gear"),
      by = "gear",
      collapse_levels = TRUE
    ))
    ht2 <- suppressWarnings(hypothesis_test(
      mod,
      terms = c("cyl", "vs", "gear"),
      by = "gear",
      collapse_levels = FALSE
    ))
    expect_equal(ht1$Contrast, ht2$Contrast, tolerance = 1e-3)
    expect_identical(
      ht1$vs,
      c(
        "a=1", "a=1", "a=1-b=2", "a=1-b=2", "a=1-b=2", "a=1", "a=1-b=2",
        "a=1-b=2", "a=1-b=2", "a=1-b=2", "a=1-b=2", "a=1-b=2", "b=2",
        "b=2", "b=2", "a=1", "a=1", "a=1-b=2", "a=1-b=2", "a=1-b=2",
        "a=1", "a=1-b=2", "a=1-b=2", "a=1-b=2", "a=1-b=2", "a=1-b=2",
        "a=1-b=2", "b=2", "b=2", "b=2", "a=1", "a=1", "a=1-b=2", "a=1-b=2",
        "a=1-b=2", "a=1", "a=1-b=2", "a=1-b=2", "a=1-b=2", "a=1-b=2",
        "a=1-b=2", "a=1-b=2", "b=2", "b=2", "b=2"
      )
    )
    expect_identical(
      ht1$cyl,
      c(
        "4-6", "4-8", "4", "4-6", "4-8", "6-8", "6-4", "6", "6-8",
        "8-4", "8-6", "8", "4-6", "4-8", "6-8", "4-6", "4-8", "4", "4-6",
        "4-8", "6-8", "6-4", "6", "6-8", "8-4", "8-6", "8", "4-6", "4-8",
        "6-8", "4-6", "4-8", "4", "4-6", "4-8", "6-8", "6-4", "6", "6-8",
        "8-4", "8-6", "8", "4-6", "4-8", "6-8"
      )
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
    data(efc, package = "ggeffects")
    efc$c172code <- as.factor(efc$c172code)
    efc$c161sex <- as.factor(efc$c161sex)
    levels(efc$c161sex) <- c("male", "female")
    m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex * c172code, data = efc)

    out <- hypothesis_test(m, c("c172code", "c161sex"), collapse_levels = TRUE)
    expect_snapshot(print(out))
  })
}
