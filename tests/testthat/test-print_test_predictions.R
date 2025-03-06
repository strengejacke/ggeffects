skip_on_os(c("mac", "linux", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("ggplot2")
skip_if_not_installed("lme4")

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

test_that("print hypothesis_test simple contrast response scale", {
  out <- hypothesis_test(m, "var_binom", scale = "response")
  expect_snapshot(print(out))
})
test_that("print hypothesis_test contrasts response scale", {
  out <- hypothesis_test(m, c("var_binom", "var_cont"), scale = "response")
  expect_snapshot(print(out))
})

test_that("print hypothesis_test comma and dash levels", {
  data(iris)
  d <- iris
  set.seed(123)
  d$f1 <- as.factor(sample(c("no comma", "with, comma", "and, another, comma"), nrow(d), replace = TRUE))
  d$f2 <- as.factor(sample(letters[1:2], nrow(d), replace = TRUE))

  m <- lme4::lmer(Sepal.Length ~ Sepal.Width + f1 + f2 + (1 | Species), data = d)
  ht <- hypothesis_test(m, c("f1", "f2"))
  expect_identical(nrow(ht), 15L)
  expect_snapshot(print(ht))

  d <- iris
  set.seed(123)
  d$f1 <- as.factor(sample(c("no comma", "with, comma", "and, another, comma"), nrow(d), replace = TRUE))
  d$f2 <- as.factor(sample(c("comma, here", "nothere"), nrow(d), replace = TRUE))

  m <- lme4::lmer(Sepal.Length ~ Sepal.Width + f1 + f2 + (1 | Species), data = d)
  ht <- hypothesis_test(m, c("f1", "f2"))
  expect_identical(nrow(ht), 15L)
  expect_snapshot(print(ht, table_width = Inf))

  d <- iris
  set.seed(123)
  d$f1 <- as.factor(sample(c("no dash", "with, comma", "and-dash"), nrow(d), replace = TRUE))
  d$f2 <- as.factor(sample(c("comma, here", "dash-there"), nrow(d), replace = TRUE))

  m <- lme4::lmer(Sepal.Length ~ Sepal.Width + f1 + f2 + (1 | Species), data = d)
  ht <- hypothesis_test(m, c("f1", "f2"), collapse_levels = TRUE)
  expect_identical(nrow(ht), 15L)
  expect_snapshot(print(ht))
})

test_that("print hypothesis_test collapse levels", {
  data(efc, package = "ggeffects")
  efc$c172code <- as.factor(efc$c172code)
  efc$c161sex <- as.factor(efc$c161sex)
  levels(efc$c161sex) <- c("male", "female")
  m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex * c172code, data = efc)

  out <- hypothesis_test(m, c("c172code", "c161sex"), collapse_levels = TRUE)
  expect_snapshot(print(out))
})


test_that("hypothesis_test, ci-level", {
  data(iris)
  m <- lm(Sepal.Length ~ Species, data = iris)
  out <- hypothesis_test(m, "Species")
  expect_snapshot(print(out))
  out <- hypothesis_test(m, "Species", ci_level = 0.8)
  expect_snapshot(print(out))
})


test_that("glmmTMB, orderedbeta", {
  skip_if_not_installed("datawizard")
  skip_if_not_installed("glmmTMB")
  data(mtcars)
  mtcars$ord <- datawizard::normalize(mtcars$mpg)
  mtcars$gear <- as.factor(mtcars$gear)
  m <- glmmTMB::glmmTMB(
    ord ~ wt + hp + gear + (1 | cyl),
    data = mtcars,
    family = glmmTMB::ordbeta()
  )
  out2 <- predict_response(m, "gear", margin = "average")
  expect_snapshot(print(test_predictions(m, "gear")))
})


skip_if_not_installed("withr")
withr::with_environment(
  new.env(),
  test_that("print hypothesis_test collapse CI", {
    data(efc, package = "ggeffects")
    efc$e42dep <- as.factor(efc$e42dep)
    fit <- lm(barthtot ~ e42dep + c160age, data = efc)
    pr <- ggpredict(fit, "e42dep")
    out <- hypothesis_test(pr)
    expect_snapshot(print(out))
    expect_snapshot(print(out, collapse_ci = TRUE))
  })
)


## TODO: these currently don't work, as they conflict with the depracted
## "transform" argument in modelbased. Once that argument is removed,
## this can be re-enabled.

# test_that("print hypothesis_test simple contrast exp scale", {
#   out <- hypothesis_test(m, "var_binom", scale = "exp")
#   expect_snapshot(print(out))
# })
# test_that("print hypothesis_test simple contrast odds ratio scale", {
#   out <- hypothesis_test(m, "var_binom", scale = "oddsratios")
#   expect_snapshot(print(out))
# })
# test_that("print hypothesis_test simple predictions exp scale", {
#   out <- hypothesis_test(m, "var_binom", test = NULL, scale = "exp")
#   expect_snapshot(print(out))
# })
# test_that("print hypothesis_test simple predictions odds ratio scale", {
#   out <- hypothesis_test(m, "var_binom", test = NULL, scale = "oddsratios")
#   expect_snapshot(print(out))
# })
# test_that("print hypothesis_test contrasts exp scale", {
#   out <- hypothesis_test(m, c("var_binom", "var_cont"), scale = "exp")
#   expect_snapshot(print(out))
# })
# test_that("print hypothesis_test predictions exp scale", {
#   out <- hypothesis_test(m, c("var_binom", "var_cont"), test = NULL, scale = "exp")
#   expect_snapshot(print(out))
# })
