skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")
skip_if_not_installed("datawizard")

test_that("test_predictions, engine ggeffects, linear models", {
  data(efc, package = "ggeffects")
  efc$c172code <- as.factor(efc$c172code)
  efc$c161sex <- as.factor(efc$c161sex)
  efc$e15relat <- as.factor(efc$e15relat)
  efc$e42dep <- as.factor(efc$e42dep)
  levels(efc$c161sex) <- c("male", "female")

  # single focal term
  m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

  # categorical
  pr <- ggemmeans(m, "c172code")
  out1 <- test_predictions(pr, engine = "ggeffects")
  out2 <- test_predictions(m, "c172code", engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(attributes(out1)$standard_error, attributes(out2)$standard_error, tolerance = 1e-1)
  expect_identical(out1$c172code, out2$c172code)

  # multiple focal terms, interaction
  m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex * c172code, data = efc)

  # categorical
  pr <- ggemmeans(m, c("c172code", "c161sex"))
  out1 <- test_predictions(pr, engine = "ggeffects")
  out2 <- test_predictions(m, c("c172code", "c161sex"), engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-2)
  expect_identical(out1$c172code, out2$c172code)
  expect_equal(attributes(out1)$standard_error[1:2], attributes(out2)$standard_error[1:2], tolerance = 1e-1)

  # difference-in-difference
  pr <- ggemmeans(m, c("c172code", "c161sex"))
  out1 <- test_predictions(pr, engine = "ggeffects", test = "interaction")
  out2 <- test_predictions(m, c("c172code", "c161sex"), engine = "emmeans", test = "interaction")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-2)
  expect_identical(out1$c172code, out2$c172code)
  expect_identical(out1$c172code, c("1-2", "1-3", "2-3"))
  expect_identical(out1$c161sex, c("male and female", "male and female", "male and female"))
  expect_identical(attributes(out1)$test, "interaction")
  expect_equal(attributes(out1)$standard_error, attributes(out2)$standard_error, tolerance = 1e-1)

  # interaction categorical * numeric
  m <- lm(barthtot ~ c12hour + neg_c_7 * c161sex, data = efc)
  pr <- ggemmeans(m, c("c161sex", "neg_c_7"))
  out1 <- test_predictions(pr, engine = "ggeffects")
  out2 <- test_predictions(m, c("c161sex", "neg_c_7"), engine = "emmeans")
  expect_equal(out1$Contrast[1:2], out2$Contrast[1:2], tolerance = 1e-3)
  expect_equal(
    out1$conf.low,
    c(
      6.1644, 5.86451, 14.73015, 2.51274, 4.69154, 11.28046, -3.62851,
      5.29921, -4.14491, 3.18193, 12.19047, -6.30176, 2.00504, 2.28462,
      9.13784
    ),
    tolerance = 1e-3
  )
  expect_equal(
    attributes(out1)$standard_error,
    c(
      1.62161, 1.65229, 1.89761, 2.71915, 2.16782, 2.89225, 1.93083,
      2.14453, 2.87483, 2.43774, 2.61026, 2.62943, 3.05993, 2.81282,
      3.40259
    ),
    tolerance = 1e-3
  )

  # difference-in-difference
  pr <- ggemmeans(m, c("c161sex", "neg_c_7"))
  out1 <- test_predictions(pr, engine = "ggeffects", test = "interaction")
  out2 <- test_predictions(m, c("c161sex", "neg_c_7"), engine = "emmeans", test = "interaction")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$conf.low, c(-8.13332, -7.70486, -10.28511), tolerance = 1e-3)
  ## FIXME: SEs are larger than for emmeans
  expect_equal(
    attributes(out1)$standard_error,
    c(3.46306, 3.26221, 3.89597),
    tolerance = 1e-3
  )

  # errors
  expect_error(
    test_predictions(pr, engine = "ggeffects", equivalence = c(-1, 1)),
    regex = "Equivalence testing is currently"
  )
  expect_error(
    test_predictions(pr, engine = "ggeffects", scale = "link"),
    regex = "Only `scale = \"response\"`"
  )
})


test_that("test_predictions, engine ggeffects, glm", {
  set.seed(123)
  dat <- data.frame(
    outcome = rbinom(n = 100, size = 1, prob = 0.35),
    var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
    var_cont = rnorm(n = 100, mean = 10, sd = 7),
    groups = sample(letters[1:2], size = 100, replace = TRUE)
  )
  m <- glm(outcome ~ var_binom * groups + var_cont, data = dat, family = binomial())

  pr <- predict_response(m, c("var_binom", "groups"))
  out1 <- test_predictions(pr, engine = "ggeffects")
  out2 <- test_predictions(m, c("var_binom", "groups"), engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-3)
  expect_equal(attributes(out1)$standard_error, attributes(out2)$standard_error, tolerance = 1e-3)

  pr <- predict_response(m, c("var_binom", "groups"))
  out1 <- test_predictions(pr, engine = "ggeffects", test = "interaction")
  out2 <- test_predictions(m, c("var_binom", "groups"), engine = "emmeans", test = "interaction")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-2)
  expect_equal(attributes(out1)$standard_error, attributes(out2)$standard_error, tolerance = 1e-2)
})


test_that("test_predictions, engine ggeffects, by-arg and printing levels with dots", {
  set.seed(123)
  dat <- data.frame(
    outcome = rnorm(n = 100),
    x1 = as.factor(sample(c("1. Generation", "2nd Gen", "Gen. 3."), 100, TRUE)),
    x2 = as.factor(sample.int(2, 100, TRUE, prob = c(0.6, 0.4))),
    x3 = as.factor(sample(letters[1:2], 100, TRUE, prob = c(0.3, 0.7)))
  )
  m <- lm(outcome ~ x1 * x2, data = dat)

  pr <- ggpredict(m, c("x1", "x2"))
  out1 <- test_predictions(pr, engine = "ggeffects")
  out2 <- test_predictions(m, c("x1", "x2"), engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-2)
  expect_snapshot(print(out1))

  out1 <- test_predictions(pr, engine = "ggeffects", by = "x2")
  out2 <- test_predictions(m, c("x1", "x2"), engine = "emmeans", by = "x2")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-2)
  expect_snapshot(print(out1))

  m <- lm(outcome ~ x1 * x2 * x3, data = dat)

  pr <- ggpredict(m, c("x1", "x2", "x3"))
  out1 <- test_predictions(pr, engine = "ggeffects")
  out2 <- test_predictions(m, c("x1", "x2", "x3"), engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-2)
  expect_snapshot(print(out1))

  out1 <- test_predictions(pr, engine = "ggeffects", by = c("x2", "x3"))
  expect_snapshot(print(out1))
})


test_that("test_predictions, engine ggeffects, by-arg and column order", {
  data(coffee_data, package = "ggeffects")
  # Median split
  coffee_data$alertness_d <- datawizard::categorize(coffee_data$alertness, lowest = 0)
  coffee_data$treatment <- coffee_data$coffee
  m <- glm(alertness_d ~ time * treatment, data = coffee_data, family = binomial())
  pr <- predict_response(m, terms = c("time", "treatment"))
  out1 <- test_predictions(pr, by = "time", engine = "ggeffects")
  out2 <- test_predictions(pr, by = "time", engine = "emmeans")
  expect_identical(colnames(out1)[1:3], colnames(out2)[1:3])
  expect_snapshot(print(out1))
  expect_snapshot(print(out2))
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-3)
  expect_equal(attributes(out1)$standard_error, attributes(out2)$standard_error, tolerance = 1e-3)
})


test_that("test_predictions, engine ggeffects, Bayes", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("rstanarm")

  set.seed(1234)
  dat <- data.frame(
    outcome = rbinom(n = 100, size = 1, prob = 0.35),
    var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.3)),
    var_cont = rnorm(n = 100, mean = 10, sd = 7),
    groups = sample(letters[1:2], size = 100, replace = TRUE)
  )

  m1 <- glm(outcome ~ var_binom * groups + var_cont, data = dat, family = binomial())
  m2 <- insight::download_model("stanreg_bernoulli_1")
  m3 <- insight::download_model("brms_bernoulli_1")

  skip_if(is.null(m2) || is.null(m3))

  pr1 <- predict_response(m1, c("var_binom", "groups"))
  pr2 <- predict_response(m2, c("var_binom", "groups"))
  pr3 <- predict_response(m3, c("var_binom", "groups"))

  out1 <- test_predictions(pr1, engine = "ggeffects")
  out2 <- test_predictions(pr2, engine = "ggeffects")
  out3 <- test_predictions(pr3, engine = "ggeffects")

  expect_equal(out1$Contrast[1:3], out2$Contrast[1:3], tolerance = 1e-1)
  expect_equal(out2$Contrast[1:3], out3$Contrast[1:3], tolerance = 1e-1)
  expect_equal(out1$conf.low[1:3], out2$conf.low[1:3], tolerance = 1e-1)
  expect_equal(out2$conf.low[1:3], out3$conf.low[1:3], tolerance = 1e-1)
})


skip_if_not_installed("withr")
withr::with_environment(
  new.env(),
  test_that("test_predictions, gamm4 works with engine = ggeffects", {
    skip_on_cran()
    unloadNamespace("gam")
    skip_if_not_installed("gamm4")
    skip_if_not_installed("mgcv")
    skip_if_not_installed("lme4")

    set.seed(123)
    dat <- mgcv::gamSim(1, n = 400, scale = 2) ## simulate 4 term additive truth
    dat$fac <- fac <- as.factor(sample.int(20, 400, replace = TRUE))
    dat$y <- dat$y + model.matrix(~ fac - 1) %*% rnorm(20) * 0.5

    set.seed(123)
    m1 <- gamm4::gamm4(y ~ s(x0) + x1 + s(x2), data = dat, random = ~ (1 | fac))
    pr <- predict_response(m1, "x1 [0.1, 0.5, 0.8]")
    out <- test_predictions(pr, engine = "ggeffects")
    expect_equal(out$Contrast, c(-2.50769, -4.38845, -1.88076), tolerance = 1e-3)
  })
)
