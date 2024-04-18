skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")

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
  expect_equal(out1$Contrast[1:2], out2$Contrast[1:2], tolerance = 1e-3)
  expect_equal(out1$CI_low[1:2], out2$conf.low[1:2], tolerance = 1e-2)
  expect_identical(out1$c172code[1:2], out2$c172code[1:2])
  expect_equal(attributes(out1)$standard_error[1:2], attributes(out2)$standard_error[1:2], tolerance = 1e-1)

  # difference-in-difference
  pr <- ggemmeans(m, c("c172code", "c161sex"))
  out1 <- test_predictions(pr, engine = "ggeffects", test = "interaction")
  out2 <- test_predictions(m, c("c172code", "c161sex"), engine = "emmeans", test = "interaction")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$CI_low, out2$conf.low, tolerance = 1e-2)
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
    out1$CI_low,
    c(
      -6.30176, 2.28462, 4.69154, 3.18193, 5.86451, -3.62851, 9.13784,
      11.28046, 2.00504, 2.51274, 12.19047, 14.73015, 5.29921, 6.1644,
      -4.14491
    ),
    tolerance = 1e-3
  )
  expect_equal(
    attributes(out1)$standard_error,
    c(
      2.62943, 2.81282, 2.16782, 2.43774, 1.65229, 1.93083, 3.40259,
      2.89225, 3.05993, 2.71915, 2.61026, 1.89761, 2.14453, 1.62161,
      2.87483
    ),
    tolerance = 1e-3
  )

  # difference-in-difference
  pr <- ggemmeans(m, c("c161sex", "neg_c_7"))
  out1 <- test_predictions(pr, engine = "ggeffects", test = "interaction")
  out2 <- test_predictions(m, c("c161sex", "neg_c_7"), engine = "emmeans", test = "interaction")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$CI_low, c(-7.70486, -10.28511, -8.13332), tolerance = 1e-3)
  ## FIXME: SEs are larger than for emmeans
  expect_equal(
    attributes(out1)$standard_error,
    c(3.26221, 3.89597, 3.46306),
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
  expect_equal(out1$Contrast[1:2], out2$Contrast[1:2], tolerance = 1e-3)
  expect_equal(out1$CI_low[1:2], out2$conf.low[1:2], tolerance = 1e-2)
  expect_equal(attributes(out1)$standard_error[1:2], attributes(out2)$standard_error[1:2], tolerance = 1e-1)

  pr <- predict_response(m, c("var_binom", "groups"))
  out1 <- test_predictions(pr, engine = "ggeffects", test = "interaction")
  out2 <- test_predictions(m, c("var_binom", "groups"), engine = "emmeans", test = "interaction")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(out1$CI_low, out2$conf.low, tolerance = 1e-2)
  expect_equal(attributes(out1)$standard_error, attributes(out2)$standard_error, tolerance = 1e-1)
})
