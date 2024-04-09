skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")

test_that("test_predictions, engine emmeans", {
  data(efc, package = "ggeffects")
  efc$c172code <- as.factor(efc$c172code)
  efc$c161sex <- as.factor(efc$c161sex)
  efc$e15relat <- as.factor(efc$e15relat)
  efc$e42dep <- as.factor(efc$e42dep)
  levels(efc$c161sex) <- c("male", "female")

  # single focal term
  m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

  # categorical
  out1 <- test_predictions(m, "c172code")
  out2 <- test_predictions(m, "c172code", engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_identical(out1$c172code, out2$c172code)

  # slope
  out1 <- test_predictions(m, "neg_c_7")
  out2 <- test_predictions(m, "neg_c_7", engine = "emmeans")
  expect_equal(out1$Slope, out2$Slope, tolerance = 1e-3)
  expect_identical(out1$c172code, out2$c172code)

  # multiple focal terms, interaction
  m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex * c172code, data = efc)

  # categorical
  out1 <- test_predictions(m, c("c172code", "c161sex"))
  out2 <- test_predictions(m, c("c172code", "c161sex"), engine = "emmeans")
  expect_equal(out1$Slope, out2$Slope, tolerance = 1e-3)
  expect_identical(out1$c172code, out2$c172code)

  # difference-in-difference
  out1 <- test_predictions(m, c("c172code", "c161sex"), test = "(b1 - b2) = (b4 - b5)")
  out2 <- test_predictions(m, c("c172code", "c161sex"), engine = "emmeans", test = "interaction")
  expect_equal(out1$Contrast[1], out2$Contrast[1], tolerance = 1e-3)
  expect_identical(out2$c172code, c("1-2", "1-3", "2-3"))
  expect_identical(out2$c161sex, c("male and female", "male and female", "male and female"))
  expect_identical(attributes(out2)$test, "interaction")

  # interaction numeric * categorical
  m <- lm(barthtot ~ c12hour + neg_c_7 * c161sex, data = efc)
  out1 <- test_predictions(m, c("neg_c_7", "c161sex"))
  out2 <- test_predictions(m, c("neg_c_7", "c161sex"), engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)

  out1 <- test_predictions(m, c("c161sex", "neg_c_7"))
  out2 <- test_predictions(m, c("c161sex", "neg_c_7"), engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_identical(out1$neg_c_7, out2$neg_c_7)
})


test_that("test_predictions, engine emmeans, glm binomial", {
  set.seed(123)
  dat <- data.frame(
    outcome = rbinom(n = 100, size = 1, prob = 0.35),
    var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
    var_cont = rnorm(n = 100, mean = 10, sd = 7),
    groups = sample(letters[1:4], size = 100, replace = TRUE)
  )

  # single focal term
  m <- glm(outcome ~ var_binom + var_cont + groups,
    data = dat, family = binomial()
  )

  # categorical
  out1 <- test_predictions(m, "var_binom", margin = "marginaleffects")
  out2 <- test_predictions(m, "var_binom", engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-1)
  expect_identical(out1$c172code, out2$c172code)

  # slope
  out1 <- test_predictions(m, "var_cont", margin = "marginaleffects")
  out2 <- test_predictions(m, "var_cont", engine = "emmeans")
  expect_equal(out1$Slope, out2$Slope, tolerance = 1e-2)
  expect_identical(out1$c172code, out2$c172code)

  # multiple focal terms, interaction
  m <- glm(outcome ~ var_binom * var_cont + groups,
    data = dat, family = binomial()
  )

  # interaction numeric * categorical
  out1 <- test_predictions(m, c("var_cont", "var_binom"), margin = "marginaleffects")
  out2 <- test_predictions(m, c("var_cont", "var_binom"), engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-2)
  expect_identical(out1$c172code, out2$c172code)

  # multiple focal terms, interaction
  m <- glm(outcome ~ var_binom * groups,
    data = dat, family = binomial()
  )

  # categorical
  out1 <- test_predictions(m, c("groups", "var_binom"), margin = "marginaleffects")
  out2 <- test_predictions(m, c("groups", "var_binom"), engine = "emmeans")
  expect_equal(
    out1$Contrast[out1$groups == "c-c" & out1$var_binom == "0-1"],
    out2$Contrast[out2$groups == "c-c" & out2$var_binom == "0-1"],
    tolerance = 1e-2
  )

  # # difference-in-difference
  out1 <- test_predictions(m, c("groups", "var_binom"), test = "(b1 - b3) = (b2 - b4)", margin = "marginaleffects")
  out2 <- test_predictions(m, c("groups", "var_binom"), engine = "emmeans", test = "interaction")
  expect_equal(out1$Contrast, out2$Contrast[1], tolerance = 1e-2)
})


test_that("test_predictions, engine emmeans, 3-way interaction", {
  set.seed(12)
  dat <- data.frame(
    outcome = rnorm(n = 100),
    x1 = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
    x2 = as.factor(sample.int(3, 100, TRUE)),
    x3 = sample(letters[1:4], size = 100, replace = TRUE)
  )

  m <- lm(outcome ~ x1 * x2 * x3, data = dat)
  expect_snapshot(print(test_predictions(m, terms = c("x1", "x2", "x3"), engine = "emmeans")))
  expect_snapshot(print(test_predictions(m, terms = c("x1", "x2", "x3"), engine = "emmeans", test = "interaction")))
})


test_that("test_predictions, engine emmeans, by and variable name = level value", {
  data(coffee_data, package = "ggeffects")
  m <- lm(alertness ~ time * coffee + sex, data = coffee_data)

  out1 <- test_predictions(m, c("time", "coffee"), engine = "emmeans")
  emm <- emmeans::emmeans(m, specs = c("time", "coffee"))
  out2 <- as.data.frame(emmeans::contrast(emm, method = "pairwise"))
  expect_equal(out1$Contrast, out2$estimate, tolerance = 1e-3)
  expect_identical(
    unlist(Map(
      function(i, j) paste(i[1], j[1], "-", i[2], j[2]),
      strsplit(out1$time, "-", fixed = TRUE),
      strsplit(out1$coffee, "-", fixed = TRUE)
    )),
    out2$contrast
  )

  out1 <- test_predictions(m, c("time", "coffee"), by = "sex", engine = "emmeans")
  emm <- emmeans::emmeans(m, specs = c("time", "coffee"), by = "sex")
  out2 <- as.data.frame(emmeans::contrast(emm, method = "pairwise"))
  expect_equal(out1$Contrast, out2$estimate, tolerance = 1e-3)
  expect_identical(
    unlist(Map(
      function(i, j) paste(i[1], j[1], "-", i[2], j[2]),
      strsplit(out1$time, "-", fixed = TRUE),
      strsplit(out1$coffee, "-", fixed = TRUE)
    )),
    as.character(out2$contrast)
  )

  expect_snapshot(print(test_predictions(m, c("time", "coffee"), engine = "emmeans")))
  expect_snapshot(print(test_predictions(m, c("time", "coffee"), by = "sex", engine = "emmeans")))

  # check if ggeffects objects works
  pr <- predict_response(m, c("time", "coffee"), margin = "marginalmeans")
  out <- test_predictions(pr, by = "coffee", engine = "emmeans", p_adjust = "tukey")
  expect_equal(out$Contrast, c(1.92766, -1.92766, -3.85532, -5.78298, -5.78298, 0), tolerance = 1e-3)
  expect_equal(out$p.value, c(0.59923, 0.59923, 0.13394, 0.01235, 0.01235, 1), tolerance = 1e-3)
  expect_identical(
    out$time,
    c(
      "morning-noon", "morning-afternoon", "noon-afternoon", "morning-noon",
      "morning-afternoon", "noon-afternoon"
    )
  )

  # validate "by" against emmeans
  out1 <- test_predictions(m, "time", by = "coffee", engine = "emmeans")
  emm <- emmeans::emmeans(m, specs = "time", by = "coffee")
  out2 <- as.data.frame(emmeans::contrast(emm, method = "pairwise"))
  expect_equal(out1$Contrast, out2$estimate, tolerance = 1e-3)
  expect_identical(gsub("-", " - ", out1$time, fixed = TRUE), as.character(out2$contrast))
})
