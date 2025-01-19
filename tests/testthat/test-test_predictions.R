skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("ggplot2")
skip_if_not_installed("lme4")
skip_if_not_installed("nlme")

set.seed(123)
n <- 200
d <- data.frame(
  outcome = rnorm(n),
  groups = as.factor(sample(c("treatment", "control"), n, TRUE)),
  episode = as.factor(sample.int(3, n, TRUE)),
  ID = as.factor(rep(1:10, n / 10)),
  sex = as.factor(sample(c("female", "male"), n, TRUE, prob = c(0.4, 0.6)))
)
model1 <- lm(outcome ~ groups * episode, data = d)


test_that("test_predictions, error", {
  expect_error(
    test_predictions(model1, c("groups", "episode"), engine = "ggeffects"),
    regex = "Argument `engine` must be"
  )
  pr <- predict_response(model1, c("groups", "episode"))
  expect_silent(test_predictions(pr, engine = "ggeffects"))
})


test_that("test_predictions, categorical, pairwise", {
  out <- test_predictions(model1, c("groups", "episode"))
  expect_named(out, c("groups", "episode", "Contrast", "conf.low", "conf.high", "p.value"))
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
  expect_equal(
    attributes(out)$standard_error,
    c(
      0.23286, 0.21745, 0.23533, 0.22247, 0.21449, 0.23558, 0.25218,
      0.24022, 0.23286, 0.23803, 0.22532, 0.21745, 0.24262, 0.23533,
      0.22247
    ),
    tolerance = 1e-3
  )
})

test_that("test_predictions, categorical, pairwise, p_adjust", {
  out1 <- test_predictions(model1, c("groups", "episode"))
  out2 <- test_predictions(model1, c("groups", "episode"), p_adjust = "tukey")
  expect_equal(
    out1$p.value,
    c(
      0.074, 0.3503, 0.5295, 0.7504, 0.5729, 0.009, 0.0258, 0.1497,
      0.2033, 0.8163, 0.2247, 0.137, 0.3676, 0.2538, 0.8215
    ),
    tolerance = 1e-3,
    ignore_attr = FALSE
  )
  expect_equal(
    out2$p.value,
    c(
      0.4704, 0.9366, 0.9887, 0.9996, 0.9931, 0.0927, 0.2215, 0.6985,
      0.7976, 0.9999, 0.8276, 0.6689, 0.9453, 0.862, 0.9999
    ),
    tolerance = 1e-3,
    ignore_attr = FALSE
  )
})

test_that("test_predictions, categorical, NULL", {
  out <- test_predictions(model1, c("groups", "episode"), test = NULL)
  expect_named(out, c("groups", "episode", "Predicted", "conf.low", "conf.high", "p.value"))
  expect_equal(out$Predicted, c(0.028, -0.3903, 0.2316, 0.1763, -0.0428, -0.0931),
    tolerance = 1e-3,
    ignore_attr = FALSE
  )
  expect_identical(
    out$groups,
    structure(c(1L, 2L, 1L, 2L, 1L, 2L), levels = c("control", "treatment"), class = "factor")
  )
  out <- test_predictions(model1, c("groups", "episode"), test = "slope")
  expect_equal(out$Predicted, c(0.028, -0.3903, 0.2316, 0.1763, -0.0428, -0.0931),
    tolerance = 1e-3,
    ignore_attr = FALSE
  )
})


test_that("test_predictions, interaction", {
  data(iris)
  model2 <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
  out <- test_predictions(model2, c("Sepal.Length", "Species"))
  expect_named(out, c("Sepal.Length", "Species", "Contrast", "conf.low", "conf.high", "p.value"))
  expect_equal(out$Contrast, c(0.4788, 0.5666, 0.0878),
    tolerance = 1e-3,
    ignore_attr = FALSE
  )
  expect_identical(out$Sepal.Length, c("slope", "slope", "slope"))
})

test_that("test_predictions, by-argument", {
  skip_if_not_installed("datawizard")
  data(efc, package = "ggeffects")
  efc$c161sex <- datawizard::to_factor(efc$c161sex)
  efc$c172code <- datawizard::to_factor(efc$c172code)

  mfilter <- lm(neg_c_7 ~ c161sex * c172code + e42dep + c12hour, data = efc)
  prfilter <- ggpredict(mfilter, "c172code")

  out <- test_predictions(prfilter, by = "c161sex")
  expect_identical(nrow(out), 6L)
  expect_identical(
    out$c172code,
    c(
      "low level of education-intermediate level of education",
      "low level of education-high level of education",
      "intermediate level of education-high level of education",
      "low level of education-intermediate level of education",
      "low level of education-high level of education",
      "intermediate level of education-high level of education"
    )
  )
  expect_equal(out$p.value, c(0.3962, 0.6512, 0.7424, 0.9491, 0.0721, 0.0288), tolerance = 1e-3)

  out <- test_predictions(prfilter, by = "c161sex", p_adjust = "tukey")
  expect_equal(out$p.value, c(0.6727, 0.8934, 0.9422, 0.9978, 0.1699, 0.0734), tolerance = 1e-3)

  prfilter <- ggpredict(mfilter, c("c172code", "c161sex"))
  out <- test_predictions(prfilter, p_adjust = "tukey")
  out <- out[out$c161sex %in% c("Male-Male", "Female-Female"), , drop = FALSE]
  expect_equal(out$p.value, c(0.9581, 0.9976, 0.9995, 1, 0.4657, 0.2432), tolerance = 1e-3)

  expect_error(test_predictions(mfilter, "c161sex", by = "c12hour"), regex = "categorical")
})


model3 <- suppressMessages(lme4::lmer(outcome ~ groups * episode + sex + (1 | ID), data = d))

test_that("test_predictions, categorical, pairwise", {
  out <- test_predictions(model3, c("groups", "episode"))
  expect_named(out, c("groups", "episode", "Contrast", "conf.low", "conf.high", "p.value"))
  expect_equal(
    out$Contrast,
    c(
      -0.20515, 0.06664, 0.41991, -0.15277, 0.11871, 0.27179, 0.62506,
      0.05238, 0.32386, 0.35326, -0.21941, 0.05207, -0.57267, -0.3012,
      0.27148
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
  expect_identical(
    out$episode,
    c(
      "1-2", "1-3", "1-1", "1-2", "1-3", "2-3", "2-1", "2-2", "2-3",
      "3-1", "3-2", "3-3", "1-2", "1-3", "2-3"
    )
  )
})

test_that("test_predictions, categorical, NULL", {
  out <- test_predictions(model3, c("groups", "episode"), test = NULL)
  out <- out[order(out$groups, out$episode), ]
  expect_named(out, c("groups", "episode", "Predicted", "conf.low", "conf.high", "p.value"))
  expect_equal(out$Predicted, c(0.0559, 0.2611, -0.0107, -0.364, 0.2087, -0.0628),
    tolerance = 1e-3,
    ignore_attr = FALSE
  )
  expect_identical(
    out$groups,
    structure(c(1L, 1L, 1L, 2L, 2L, 2L), levels = c("control", "treatment"), class = "factor")
  )
  expect_identical(
    out$episode,
    structure(c(1L, 2L, 3L, 1L, 2L, 3L), levels = c("1", "2", "3"), class = "factor")
  )
})


d <- nlme::Orthodont
m <- lme4::lmer(distance ~ age * Sex + (1 | Subject), data = d)

test_that("test_predictions, numeric, one focal, pairwise", {
  out <- test_predictions(m, "age")
  expect_named(out, c("age", "Slope", "conf.low", "conf.high", "p.value"))
  expect_equal(out$Slope, 0.6602, tolerance = 1e-3, ignore_attr = FALSE)
})

test_that("test_predictions, numeric, one focal, NULL", {
  out <- test_predictions(m, "age", test = NULL)
  expect_named(out, c("age", "Slope", "conf.low", "conf.high", "p.value"))
  expect_equal(out$Slope, 0.6602, tolerance = 1e-3, ignore_attr = FALSE)
})

test_that("test_predictions, categorical, one focal, pairwise", {
  out <- test_predictions(m, "Sex")
  expect_named(out, c("Sex", "Contrast", "conf.low", "conf.high", "p.value"))
  expect_equal(out$Contrast, 2.321023, tolerance = 1e-3, ignore_attr = FALSE)
})

test_that("test_predictions, categorical, one focal, NULL", {
  out <- test_predictions(m, "Sex", test = NULL)
  expect_named(out, c("Sex", "Predicted", "conf.low", "conf.high", "p.value"))
  expect_equal(out$Predicted, c(24.9688, 22.6477), tolerance = 1e-3, ignore_attr = FALSE)
})


test_that("test_predictions, masked chars in levels", {
  set.seed(123)
  n <- 200
  d <- data.frame(
    outcome = rnorm(n),
    groups = as.factor(sample(c("ta-ca", "tb-cb"), n, TRUE)),
    episode = as.factor(sample.int(3, n, TRUE)),
    ID = as.factor(rep(1:10, n / 10)),
    sex = as.factor(sample(c("1", "2"), n, TRUE, prob = c(0.4, 0.6)))
  )
  model <- suppressMessages(lme4::lmer(outcome ~ groups * sex + episode + (1 | ID), data = d))
  out <- test_predictions(model, c("groups", "sex"))
  expect_named(out, c("groups", "sex", "Contrast", "conf.low", "conf.high", "p.value"))
  expect_equal(
    out$Contrast,
    c(-0.1854, -0.4473, -0.2076, -0.2619, -0.0222, 0.2397),
    tolerance = 1e-3,
    ignore_attr = FALSE
  )
  expect_identical(
    out$groups,
    c(
      "ta-ca-ta-ca", "ta-ca-tb-cb", "ta-ca-tb-cb", "ta-ca-tb-cb",
      "ta-ca-tb-cb", "tb-cb-tb-cb"
    )
  )
  # ggeffects can be passed directly when model is named "model"
  pr <- predict_response(model, c("groups", "sex"))
  expect_silent(test_predictions(pr))
})

test_that("test_predictions, don't drop single columns", {
  data(iris)
  iris$Sepal.Width.factor <- factor(as.numeric(iris$Sepal.Width >= 3))
  m <- lme4::lmer(Petal.Length ~ Petal.Width * Sepal.Width.factor + (1 | Species), data = iris)
  expect_s3_class(
    test_predictions(m, c("Sepal.Width.factor", "Petal.Width [0.5]")),
    "ggcomparisons"
  )
})

test_that("test_predictions, make sure random effects group is categorical", {
  data(sleepstudy, package = "lme4")
  set.seed(123)
  sleepstudy$grp <- as.factor(sample(letters[1:3], nrow(sleepstudy), replace = TRUE))
  sleepstudy$ID <- as.numeric(sleepstudy$Subject)
  m <- lme4::lmer(Reaction ~ Days + (1 | ID), sleepstudy)
  out <- test_predictions(ggpredict(m, "Days"))
  expect_equal(out$Slope, 10.467285959584, tolerance = 1e-4)

  m <- lme4::lmer(Reaction ~ Days * grp + (1 | ID), sleepstudy)
  out <- test_predictions(ggpredict(m, c("Days", "grp")))
  expect_equal(out$Contrast, c(-0.0813, -1.26533, -1.18403), tolerance = 1e-4)
})


test_that("test_predictions, works with glmmTMB and w/o vcov", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("datawizard")
  data(efc, package = "ggeffects")
  efc <- datawizard::to_factor(efc, select = c("c161sex", "c172code", "c175empl"))
  efc <- datawizard::recode_values(
    efc,
    select = "c160age",
    recode = list(`1` = "min:40", `2` = 41:64, `3` = "65:max")
  )
  efc <- datawizard::data_rename(
    efc,
    select = c("c161sex", "c160age", "quol_5", "c175empl"),
    replacement = c("gender", "age", "qol", "employed")
  )
  efc <- datawizard::data_modify(efc, age = factor(age, labels = c("-40", "41-64", "65+")))
  m_null <- glmmTMB::glmmTMB(qol ~ 1 + (1 | gender:employed:age), data = efc)
  predictions <- ggpredict(m_null, c("gender", "employed", "age"), type = "random", ci_level = NA)
  out1 <- test_predictions(predictions, verbose = FALSE)[1:5, ]
  out2 <- test_predictions(predictions, vcov = TRUE, verbose = FALSE)[1:5, ]
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4)
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-4)
  # validate against raw values
  expect_equal(out1$Contrast, c(0.30375, 0.82708, -0.87857, -0.22756, -0.10064), tolerance = 1e-4)
  expect_equal(out1$conf.low, c(0.30375, 0.82708, -0.87857, -0.22756, NA), tolerance = 1e-4)
})


test_that("test_predictions, correct order of character vectors", {
  skip_if_not_installed("marginaleffects", minimum_version = "0.20.0")
  skip_if_not_installed("datawizard")

  set.seed(1234)
  dat <- data.frame(
    outcome = rbinom(n = 100, size = 1, prob = 0.35),
    var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.3)),
    var_cont = rnorm(n = 100, mean = 10, sd = 7),
    groups = sample(letters[1:2], size = 100, replace = TRUE)
  )
  m1 <- glm(outcome ~ var_binom * groups + var_cont, data = dat, family = binomial())
  pr1 <- predict_response(m1, c("var_binom", "groups"), verbose = FALSE)
  out1 <- test_predictions(pr1, engine = "ggeffects")
  out2 <- test_predictions(pr1)
  out2 <- datawizard::data_arrange(out2, c("var_binom", "groups"))
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-4)
  expect_equal(
    as.data.frame(out1)[c("var_binom", "groups")],
    as.data.frame(out2)[c("var_binom", "groups")],
    ignore_attr = TRUE
  )
})


test_that("test_predictions, zero-inflated models", {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  m1 <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson, data = Salamanders
  )
  pr1 <- predict_response(m1, "mined", margin = "empirical")
  out1 <- test_predictions(pr1)
  out2 <- test_predictions(m1, "mined", scale = "conditional")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-4)

  pr1 <- predict_response(m1, "mined", type = "zero_inflated", margin = "empirical")
  out1 <- test_predictions(pr1)
  out2 <- test_predictions(m1, "mined", scale = "response")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-4)

  pr1 <- predict_response(m1, "mined", type = "zi_prob")
  out1 <- test_predictions(pr1)
  out2 <- test_predictions(m1, "mined", scale = "zprob")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-4)

  # validate against emmeans
  skip_if_not_installed("emmeans")
  emm <- emmeans::emmeans(m1, "mined", regrid = "response", component = "zi")
  out3 <- as.data.frame(confint(emmeans::contrast(emm, method = "pairwise")))
  expect_equal(attributes(out1)$standard_error, out3$SE, tolerance = 1e-1)
  expect_equal(out1$conf.low, out3$asymp.LCL, tolerance = 1e-2)
})
