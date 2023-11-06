skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("lme4")
skip_if_not_installed("datawizard")
skip_if_not_installed("marginaleffects")

# lmer ----

data(efc, package = "ggeffects")
efc$grp <- datawizard::to_factor(efc$e15relat)
fit <- lme4::lmer(neg_c_7 ~ c12hour + e42dep + c161sex + c172code + (1 | grp), data = efc)


test_that("validate ggpredict lmer against predict", {
  nd <- data_grid(fit, "e42dep")
  pr <- predict(fit, newdata = nd, re.form = NA)
  predicted <- ggpredict(fit, "e42dep")
  expect_equal(predicted$predicted, pr, tolerance = 1e-3, ignore_attr = TRUE)
})


test_that("validate ggpredict lmer against marginaleffects", {
  out1 <- marginaleffects::predictions(
    fit,
    variables = "e42dep",
    newdata = marginaleffects::datagrid(fit)
  )
  out1 <- out1[order(out1$e42dep), ]
  out2 <- ggpredict(
    fit,
    "e42dep",
    condition = c(grp = "child"),
    type = "random",
    interval = "confidence"
  )
  expect_equal(
    out1$estimate,
    out2$predicted,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out1$estimate - stats::qt(0.975, df = 826) * out1$std.error,
    out2$conf.low,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("ggpredict, lmer", {
  expect_s3_class(ggpredict(fit, "c12hour"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex")), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
  expect_s3_class(ggpredict(fit, "c12hour", type = "re"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), type = "re"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "re"), "data.frame")
})


test_that("ggpredict, lmer", {
  pr <- ggpredict(fit, "c12hour")
  expect_equal(pr$std.error[1:5], c(0.2911, 0.2852, 0.2799, 0.2752, 0.2713), tolerance = 1e-3)
  pr <- ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "re")
  expect_equal(pr$std.error[1:5], c(3.5882, 3.58185, 3.58652, 3.58162, 3.57608), tolerance = 1e-3)
  # validate against predict
  pr <- ggpredict(fit, "c12hour")
  nd <- data_grid(fit, "c12hour")
  pr2 <- suppressWarnings(predict(
    fit,
    newdata = nd,
    se.fit = TRUE,
    re.form = NA,
    allow.new.levels = TRUE
  ))
  expect_equal(pr$std.error[1:5], pr2$se.fit[1:5], tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(
    pr$conf.low,
    pr2$fit - qt(0.975, ggeffects:::.get_df(fit)) * pr2$se.fit,
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})


test_that("ggpredict, lmer-simulate", {
  expect_s3_class(ggpredict(fit, "c12hour", type = "sim"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), type = "sim"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "sim"), "data.frame")
})


test_that("ggeffect, lmer", {
  expect_s3_class(ggeffect(fit, "c12hour"), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c161sex")), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
})


test_that("ggeffect, lmer", {
  data(efc, package = "ggeffects")
  efc$cluster <- as.factor(efc$e15relat)
  efc <- datawizard::standardise(efc, c("c160age", "e42dep"), append = "_z")
  m <- lme4::lmer(
    neg_c_7 ~ c160age_z * e42dep_z + c161sex + (1 | cluster),
    data = efc
  )
  p1 <- ggpredict(m, terms = c("c160age_z", "e42dep_z [-1.17,2.03]"))
  p2 <- ggemmeans(m, terms = c("c160age_z", "e42dep_z [-1.17,2.03]"))
  expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)
})


test_that("ggeffect, lmer", {
  data(efc, package = "ggeffects")
  efc$cluster <- as.factor(efc$e15relat)
  efc <- datawizard::to_factor(efc, c("e42dep", "c172code", "c161sex"))
  efc$c172code[efc$c172code == "intermediate level of education"] <- NA
  m <- suppressMessages(lme4::lmer(
    neg_c_7 ~ c172code + e42dep + c161sex + (1 | cluster),
    data = efc
  ))
  expect_s3_class(ggpredict(m, terms = "e42dep"), "data.frame")
  expect_s3_class(ggemmeans(m, terms = "e42dep"), "data.frame")

  p1 <- ggpredict(m, terms = "e42dep")
  p2 <- ggemmeans(m, terms = "e42dep")
  p3 <- ggemmeans(m, terms = "e42dep", condition = c(c161sex = "Male", c172code = "low level of education"))
  expect_equal(p1$predicted[1], 8.902934, tolerance = 1e-3)
  expect_equal(p2$predicted[1], 9.742945, tolerance = 1e-3)
  expect_equal(p1$predicted[1], p3$predicted[1], tolerance = 1e-3)
})


test_that("ggeffect, lmer", {
  data(sleepstudy, package = "lme4")
  m <- suppressWarnings(lme4::lmer(
    log(Reaction) ~ Days + I(Days^2) + (1 + Days + exp(Days) | Subject),
    data = sleepstudy
  ))
  p1 <- ggpredict(m, terms = "Days", verbose = FALSE)
  p2 <- ggemmeans(m, terms = "Days", verbose = FALSE)
  p3 <- ggeffect(m, terms = "Days")
  expect_message(expect_message(ggemmeans(m, terms = "Days"), "polynomial"), "log-transformed")
  expect_equal(p1$predicted[1], 253.5178, tolerance = 1e-3)
  expect_equal(p2$predicted[1], 253.5178, tolerance = 1e-3)
  expect_equal(p3$predicted[1], 5.535434, tolerance = 1e-3)
  expect_s3_class(
    ggpredict(m, terms = c("Days", "Subject [sample=5]"), type = "re", verbose = FALSE),
    "data.frame"
  )
})


test_that("ggpredict, sample random effects levels", {
  N <- 18 # Number of people
  Nt <- 9 # Number of trials

  set.seed(123)
  d <- data.frame(
    Subject = factor(sprintf("%03d", 1:N)), # create subject numbers
    subj_b0 = rnorm(n = N, mean = 250, sd = 20), # create random intercepts
    subj_b1 = rnorm(n = N, mean = 10, sd = 6) # create random slopes
  )
  d <- do.call(rbind, replicate(10, d, simplify = FALSE))
  d$Days <- rep(0:Nt, 18)
  d$Y <- d$subj_b0 + d$Days * d$subj_b1 + rnorm(n = N * (Nt + 1), sd = 15)
  fit <- lme4::lmer(Y ~ 1 + Days + (1 + Days | Subject), data = d)

  set.seed(123)
  p <- ggpredict(fit, terms = c("Days", "Subject [sample=8]"), type = "random")
  expect_identical(
    p$group,
    structure(
      c(
        1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L,
        6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L,
        6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L,
        6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L,
        6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L,
        6L, 7L, 8L
      ),
      levels = c("015", "014", "003", "010", "002", "006", "011", "005"),
      class = "factor"
    )
  )

  d$Subject <- rep(factor(1:N), 10)
  fit <- lme4::lmer(Y ~ 1 + Days + (1 + Days | Subject), data = d)

  set.seed(123)
  p <- ggpredict(fit, terms = c("Days", "Subject [sample=8]"), type = "random")
  expect_identical(
    p$group,
    structure(
      c(
        1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L,
        6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L,
        6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L,
        6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L,
        6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L,
        6L, 7L, 8L
      ),
      levels = c("2", "3", "5", "6", "10", "11", "14", "15"),
      class = "factor"
    )
  )
})
