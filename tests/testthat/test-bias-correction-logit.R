skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("glmmTMB")
skip_if_not_installed("lme4")

test_that("glmmTMB bias correction integrates population predictions and preserves known groups", {
  model <- glmmTMB::glmmTMB(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = binomial()
  )
  newdata <- data.frame(period = levels(lme4::cbpp$period), herd = "1", stringsAsFactors = FALSE)
  terms <- list(period = newdata$period)
  fit <- predict(model, newdata, type = "link", re.form = NA, se.fit = TRUE)
  eta <- unname(fit$fit)
  tau <- sqrt(glmmTMB::VarCorr(model)$cond$herd[1, 1])
  integrated <- function(eta) {
    vapply(eta, function(e) {
      integrate(function(b) plogis(e + b) * dnorm(b, sd = tau), -Inf, Inf, rel.tol = 1e-11)$value
    }, numeric(1))
  }
  tcrit <- qt(0.975, df = insight::get_df(model, type = "wald", verbose = FALSE))

  fixed <- predict_response(model, terms, verbose = FALSE)
  corrected <- predict_response(model, terms, bias_correction = TRUE, verbose = FALSE)
  point_only <- predict_response(model, terms, ci_level = NA, bias_correction = TRUE, verbose = FALSE)
  custom <- predict_response(model, terms, bias_correction = TRUE, sigma = 0, verbose = FALSE)

  expect_equal(fixed$predicted, plogis(eta), tolerance = 1e-10)
  expect_equal(corrected$predicted, integrated(eta), tolerance = 1e-9)
  expect_equal(corrected$conf.low, integrated(eta - tcrit * unname(fit$se.fit)), tolerance = 1e-9)
  expect_equal(corrected$conf.high, integrated(eta + tcrit * unname(fit$se.fit)), tolerance = 1e-9)
  expect_identical(corrected$std.error, fixed$std.error)
  expect_equal(point_only$predicted, corrected$predicted, tolerance = 1e-10)
  expect_equal(custom$predicted, fixed$predicted, tolerance = 1e-10)
  expect_identical(attr(corrected, "link_inverse")(eta), corrected$predicted)
  expect_identical(attr(custom, "link_inverse")(eta), custom$predicted)

  inverse <- attr(corrected, "link_inverse")
  expect_equal(inverse(c(-Inf, 0, Inf, NA_real_)), c(0, 0.5, 1, NA_real_))
  expect_true(all(inverse(37:50) <= 1))

  group_terms <- c(terms, list(herd = "1"))
  expected <- predict(model, newdata, type = "response", re.form = NULL)
  unit <- predict_response(model, group_terms, type = "random", verbose = FALSE)
  unit_corrected <- predict_response(model, group_terms, type = "random", bias_correction = TRUE, verbose = FALSE)
  unit_custom <- predict_response(
    model, group_terms, type = "random", bias_correction = TRUE, sigma = 1, verbose = FALSE
  )

  expect_equal(unit_corrected$predicted, expected, tolerance = 1e-10)
  expect_identical(unit_corrected$conf.low, unit$conf.low)
  expect_identical(unit_corrected$conf.high, unit$conf.high)
  expect_equal(unit_custom$predicted, expected, tolerance = 1e-10)
})

test_that("glmmTMB binomial bias correction resolves small tail probabilities", {
  data <- transform(lme4::cbpp, period_number = as.integer(period))
  model <- glmmTMB::glmmTMB(
    cbind(incidence, size - incidence) ~ period_number + (1 | herd),
    data = data,
    family = binomial(),
    start = list(theta = log(1.01)),
    map = list(theta = factor(NA))
  )
  beta <- glmmTMB::fixef(model)$cond
  eta <- c(-15, -30, -50, -100)
  terms <- list(period_number = (eta - beta[1L]) / beta[2L])
  reference <- vapply(eta, function(e) {
    integrate(function(z) plogis(e + 1.01 * z) * dnorm(z), -9, 9, abs.tol = 0, rel.tol = 1e-12)$value
  }, numeric(1))

  p <- predict_response(model, terms, bias_correction = TRUE, ci_level = NA, verbose = FALSE)
  expect_true(all(p$predicted > 0))
  expect_equal(p$predicted / reference, rep(1, length(eta)), tolerance = 1e-8)
})

test_that("pool_predictions uses the marginal logit and its standard error", {
  terms <- list(period = levels(lme4::cbpp$period))
  newdata <- data.frame(period = terms$period, herd = "1", stringsAsFactors = FALSE)
  make_fit <- function(tau) {
    glmmTMB::glmmTMB(
      cbind(incidence, size - incidence) ~ period + (1 | herd),
      data = lme4::cbpp,
      family = binomial(),
      start = list(theta = log(tau)),
      map = list(theta = factor(NA))
    )
  }
  reference <- function(model, tau) {
    fit <- predict(model, newdata, type = "link", re.form = NA, se.fit = TRUE)
    logit_and_slope <- vapply(unname(fit$fit), function(e) {
      p <- integrate(function(z) plogis(e + tau * z) * dnorm(z), -Inf, Inf, rel.tol = 1e-11)$value
      slope <- integrate(function(z) dlogis(e + tau * z) * dnorm(z), -Inf, Inf, rel.tol = 1e-11)$value
      c(qlogis(p), slope / (p * (1 - p)))
    }, numeric(2))
    list(logit = logit_and_slope[1L, ], variance = (unname(fit$se.fit) * logit_and_slope[2L, ])^2)
  }

  wide <- make_fit(5)
  narrow <- make_fit(0.65)
  first <- predict_response(wide, terms, bias_correction = TRUE, verbose = FALSE)
  second <- predict_response(narrow, terms, bias_correction = TRUE, verbose = FALSE)
  ref_wide <- reference(wide, 5)
  ref_narrow <- reference(narrow, 0.65)
  mean_logits <- (ref_wide$logit + ref_narrow$logit) / 2
  within <- (ref_wide$variance + ref_narrow$variance) / 2
  between <- (ref_wide$logit - ref_narrow$logit)^2 / 2
  std_error <- sqrt(within + 1.5 * between)
  df_complete <- attr(first, "df")
  tcrit <- qnorm(0.975)
  if (is.finite(df_complete)) {
    fraction_missing <- pmax(1.5 * between / std_error^2, 1e-4)
    df_old <- 1 / fraction_missing^2
    df_observed <- (df_complete + 1) / (df_complete + 3) * df_complete * (1 - fraction_missing)
    tcrit <- qt(0.975, df = pmax(1 / (1 / df_old + 1 / df_observed), 1))
  }

  pooled <- pool_predictions(list(first, second))
  duplicate <- pool_predictions(list(first, first))
  rows <- c(3L, 1L, 3L)
  selected <- pool_predictions(list(first[rows, ], second[rows, ]))

  expect_equal(duplicate$predicted, first$predicted, tolerance = 1e-10)
  expect_equal(pooled$predicted, plogis(mean_logits), tolerance = 1e-8)
  expect_equal(pooled$std.error, std_error, tolerance = 1e-8)
  expect_equal(pooled$conf.low, plogis(mean_logits - tcrit * std_error), tolerance = 1e-8)
  expect_equal(pooled$conf.high, plogis(mean_logits + tcrit * std_error), tolerance = 1e-8)
  expect_identical(selected$predicted, pooled$predicted[rows])
  expect_identical(selected$std.error, pooled$std.error[rows])
  expect_error(
    pool_predictions(list(first, predict_response(wide, terms, verbose = FALSE))),
    "same marginal bias correction"
  )
})
