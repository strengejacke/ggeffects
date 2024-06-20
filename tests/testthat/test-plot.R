skip_on_os(c("mac", "solaris"))
skip_if_not_installed("sjlabelled")
skip_if_not_installed("datawizard")
skip_if_not_installed("ggplot2")

# lm, linear regression ----

test_that("ggpredict, lm", {
  data(efc, package = "ggeffects")
  efc$c172code <- datawizard::to_factor(efc$c172code)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

  pr <- ggpredict(fit, "c12hour [20,30,40]")
  p <- suppressWarnings(plot(pr))
  p <- suppressWarnings(plot(pr, show_ci = FALSE))
  p <- suppressWarnings(plot(pr, show_ci = TRUE, ci_style = "dot"))
  p <- suppressWarnings(plot(pr, show_data = TRUE))
  p <- suppressWarnings(plot(pr, show_data = TRUE, jitter = FALSE))
  p <- suppressWarnings(plot(pr, colors = "bw"))
  p <- suppressWarnings(plot(pr, colors = "gs"))

  pr <- ggpredict(fit, c("c12hour", "c172code"))
  p <- suppressWarnings(plot(pr))
  p <- suppressWarnings(plot(pr, show_ci = FALSE))
  p <- suppressWarnings(plot(pr, show_ci = TRUE, ci_style = "dot"))
  p <- suppressWarnings(plot(pr, show_data = TRUE))
  p <- suppressWarnings(plot(pr, show_data = TRUE, jitter = 0))
  p <- suppressWarnings(plot(pr, facets = TRUE))
  p <- suppressWarnings(plot(pr, facets = FALSE))
  p <- suppressWarnings(plot(pr, use_theme = FALSE))
  p <- suppressWarnings(plot(pr, colors = "bw"))
  p <- suppressWarnings(plot(pr, colors = "gs"))
})


test_that("plot, correct x-labels order for character vector", {
  d_char <- data.frame(
    x = c("low", "low", "high", "high"),
    y = c(0, 1, 10, 12),
    stringsAsFactors = FALSE
  )
  m_char <- lm(y ~ x, data = d_char)
  preds <- ggpredict(m_char, terms = "x [all]", verbose = FALSE)
  expect_identical(
    attributes(preds)$x.axis.labels,
    c("high", "low")
  )

  preds <- ggpredict(m_char, terms = "x", verbose = FALSE)
  expect_identical(
    attributes(preds)$x.axis.labels,
    c("low", "high")
  )

  d_char <- data.frame(
    x = factor(c("low", "low", "high", "high")),
    y = c(0, 1, 10, 12),
    stringsAsFactors = FALSE
  )
  m_char <- lm(y ~ x, data = d_char)
  preds <- ggpredict(m_char, terms = "x [all]")
  expect_identical(
    attributes(preds)$x.axis.labels,
    c("high", "low")
  )
})


skip_on_cran()
skip_if_not_installed("vdiffr")

test_that("ggpredict, lm", {
  data(efc, package = "ggeffects")
  efc$c172code <- datawizard::to_factor(efc$c172code)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
  pr <- ggpredict(fit, "c12hour")

  vdiffr::expect_doppelganger(
    "Simple plot",
    plot(pr)
  )
  vdiffr::expect_doppelganger(
    "Simple plot, no CI",
    plot(pr, show_ci = FALSE)
  )
  vdiffr::expect_doppelganger(
    "Simple plot, CI bands as dots",
    plot(pr, show_ci = TRUE, ci_style = "dot")
  )
  set.seed(123)
  vdiffr::expect_doppelganger(
    "Simple plot, show data",
    suppressWarnings(plot(pr, show_data = TRUE))
  )
  set.seed(123)
  vdiffr::expect_doppelganger(
    "Simple plot, show data, jitter",
    plot(pr, show_data = TRUE, jitter = TRUE)
  )
  vdiffr::expect_doppelganger(
    "Simple plot, bw",
    plot(pr, colors = "bw")
  )
  vdiffr::expect_doppelganger(
    "Simple plot, grey scale",
    plot(pr, colors = "gs")
  )

  efc$c161sex <- datawizard::to_factor(efc$c161sex)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex * c172code, data = efc)
  pr <- ggpredict(fit, c("c161sex", "c172code"))

  vdiffr::expect_doppelganger(
    "Simple plot, categorical",
    plot(pr)
  )

  vdiffr::expect_doppelganger(
    "Simple plot, categorical, no CI",
    plot(pr, show_ci = FALSE)
  )

  vdiffr::expect_doppelganger(
    "Simple plot, categorical, CI bands as dots",
    plot(pr, show_ci = TRUE, ci_style = "dot")
  )
  set.seed(123)
  vdiffr::expect_doppelganger(
    "Simple plot, categorical, show data",
    suppressWarnings(plot(pr, show_data = TRUE))
  )
  set.seed(123)
  vdiffr::expect_doppelganger(
    "Simple plot, categorical, show data, jitter",
    plot(pr, show_data = TRUE, jitter = TRUE)
  )
  vdiffr::expect_doppelganger(
    "Simple plot, categorical, bw",
    plot(pr, colors = "bw")
  )
  vdiffr::expect_doppelganger(
    "Simple plot, categorical, grey scale",
    plot(pr, colors = "gs")
  )
})


test_that("plot with data points", {
  skip_if_not_installed("betareg")
  skip_if_not_installed("datawizard")
  set.seed(1)
  ex <- data.frame(
    x = rnorm(2000),
    group = sample(letters[1:4], size = 2000, replace = TRUE),
    stringsAsFactors = FALSE
  )
  ex <- datawizard::data_modify(
    ex,
    group_value = datawizard::recode_into(
      group == "a" ~ 1,
      group == "b" ~ 2,
      group == "c" ~ 0,
      group == "d" ~ -1
    ),
    y_latent = x + rnorm(2000) + group_value,
    y = pnorm(y_latent, sd = 3),
    group = as.factor(group)
  )

  beta_fit <- betareg::betareg(y ~ x + group, data = ex)
  beta_fit_preds <- ggpredict(beta_fit, terms = c("x", "group [a, b]"))
  vdiffr::expect_doppelganger(
    "Colored data points with special focal terms",
    plot(beta_fit_preds, show_data = TRUE)
  )
})


test_that("collapse groups works", {
  skip_if_not_installed("lme4")
  data(ChickWeight)
  m <- lme4::lmer(weight ~ Diet + Time + (1 | Chick), data = ChickWeight)
  gge <- ggpredict(m, terms = "Diet")
  vdiffr::expect_doppelganger(
    "Collapse random effects works again",
    plot(gge, collapse_group = TRUE)
  )
})
