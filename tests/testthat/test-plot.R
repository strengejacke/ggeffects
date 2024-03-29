skip_on_os(c("mac", "solaris"))
skip_if_not_installed("sjlabelled")
skip_if_not_installed("datawizard")

# lm, linear regression ----

data(efc, package = "ggeffects")
efc$c172code <- datawizard::to_factor(efc$c172code)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

test_that("ggpredict, lm", {
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
  preds <- ggpredict(m_char, terms = "x [all]")
  expect_identical(
    attributes(preds)$x.axis.labels,
    c("high", "low")
  )

  preds <- ggpredict(m_char, terms = "x")
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
