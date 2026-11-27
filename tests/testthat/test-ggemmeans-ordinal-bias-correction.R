skip_on_cran()
skip_if_not_installed("ordinal")
skip_if_not_installed("emmeans")

test_that("ggemmeans applies bias_correction for clmm models", {
  data(wine, package = "ordinal")
  fit <- ordinal::clmm(rating ~ temp + (1 | judge), data = wine, nAGQ = 31, link = "logit")

  # With explicit sigma
  sigma_val <- 0.951
  out_gge <- ggemmeans(
    fit,
    terms = "temp",
    bias_correction = TRUE,
    sigma = sigma_val,
    verbose = FALSE
  )
  out_emm <- as.data.frame(emmeans::emmeans(
    fit,
    specs = c("rating", "temp"),
    mode = "prob",
    bias.adjust = TRUE,
    sigma = sigma_val
  ))
  out_unadj <- ggemmeans(
    fit,
    terms = "temp",
    bias_correction = FALSE,
    verbose = FALSE
  )

  # Order both by response level and predictor to compare directly
  out_gge <- out_gge[order(out_gge$response.level, out_gge$x), ]
  out_emm <- out_emm[order(out_emm$rating, out_emm$temp), ]
  out_unadj <- out_unadj[order(out_unadj$response.level, out_unadj$x), ]

  expect_equal(out_gge$predicted, out_emm$prob, tolerance = 1e-4)
  expect_equal(out_gge$std.error, out_emm$SE, tolerance = 1e-4)
  expect_equal(out_gge$conf.low, out_emm$asymp.LCL, tolerance = 1e-4)
  expect_equal(out_gge$conf.high, out_emm$asymp.UCL, tolerance = 1e-4)
  expect_gt(max(abs(out_gge$predicted - out_unadj$predicted)), 0.03)

  # With default sigma derived from residual variance
  out_gge_default <- ggemmeans(
    fit,
    terms = "temp",
    bias_correction = TRUE,
    verbose = FALSE
  )
  out_emm_default <- as.data.frame(emmeans::emmeans(
    fit,
    specs = c("rating", "temp"),
    mode = "prob",
    bias.adjust = TRUE,
    sigma = sqrt(insight::get_variance_residual(fit))
  ))
  out_gge_default <- out_gge_default[order(out_gge_default$response.level, out_gge_default$x), ]
  out_emm_default <- out_emm_default[order(out_emm_default$rating, out_emm_default$temp), ]

  expect_equal(out_gge_default$predicted, out_emm_default$prob, tolerance = 1e-4)
  expect_equal(out_gge_default$std.error, out_emm_default$SE, tolerance = 1e-4)
})
