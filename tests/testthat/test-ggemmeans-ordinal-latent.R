skip_on_cran()
skip_if_not_installed("ordinal")
skip_if_not_installed("emmeans")

data(wine, package = "ordinal")
m_clm <- ordinal::clm(rating ~ temp + contact, data = wine)
m_clmm <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine)

test_that("ggemmeans, latent = TRUE, clm and clmm", {
  for (m in list(m_clm, m_clmm)) {
    out <- ggemmeans(m, c("temp", "contact"), latent = TRUE, verbose = FALSE)
    ref <- as.data.frame(emmeans::emmeans(m, ~ temp + contact, mode = "latent"))

    # one row per grid point, no response levels
    expect_identical(nrow(out), nrow(ref))
    expect_false("response.level" %in% colnames(out))
    expect_named(
      as.data.frame(out),
      c("x", "predicted", "std.error", "conf.low", "conf.high", "group")
    )

    # values match emmeans on the latent scale
    key_out <- paste(out$x, out$group)
    key_ref <- paste(ref$temp, ref$contact)
    expect_equal(
      out$predicted,
      ref$emmean[match(key_out, key_ref)],
      tolerance = 1e-6,
      ignore_attr = TRUE
    )
    expect_equal(
      out$std.error,
      ref$SE[match(key_out, key_ref)],
      tolerance = 1e-6,
      ignore_attr = TRUE
    )

    # thresholds are available for plotting, and the scale is not logistic
    expect_equal(
      attributes(out)$latent_thresholds,
      m$alpha,
      tolerance = 1e-8,
      ignore_attr = TRUE
    )
    expect_named(attributes(out)$latent_thresholds, colnames(m$Theta))
    expect_false(isTRUE(as.logical(attributes(out)$logistic)))
  }
})

test_that("ggemmeans, latent = TRUE, single term and plot", {
  out <- ggemmeans(m_clmm, "temp", latent = TRUE, verbose = FALSE)
  ref <- as.data.frame(emmeans::emmeans(m_clmm, ~temp, mode = "latent"))
  expect_equal(out$predicted, ref$emmean, tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(
    attributes(out)$latent_thresholds,
    m_clmm$alpha,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot(out), "ggplot")
})

test_that("ggemmeans, mode = 'latent' still works when model is named", {
  out <- ggemmeans(
    model = m_clm,
    terms = "temp",
    mode = "latent",
    verbose = FALSE
  )
  ref <- as.data.frame(emmeans::emmeans(m_clm, ~temp, mode = "latent"))
  expect_equal(out$predicted, ref$emmean, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("ggemmeans, default probabilities unchanged for ordinal models", {
  out <- ggemmeans(m_clmm, "temp", verbose = FALSE)
  expect_true("response.level" %in% colnames(out))
  expect_identical(nrow(out), 2L * nlevels(wine$rating))
  expect_equal(out$predicted[1], 0.09760731, tolerance = 1e-3)
})

test_that("ggemmeans, latent = TRUE, structured thresholds", {
  # with symmetric thresholds, `alpha` holds the reduced parameters, the
  # actual thresholds are in `Theta`
  m_sym <- ordinal::clm(
    rating ~ temp + contact,
    data = wine,
    threshold = "symmetric"
  )
  out <- ggemmeans(m_sym, "temp", latent = TRUE, verbose = FALSE)
  thresholds <- attributes(out)$latent_thresholds
  expect_named(thresholds, c("1|2", "2|3", "3|4", "4|5"))
  expect_equal(
    thresholds,
    m_sym$Theta[1, ],
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  expect_equal(
    out$predicted,
    as.data.frame(emmeans::emmeans(m_sym, ~temp, mode = "latent"))$emmean,
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})

test_that("ggemmeans, latent = TRUE, polr", {
  skip_if_not_installed("MASS")
  data(housing, package = "MASS")
  m_polr <- MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing)
  out <- ggemmeans(m_polr, "Infl", latent = TRUE, verbose = FALSE)
  expect_equal(
    out$predicted,
    as.data.frame(emmeans::emmeans(m_polr, ~Infl, mode = "latent"))$emmean,
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  expect_identical(attributes(out)$latent_thresholds, m_polr$zeta)
})
