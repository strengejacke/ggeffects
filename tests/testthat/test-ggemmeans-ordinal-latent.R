skip_on_cran()
skip_if_not_installed("ordinal")
skip_if_not_installed("emmeans")

data(wine, package = "ordinal")
m_clm <- ordinal::clm(rating ~ temp + contact, data = wine)
m_clmm <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine)

# emmeans centers the latent scale at the mean of the thresholds, so the
# centered thresholds must reproduce the cumulative probabilities
.check_thresholds <- function(out, model, thresholds) {
  probs <- ggemmeans(model, c("temp", "contact"), verbose = FALSE)
  for (i in seq_len(nrow(out))) {
    cum_prob <- cumsum(probs$predicted[
      probs$x == out$x[i] & probs$group == out$group[i]
    ])
    expect_equal(
      unname(model$family_link(thresholds - out$predicted[i])),
      cum_prob[seq_along(thresholds)],
      tolerance = 1e-4
    )
  }
}

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

    # thresholds are centered like the latent scale, and reproduce the
    # cumulative probabilities
    thresholds <- attributes(out)$latent_thresholds
    expect_named(thresholds, colnames(m$Theta))
    expect_equal(
      thresholds,
      m$Theta[1, ] - mean(m$Theta[1, ]),
      tolerance = 1e-8,
      ignore_attr = TRUE
    )
    m$family_link <- stats::plogis
    .check_thresholds(out, m, thresholds)

    # scale is flagged as latent, not as logistic
    expect_true(isTRUE(attributes(out)$latent))
    expect_false(isTRUE(as.logical(attributes(out)$logistic)))
  }
})

test_that("ggemmeans, latent = TRUE, single term, print and plot", {
  out <- ggemmeans(m_clmm, "temp", latent = TRUE, verbose = FALSE)
  ref <- as.data.frame(emmeans::emmeans(m_clmm, ~temp, mode = "latent"))
  expect_equal(out$predicted, ref$emmean, tolerance = 1e-6, ignore_attr = TRUE)
  expect_snapshot(print(ggemmeans(
    m_clm,
    "temp",
    latent = TRUE,
    verbose = FALSE
  )))
  skip_if_not_installed("ggplot2")
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  # thresholds are drawn as horizontal lines
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomHline"),
    TRUE
  )))
})

test_that("ggemmeans, latent = TRUE, rescale", {
  out <- ggemmeans(
    m_clm,
    "temp",
    latent = TRUE,
    rescale = c(10, 2),
    verbose = FALSE
  )
  ref <- as.data.frame(
    emmeans::emmeans(m_clm, ~temp, mode = "latent", rescale = c(10, 2))
  )
  expect_equal(out$predicted, ref$emmean, tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(
    attributes(out)$latent_thresholds,
    10 + 2 * (m_clm$Theta[1, ] - mean(m_clm$Theta[1, ])),
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
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
    m_sym$Theta[1, ] - mean(m_sym$Theta[1, ]),
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
  out <- ggemmeans(m_polr, c("Infl", "Type"), latent = TRUE, verbose = FALSE)
  ref <- as.data.frame(emmeans::emmeans(m_polr, ~ Infl + Type, mode = "latent"))
  expect_equal(
    out$predicted,
    ref$emmean[match(paste(out$x, out$group), paste(ref$Infl, ref$Type))],
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  expect_equal(
    attributes(out)$latent_thresholds,
    m_polr$zeta - mean(m_polr$zeta),
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
})

test_that("ggemmeans, unsupported modes and classes", {
  # other emmeans modes for ordinal models are not supported
  expect_error(
    ggemmeans(
      model = m_clm,
      terms = "temp",
      mode = "cum.prob",
      verbose = FALSE
    ),
    "supports"
  )
  # for other models, `latent` and `mode` are ignored with a message
  wine$good <- as.numeric(as.numeric(wine$rating) > 2)
  m_glm <- glm(good ~ temp + contact, data = wine, family = binomial())
  expect_message(
    {
      out <- ggemmeans(m_glm, "temp", latent = TRUE)
    },
    "ignored"
  )
  expect_equal(
    out$predicted,
    ggemmeans(m_glm, "temp", verbose = FALSE)$predicted,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  # default probabilities for ordinal models are unchanged
  out <- ggemmeans(m_clm, "temp", verbose = FALSE)
  expect_true("response.level" %in% colnames(out))
  expect_identical(nrow(out), 2L * nlevels(wine$rating))
})
