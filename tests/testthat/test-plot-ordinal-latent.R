skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("vdiffr")
skip_if_not_installed("effects")
skip_if_not_installed("MASS")
skip_if_not_installed("ordinal")

test_that("ggeffect, polr, latent = FALSE", {
  library(MASS)
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  out <- ggeffect(m, c("Infl", "Type"), latent = FALSE)
  expect_snapshot(print(out))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "polr, latent = FALSE",
    suppressWarnings(plot(out))
  )

  data(wine, package = "ordinal")
  fm1 <- ordinal::clm(rating ~ temp * contact, data = wine)
  out <- ggeffect(fm1, c("temp", "contact"), latent = FALSE)
  expect_snapshot(print(out))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "clm, latent = FALSE",
    suppressWarnings(plot(out))
  )
})


test_that("ggeffect, polr, latent = TRUE", {
  library(MASS) # nolint
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  out <- ggeffect(m, c("Infl", "Type"), latent = TRUE)
  expect_snapshot(print(out))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "polr, latent = TRUE",
    suppressWarnings(plot(out))
  )
  pc <- test_predictions(out, engine = "ggeffects")
  expect_equal(
    pc$Contrast[1:10],
    c(
      -0.20616, 0.51866, 0.72483, 0.57235, 0.36619, 1.09101, 1.08266,
      1.80748, 2.01365, 1.86117
    ),
    tolerance = 1e-3
  )
  expect_identical(
    pc$Infl[1:10],
    c(
      "High-High", "High-High", "High-High", "High-High", "High-High",
      "High-High", "High-Low", "High-Low", "High-Low", "High-Low"
    )
  )

  data(wine, package = "ordinal")
  fm1 <- ordinal::clm(rating ~ temp * contact, data = wine)
  out <- ggeffect(fm1, c("temp", "contact"), latent = TRUE)
  expect_snapshot(print(out))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "clm, latent = TRUE",
    suppressWarnings(plot(out))
  )
})
