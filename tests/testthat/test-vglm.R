skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("VGAM")
unloadNamespace("gam")

test_that("ggpredict", {
  d.AD <- data.frame(
    treatment = gl(3, 3),
    outcome = gl(3, 1, 9),
    counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
  )
  m1 <- VGAM::vglm(
    counts ~ outcome + treatment,
    family = VGAM::poissonff,
    data = d.AD,
    trace = TRUE
  )
  p <- ggpredict(m1, "outcome")
  expect_equal(p$predicted[1], 21, tolerance = 1e-3)
})


test_that("ggpredict", {
  set.seed(123)
  N <- 100
  X1 <- rnorm(N, 175, 7)
  X2 <- rnorm(N, 30, 8)
  Ycont <- 0.5 * X1 - 0.3 * X2 + 10 + rnorm(N, 0, 6)

  Yord <- cut(
    Ycont,
    breaks = quantile(Ycont),
    include.lowest = TRUE,
    labels = c("--", "-", "+", "++"),
    ordered = TRUE
  )
  dfOrd <- data.frame(X1, X2, Yord)
  m2 <- VGAM::vglm(Yord ~ X1 + X2, family = VGAM::propodds, data = dfOrd)
  p <- ggpredict(m2, terms = "X1")

  expect_equal(p$predicted[1], 0.2633227, tolerance = 1e-3)
  expect_identical(nrow(p), 27L)

  p <- ggpredict(m2, terms = "X1", ci_level = NA)
  expect_equal(p$predicted[1], 0.7366773, tolerance = 1e-3)
  expect_identical(nrow(p), 36L)
})


test_that("ggpredict", {
  data(pneumo, package = "VGAM")
  pneumo <- transform(pneumo, let = log(exposure.time))
  m3 <- VGAM::vglm(cbind(normal, mild, severe) ~ let, VGAM::propodds, data = pneumo)

  p <- ggpredict(m3, "let")
  expect_equal(p$predicted[1], 0.005992263, tolerance = 1e-3)
  expect_identical(nrow(p), 16L)

  p <- ggpredict(m3, "let", ci_level = NA)
  expect_equal(p$predicted[1], 0.9940077, tolerance = 1e-3)
  expect_identical(nrow(p), 24L)
})


test_that("ggpredict, multivariate vglm", {
  data("hunua", package = "VGAM")
  shunua <- hunua[sort.list(with(hunua, altitude)), ]

  fit <- VGAM::vglm(cbind(agaaus, kniexc) ~ altitude,
    VGAM::binomialff(multiple.responses = TRUE),
    data = shunua
  )

  # validate against predict
  out <- ggpredict(fit, "altitude [all]")
  nd <- new_data(fit, "altitude [all]")
  pr <- as.data.frame(predict(fit, newdata = nd))

  expect_equal(out$predicted[c(TRUE, FALSE)], plogis(pr[[1]]), tolerance = 1e-3)
  expect_equal(out$predicted[c(FALSE, TRUE)], plogis(pr[[2]]), tolerance = 1e-3)
})
