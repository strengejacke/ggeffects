unloadNamespace("gam")

.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest && requiet("testthat") && requiet("ggeffects") && requiet("VGAM")) {
  d.AD <- data.frame(
    treatment = gl(3, 3),
    outcome = gl(3, 1, 9),
    counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
  )

  m1 <- vglm(counts ~ outcome + treatment, family = poissonff, data = d.AD, trace = TRUE)

  test_that("ggpredict", {
    p <- ggpredict(m1, "outcome")
    expect_equal(p$predicted[1], 21, tolerance = 1e-3)
  })




  set.seed(123)
  N     <- 100
  X1    <- rnorm(N, 175, 7)
  X2    <- rnorm(N,  30, 8)
  Ycont <- 0.5 * X1 - 0.3 * X2 + 10 + rnorm(N, 0, 6)

  Yord  <- cut(
    Ycont,
    breaks = quantile(Ycont),
    include.lowest = TRUE,
    labels = c("--", "-", "+", "++"),
    ordered = TRUE
  )

  dfOrd <- data.frame(X1, X2, Yord)

  m2 <- vglm(Yord ~ X1 + X2, family = propodds, data = dfOrd)

  test_that("ggpredict", {
    p <- ggpredict(m2, terms = "X1")
    expect_equal(p$predicted[1], 0.2633227, tolerance = 1e-3)
    expect_equal(nrow(p), 27)

    p <- ggpredict(m2, terms = "X1", ci = NA)
    expect_equal(p$predicted[1], 0.7366773, tolerance = 1e-3)
    expect_equal(nrow(p), 36)
  })


  data(pneumo)
  pneumo <- transform(pneumo, let = log(exposure.time))
  m3 <- vglm(cbind(normal, mild, severe) ~ let, propodds, data = pneumo)

  test_that("ggpredict", {
    p <- ggpredict(m3, "let")
    expect_equal(p$predicted[1], 0.005992263, tolerance = 1e-3)
    expect_equal(nrow(p), 16)

    p <- ggpredict(m3, "let", ci = NA)
    expect_equal(p$predicted[1], 0.9940077, tolerance = 1e-3)
    expect_equal(nrow(p), 24)
  })
}
