unloadNamespace("gam")

if (require("testthat") && require("ggeffects") && require("VGAM")) {
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
}
