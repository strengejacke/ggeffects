if (require("testthat") && require("ggeffects") && require("MCMCglmm")) {
  data(PlodiaPO)
  m1 <- MCMCglmm(
    PO ~ plate,
    random = ~FSfamily,
    data = PlodiaPO,
    verbose = FALSE,
    nitt = 1300,
    burnin = 300,
    thin = 1
  )

  test_that("ggpredict", {
    p <- ggpredict(m1, "plate")
    expect_equal(p$predicted[1], 1.055984, tolerance = 1e-5)
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, "plate")
    expect_equal(p$predicted[1], 1.056472, tolerance = 1e-5)
  })
}
