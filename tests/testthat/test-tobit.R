if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("AER")
)) {
  unloadNamespace("VGAM")
  context("ggeffects, tobit")

  data("Affairs")
  m1 <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating, data = Affairs)

  test_that("ggpredict, tobit", {
    pr <- ggpredict(m1, "yearsmarried", condition = c(religiousness = 3, occupation = 5, rating = 4))
    expect_equal(pr$predicted[1], -10.15089, tolerance = 1e-4)
  })

  test_that("ggeffect, tobit", {
    expect_null(ggeffect(m1, "yearsmarried"))
  })

  test_that("ggemmeans, tobit", {
    pr <- ggemmeans(m1, "yearsmarried", condition = c(religiousness = 3, occupation = 5, rating = 4))
    expect_equal(pr$predicted[1], -10.15089, tolerance = 1e-4)
  })
}
