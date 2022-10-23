if (suppressWarnings(
  requiet("testthat") &&
  requiet("ggeffects") &&
  requiet("rms")
)) {

  #example data
  set.seed(123)
  d <- data.frame(
    y = ifelse(rnorm(100) > 0, 1, 0),
    x = rnorm(100)
  )

  m <- orm(y ~ x, data = d)


  test_that("ggpredict, orm", {
    pr <- ggpredict(m, "x [-2:2 by=1]")
    expect_equal(pr$predicted, c(0.55423, 0.5362, 0.51807, 0.49989, 0.48171), tolerance = 1e-2)
  })

  test_that("ggemmeans, orm", {
    pr <- ggemmeans(m, "x [-2:2 by=1]")
    expect_equal(pr$predicted, c(0.55423, 0.5362, 0.51807, 0.49989, 0.48171), tolerance = 1e-2)
  })
}
