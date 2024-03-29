skip_on_os(c("mac", "solaris"))
skip_if_not_installed("datawizard")

test_that("ggpredict, raw data available", {
  set.seed(1234)
  x <- rnorm(200)
  z <- rnorm(200)
  # quadratic relationship
  y <- 2 * x + x^2 + 4 * z + rnorm(200)

  d <- data.frame(x, y, z)
  mrowname <- lm(y ~ x + z, data = d)
  pr <- ggpredict(mrowname, "x [all]")
  expect_false(is.null(attributes(pr)$rawdata))
  expect_identical(
    attributes(pr)$rawdata$rowname,
    as.character(seq_len(200))
  )
})
