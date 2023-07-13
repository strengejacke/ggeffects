if (suppressWarnings(requiet("testthat") && requiet("ggeffects"))) {
  data(mtcars)
  set.seed(123)
  mtcars$unif <- runif(nrow(mtcars))

  # add missing data
  mpg_miss <- mtcars
  mpg_miss$mpg[1] <- NA
  mpg_miss$cyl[2] <- NA
  mpg_miss$unif[3] <- NA

  test_that("ggpredict, raw data available", {
    lm_model <- lm(mpg ~ cyl, data = mpg_miss, weights = mpg_miss$unif)
    out <- ggpredict(model = lm_model, terms = "cyl")
    expect_false(is.null(attributes(out$rawdata)))
  })

  test_that("ggpredict, raw data available", {
    lm_model_ok <- lm(mpg ~ cyl, data = mpg_miss)
    out <- ggpredict(model = lm_model_ok, terms = "cyl")
    expect_false(is.null(attributes(out$rawdata)))
  })
}
