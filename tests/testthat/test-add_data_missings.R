skip_on_os(c("mac", "solaris"))
skip_if_not_installed("datawizard")

test_that("ggpredict, raw data available", {
  data(mtcars)

  # add missing data
  mpg_miss <- mtcars
  set.seed(123)
  mpg_miss$unif <- runif(nrow(mpg_miss))
  mpg_miss$mpg[1] <- NA
  mpg_miss$cyl[2] <- NA
  mpg_miss$unif[3] <- NA

  mtcars_miss <<- mpg_miss

  lm_model <- lm(mpg ~ cyl, data = mtcars_miss, weights = mtcars_miss$unif)
  out <- ggpredict(model = lm_model, terms = "cyl")
  expect_false(is.null(attributes(out)$rawdata))

  lm_model_ok <- lm(mpg ~ cyl, data = mtcars_miss)
  out <- ggpredict(model = lm_model_ok, terms = "cyl")
  expect_false(is.null(attributes(out)$rawdata))
})
