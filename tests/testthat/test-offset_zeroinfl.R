skip_on_os(c("mac", "solaris"))
skip_if_not_installed("pscl")

#Generate some zero-inflated data
set.seed(123)
N <- 100 #Samples
x <- runif(N, 0, 10) #Predictor
off <- rgamma(N, 3, 2) #Offset variable
yhat <- -1 + x * 0.5 + log(off) #Prediction on log scale
dat <- data.frame(y = NA, x, logOff = log(off)) #Storage dataframe

dat$y <- rpois(N, exp(yhat)) #Poisson process
dat$y <- ifelse(rbinom(N, 1, 0.3), 0, dat$y) #Zero-inflation process

#Fit zeroinfl model using 2 methods of offset input
m1 <- pscl::zeroinfl(y ~ offset(logOff) + x | 1, data = dat, dist = "poisson")
m2 <- pscl::zeroinfl(y ~ x | 1, data = dat, offset = logOff, dist = "poisson")

#Fit zeroinfl model without offset data
m3 <- pscl::zeroinfl(y ~ x | 1, data = dat, dist = "poisson")

test_that("offset-zeroinfl-1", {
  pr <- ggpredict(m1, "x")
  expect_identical(ncol(pr), 6L)
  expect_named(pr, c("x", "predicted", "std.error", "conf.low", "conf.high", "group"))
  expect_equal(
    pr$conf.low,
    c(0.38151, 0.64241, 1.08141, 1.81951, 3.05894, 5.1351, 8.59457,
      14.28775, 23.41852, 37.68751, 59.9237),
    tolerance = 1e-3
  )
})


test_that("offset-zeroinfl-2", {
  pr <- ggpredict(m2, "x")
  expect_identical(ncol(pr), 6L)
  expect_named(pr, c("x", "predicted", "std.error", "conf.low", "conf.high", "group"))
  expect_equal(
    pr$conf.low,
    c(0.38151, 0.64241, 1.08141, 1.81951, 3.05894, 5.1351, 8.59457,
      14.28775, 23.41852, 37.68751, 59.9237),
    tolerance = 1e-3
  )
})


test_that("offset-zeroinfl-3", {
  pr <- ggpredict(m3, "x")
  expect_identical(ncol(pr), 6L)
  expect_named(pr, c("x", "predicted", "std.error", "conf.low", "conf.high", "group"))
  expect_equal(
    pr$conf.low,
    c(0.76538, 1.21064, 1.91433, 3.02552, 4.77779, 7.53369, 11.84387,
      18.49956, 28.52172, 43.24606, 64.8289),
    tolerance = 1e-3
  )
})
