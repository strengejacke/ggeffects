skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("MCMCglmm")

test_that("ggpredict", {
  set.seed(123)
  data(PlodiaPO, package = "MCMCglmm")
  m1 <- MCMCglmm::MCMCglmm(
    PO ~ plate,
    random = ~FSfamily,
    data = PlodiaPO,
    verbose = FALSE,
    nitt = 1300,
    burnin = 300,
    thin = 1
  )

  p <- ggpredict(m1, "plate")
  expect_equal(p$predicted[1], 1.055517, tolerance = 1e-3)
})

test_that("ggpredict", {
  data(iris)
  set.seed(123)
  iris$grp <<- as.factor(sample(1:3, 150, TRUE))

  set.seed(123)
  model <- MCMCglmm::MCMCglmm(
    Sepal.Length ~ Sepal.Width + Species,
    random = ~grp,
    verbose = FALSE,
    data = iris
  )

  p <- ggpredict(model, "Sepal.Width")
  expect_equal(p$predicted[1], 3.862325, tolerance = 1e-3)
  expect_equal(p$conf.low[1], 3.494669, tolerance = 1e-3)
})
