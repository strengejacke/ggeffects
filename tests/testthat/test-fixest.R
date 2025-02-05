skip_on_cran()
skip_on_os(c("mac", "solaris"))

skip_if_not_installed("fixest")
skip_if_not_installed("marginaleffects")

test_that("fixest", {
  # avoid warnings
  fixest::setFixest_nthreads(1)

  data(trade, package = "fixest")

  m1 <- fixest::femlm(Euros ~ log(dist_km) | Origin + Destination + Product, data = trade)
  m2 <- fixest::feols(
    Sepal.Width ~ Petal.Length | Species | Sepal.Length ~ Petal.Width,
    data = iris
  )

  pr <- ggpredict(m1, "dist_km", verbose = FALSE)
  expect_equal(
    pr$predicted,
    predict(m1, newdata = new_data(m1, "dist_km"), type = "response"),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_equal(
    pr$conf.low,
    c(
      NaN, 6567987.47119, 1918140.84625, 933657.78549, 560181.38314,
      376913.26471, 272669.08511, 207374.63131, 163597.57034, 132722.06505,
      110075.22955, 92936.61294, 79631.35008, 69080.50789, 60562.50146,
      53579.27456, 47777.67671, 42901.41398, 38760.67293
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  pr <- ggpredict(m2, "Petal.Length", verbose = FALSE)
  expect_equal(
    pr$predicted,
    predict(m2, newdata = new_data(m2, "Petal.Length")),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_equal(
    pr$conf.low,
    c(
      -8618.29934, -12816.64871, -17014.99809, -21213.34746, -25411.69683,
      -29610.0462, -33808.39557, -38006.74495, -42205.09432, -46403.44369,
      -50601.79306, -54800.14243, -58998.4918
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  pr <- ggpredict(m2, "Petal.Length", verbose = FALSE, vcov = "iid")
  expect_equal(
    pr$conf.low,
    c(
      -10714.26495, -15960.59713, -21206.92931, -26453.26148, -31699.59366,
      -36945.92584, -42192.25802, -47438.59019, -52684.92237, -57931.25455,
      -63177.58672, -68423.9189, -73670.25108
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})

test_that("fixest, numeric cluster variable works", {
  set.seed(101) # For reproducibility
  dat <- data.frame(
    y = rnorm(200),
    year = sample(c(2010, 2011, 2012), 200, replace = TRUE),
    var = rnorm(200)
  )
  model <- fixest::feols(y ~ var | year, data = dat)
  out <- ggpredict(model, terms = "var")
  expect_equal(out$predicted, c(-0.10301, -0.05684, -0.01067, 0.03551, 0.08168, 0.12785, 0.17402), tolerance = 1e-4)
  expect_equal(out$conf.low, c(-0.98691, -0.64611, -0.3053, 0.03551, -0.21296, -0.46142, -0.70988), tolerance = 1e-4)
})
