test_that("typical", {
  data(iris)
  mtyp <- lm(Sepal.Width ~ Sepal.Length + Petal.Length + Species, data = iris)
  out1 <- ggpredict(mtyp, "Sepal.Length")
  expect_equal(
    out1$predicted,
    c(
      3.0269, 3.10224, 3.17759, 3.25293, 3.32827, 3.40361, 3.47896,
      3.5543, 3.62964, 3.70499, 3.78033, 3.85567, 3.93101, 4.00636,
      4.0817, 4.15704, 4.23239, 4.30773, 4.38307
    ),
    tolerance = 1e-4
  )
  out2 <- ggpredict(mtyp, "Sepal.Length", typical = "median")
  expect_equal(
    out2$predicted,
    c(
      3.00177, 3.07711, 3.15246, 3.2278, 3.30314, 3.37848, 3.45383,
      3.52917, 3.60451, 3.67985, 3.7552, 3.83054, 3.90588, 3.98123,
      4.05657, 4.13191, 4.20725, 4.2826, 4.35794
    ),
    tolerance = 1e-4
  )
  out3 <- ggpredict(mtyp, "Sepal.Length", typical = "mode")
  expect_equal(
    out3$predicted,
    c(
      3.127, 3.20234, 3.27769, 3.35303, 3.42837, 3.50371, 3.57906,
      3.6544, 3.72974, 3.80509, 3.88043, 3.95577, 4.03111, 4.10646,
      4.1818, 4.25714, 4.33248, 4.40783, 4.48317
    ),
    tolerance = 1e-4
  )
  out4 <- ggpredict(mtyp, "Sepal.Length", typical = "weighted.mean")
  expect_equal(out1$predicted, out4$predicted, tolerance = 1e-4)
  out5 <- ggpredict(mtyp, "Sepal.Length", typical = c(numeric = "median", factor = "mode"))
  expect_equal(
    out5$predicted,
    c(
      3.00177, 3.07711, 3.15246, 3.2278, 3.30314, 3.37848, 3.45383,
      3.52917, 3.60451, 3.67985, 3.7552, 3.83054, 3.90588, 3.98123,
      4.05657, 4.13191, 4.20725, 4.2826, 4.35794
    ),
    tolerance = 1e-4
  )
  data(iris)
  set.seed(123)
  iris$f <- as.factor(sample(c("a", "b", "c"), 150, replace = TRUE))
  mtyp2 <- lm(Sepal.Width ~ Sepal.Length + Petal.Length + f, data = iris)
  out6 <- ggpredict(mtyp2, "Sepal.Length", typical = "median")
  out7 <- ggpredict(mtyp2, "Sepal.Length", typical = c(numeric = "median", factor = "mode"))
  expect_false(identical(out6$predicted, out7$predicted))
})
