skip_on_os(c("mac", "linux", "solaris"))
skip_if_not_installed("brglm2")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("MASS")
skip_if_not_installed("ggplot2")

test_that("print hypothesis_test ordinal outcome", {
  data("housing", package = "MASS")
  set.seed(123)
  housing$x <- rnorm(nrow(housing))
  m_polr <- MASS::polr(Sat ~ Infl + Type + Cont + x, weights = Freq, data = housing)

  out <- suppressWarnings(hypothesis_test(ggpredict(m_polr, "Type")))
  expect_snapshot(print(out))

  out <- suppressWarnings(hypothesis_test(ggpredict(m_polr, c("Type [Terrace, Apartment]", "x [1, 2]"))))
  expect_snapshot(print(out))

  out <- suppressWarnings(hypothesis_test(ggpredict(m_polr, "Type"), test = NULL))
  expect_snapshot(print(out))

  out <- suppressWarnings(hypothesis_test(ggpredict(m_polr, c("Type [Terrace, Apartment]", "x [1, 2]")), test = NULL))
  expect_snapshot(print(out))
})
