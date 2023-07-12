skip_on_os(c("mac", "linux"))
skip_if(packageVersion("insight") <= "0.19.2")

if (suppressWarnings(requiet("testthat") && requiet("ggeffects") && requiet("marginaleffects") && requiet("ggplot2") && requiet("MASS") && requiet("brglm2"))) {
  data("housing", package = "MASS")
  data("stemcell", package = "brglm2")
  set.seed(123)
  housing$x <- rnorm(nrow(housing))
  m_polr <- MASS::polr(Sat ~ Infl + Type + Cont + x, weights = Freq, data = housing)
  m_bracl <- bracl(research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = stemcell, type = "ML"
  )

  test_that("print hypothesis_test ordinal outcome", {
    out <- suppressWarnings(hypothesis_test(ggpredict(m_polr, "Type")))
    expect_snapshot(print(out))

    out <- suppressWarnings(hypothesis_test(ggpredict(m_polr, c("Type [Terrace, Apartment]", "x [1, 2]"))))
    expect_snapshot(print(out))

    out <- suppressWarnings(hypothesis_test(ggpredict(m_polr, "Type"), test = NULL))
    expect_snapshot(print(out))

    out <- suppressWarnings(hypothesis_test(ggpredict(m_polr, c("Type [Terrace, Apartment]", "x [1, 2]")), test = NULL))
    expect_snapshot(print(out))
  })

  test_that("print hypothesis_test categorical outcome", {
    out <- suppressWarnings(hypothesis_test(ggpredict(m_bracl, "gender")))
    expect_snapshot(print(out))

    out <- suppressWarnings(hypothesis_test(ggpredict(m_bracl, "gender"), test = NULL))
    expect_snapshot(print(out))
  })
}
