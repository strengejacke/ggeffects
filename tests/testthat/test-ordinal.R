skip_on_os(c("mac", "linux", "solaris"))
skip_if_not_installed("brglm2")
skip_if_not_installed("MASS")
skip_if_not_installed("nnet")

test_that("print ggpredict ordinal outcome", {
  data("stemcell", package = "brglm2")
  m_bracl <- brglm2::bracl(research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = stemcell, type = "ML"
  )
  m_polr <- MASS::polr(research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = stemcell
  )
  m_nnet <- nnet::multinom(research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = stemcell
  )

  out1 <- ggpredict(m_bracl, "gender")
  out2 <- ggpredict(m_polr, "gender")
  out3 <- ggpredict(m_nnet, "gender")

  expect_equal(out1$predicted, out2$predicted, tolerance = 0.05)
  expect_equal(out1$predicted, out3$predicted, tolerance = 0.05)
})
