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

  out4 <- predict_response(m_bracl, "gender", margin = "empirical")
  out5 <- predict_response(m_polr, "gender", margin = "empirical")
  out6 <- predict_response(m_nnet, "gender", margin = "empirical")

  expect_named(
    out4,
    c(
      "x", "predicted", "std.error", "conf.low", "conf.high", "response.level",
      "group"
    )
  )
  expect_identical(out4$response.level, out1$response.level)
  expect_equal(out4$predicted, out6$predicted, tolerance = 0.05)
  expect_equal(out5$predicted, c(
    0.30221, 0.44275, 0.1502, 0.10484, 0.29403, 0.44341, 0.15392,
    0.10863
  ), tolerance = 0.05)
})
