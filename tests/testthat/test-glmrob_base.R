if (suppressWarnings(
  requiet("testthat") &&
  requiet("ggeffects") &&
  requiet("sjmisc") &&
  requiet("robustbase")
)) {
  data(efc, package = "ggeffects")
  efc$neg_c_7d <- sjmisc::dicho(efc$neg_c_7)
  d <<- efc
  m1 <- robustbase::glmrob(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = d, family = binomial)

  test_that("ggpredict, lrm", {
    pr <- ggpredict(m1, "c12hour", verbose = FALSE)
    expect_equal(pr$predicted[1], 0.4035267, tolerance = 1e-4)
  })

  test_that("ggemmeans, lrm", {
    expect_null(ggemmeans(m1, "c12hour", verbose = FALSE))
  })
}
