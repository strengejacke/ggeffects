if (suppressWarnings(
  requiet("testthat") &&
  requiet("ggeffects") &&
  requiet("sjmisc") &&
  requiet("rms")
)) {

  data(efc)
  efc$neg_c_7d <- dicho(efc$neg_c_7)
  d <<- efc
  m1 <- lrm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = d)

  test_that("ggpredict, lrm", {
    pr <- ggpredict(m1, "c12hour")
    expect_equal(pr$predicted[1], 0.4008948, tolerance = 1e-2)
  })

  test_that("ggeffect, lrm", {
    expect_null(ggeffect(m1, "c12hour"))
  })

  test_that("ggemmeans, lrm", {
    pr <- ggemmeans(m1, "c12hour")
    expect_equal(pr$predicted[1], 0.4008948, tolerance = 1e-2)
  })
}

