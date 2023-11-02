skip_on_os(c("mac", "solaris"))
skip_if_not_installed("rms")
skip_if_not_installed("effects")
skip_if_not_installed("emmeans")
skip_if_not_installed("withr")

# test_that("ggpredict, lrm", {
#   data(efc, package = "ggeffects")
#   efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
#   d <<- efc
#   m1 <- rms::lrm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = d)
#   pr <- ggpredict(m1, "c12hour")
#   expect_equal(pr$predicted[1], 0.4008948, tolerance = 1e-2)
#   expect_null(ggeffect(m1, "c12hour"))
#   pr <- ggemmeans(m1, "c12hour")
#   expect_equal(pr$predicted[1], 0.4008948, tolerance = 1e-2)
# })

withr::with_environment(
  new.env(),
  test_that("ggpredict, lrm", {
    data(efc, package = "ggeffects")
    efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
    d <- efc
    m1 <- rms::lrm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = d)
    pr <- ggpredict(m1, "c12hour")
    expect_equal(pr$predicted[1], 0.4008948, tolerance = 1e-2)
    pr <- ggeffect(m1, "c12hour")
    expect_equal(pr$predicted[1], 0.4008948, tolerance = 1e-2)
    pr <- ggemmeans(m1, "c12hour")
    expect_equal(pr$predicted[1], 0.4008948, tolerance = 1e-2)
  })
)
