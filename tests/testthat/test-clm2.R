skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("ordinal")
skip_if_not_installed("MASS")
skip_if_not_installed("emmeans")
skip_if_not_installed("effects")
skip_if_not_installed("withr")

withr::with_package(
  "MASS",
  test_that("ggpredict, clm2", {
    data(housing, package = "MASS")
    m1 <- ordinal::clm2(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
    # ggpredict
    p <- ggpredict(m1, "Infl")
    expect_equal(p$predicted[1], 0.3784494, tolerance = 1e-3)
    expect_s3_class(p, "data.frame")
    expect_snapshot(print(p))
    # ggeffect
    p <- ggeffect(m1, "Infl")
    expect_equal(p$predicted[1], 0.457877729905463, tolerance = 1e-3)
    expect_s3_class(ggeffect(m1, c("Infl", "Type")), "data.frame")
    # ggemmeans
    expect_error(ggemmeans(m1, "Infl"), regex = "Can't handle")
  })
)
