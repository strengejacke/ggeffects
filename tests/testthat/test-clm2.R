skip_on_cran()
skip_if_not_installed("ordinal")
skip_if_not_installed("MASS")
skip_if_not_installed("emmeans")
skip_if_not_installed("effects")
skip_if_not_installed("withr")

skip_if(getRversion() < "3.6.0")

withr::with_package(
  "MASS",
  test_that("ggpredict, clm2", {
    data(housing, package = "MASS")
    m1 <- ordinal::clm2(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
    # ggpredict
    expect_error(ggpredict(m1, "Infl"))
    # ggeffect
    p <- ggeffect(m1, "Infl")
    expect_equal(p$predicted[1], 0.457877729905463, tolerance = 1e-3)
    expect_s3_class(ggeffect(m1, c("Infl", "Type")), "data.frame")
    # ggemmeans
    expect_error(ggemmeans(m1, "Infl"))
  })
)
