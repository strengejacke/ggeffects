skip_on_cran()
skip_if_not_installed("ordinal")
skip_if_not_installed("MASS")
skip_if_not_installed("emmeans")
skip_if_not_installed("effects")
skip_if_not_installed("withr")

skip_if(getRversion() < "3.6.0")

withr::with_package(
  "MASS",
  test_that("ggpredict, clmm", {
    data(wine, package = "ordinal")
    m1 <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine)

    # ggpredict
    p <- ggpredict(m1, "temp")
    expect_equal(p$predicted[1], 0.09760731, tolerance = 1e-3)
    ggpredict(m1, c("temp", "contact"))

    # ggeffect
    p <- ggeffect(m1, "temp")
    expect_equal(p$predicted[1], 0.0730260420584538, tolerance = 1e-3)
    ggeffect(m1, c("temp", "contact"))

    # ggemmeans
    p <- ggemmeans(m1, "contact")
    expect_equal(p$predicted[1], 0.08691649, tolerance = 1e-5)
    ggemmeans(m1, c("temp", "contact"))
  })
)
