skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("ordinal")
skip_if_not_installed("MASS")
skip_if_not_installed("emmeans")
skip_if_not_installed("effects")
skip_if_not_installed("withr")

withr::with_package(
  "MASS",
  test_that("ggpredict, ordinal", {
    data(wine, package = "ordinal")
    m1 <- ordinal::clm(rating ~ temp * contact, data = wine)

    # ggpredict
    p <- ggpredict(m1, "temp")
    expect_equal(p$predicted[1], 0.1960351, tolerance = 1e-3)
    expect_equal(p$conf.low[1], 0.0772626, tolerance = 1e-3)
    expect_equal(p$conf.high[1], 0.41522921, tolerance = 1e-3)
    p2 <- ggpredict(m1, c("temp", "contact"))
    expect_snapshot(print(p))
    expect_snapshot(print(p2))

    # ggeffect
    p <- ggeffect(m1, "temp")
    expect_equal(p$predicted[1], 0.110564082334497, tolerance = 1e-3)
    ggeffect(m1, c("temp", "contact"))

    # ggemmeans
    p <- ggemmeans(m1, "contact")
    expect_equal(p$predicted[1], 0.1097049, tolerance = 1e-3)
    ggemmeans(m1, c("temp", "contact"))

    # predict_response, empirical
    p <- predict_response(m1, "contact", margin = "average")
    expect_equal(p$predicted[1], 0.1097049, tolerance = 1e-3)
    p <- predict_response(m1, c("temp", "contact"), margin = "average")
    expect_named(p, c(
      "x", "predicted", "std.error", "conf.low", "conf.high", "response.level",
      "group"
    ))
    expect_identical(dim(p), c(20L, 7L))
  })
)
