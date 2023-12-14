skip_on_os(c("mac", "solaris"))
skip_if_not_installed("rms")
skip_if_not_installed("emmeans")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("ggpredict, lrm", {
    data(efc, package = "ggeffects")
    efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
    d <- efc
    m1 <- rms::lrm(neg_c_7d ~ c12hour + e42dep + c161sex + c172code, data = d)
    pr <- ggpredict(m1, "c12hour", verbose = FALSE)
    expect_equal(pr$predicted[1], 0.4008948, tolerance = 1e-2)
    pr <- ggemmeans(m1, "c12hour", verbose = FALSE)
    expect_equal(pr$predicted[1], 0.4008948, tolerance = 1e-2)
  })
)

skip_if_not_installed("ggplot2")

withr::with_environment(
  new.env(),
  test_that("ggpredict, lrm", {
    data(mpg, package = "ggplot2")
    mpg$cyl_ord <- ordered(mpg$cyl)
    fit <- rms::lrm(cyl_ord ~ hwy, data = mpg, tol = 1e-22)
    pr <- ggpredict(fit, "hwy", verbose = FALSE)
    expect_equal(
      pr$predicted[1:5],
      c(0.00131, 0.00021, 0.03213, 0.96635, 0.00327),
      tolerance = 1e-2
    )
    expect_named(pr, c("x", "predicted", "response.level", "group"))
    expect_identical(
      pr$response.level,
      c(
        "cyl_ord=4", "cyl_ord=5", "cyl_ord=6", "cyl_ord=8", "cyl_ord=4",
        "cyl_ord=5", "cyl_ord=6", "cyl_ord=8", "cyl_ord=4", "cyl_ord=5",
        "cyl_ord=6", "cyl_ord=8", "cyl_ord=4", "cyl_ord=5", "cyl_ord=6",
        "cyl_ord=8", "cyl_ord=4", "cyl_ord=5", "cyl_ord=6", "cyl_ord=8",
        "cyl_ord=4", "cyl_ord=5", "cyl_ord=6", "cyl_ord=8", "cyl_ord=4",
        "cyl_ord=5", "cyl_ord=6", "cyl_ord=8", "cyl_ord=4", "cyl_ord=5",
        "cyl_ord=6", "cyl_ord=8", "cyl_ord=4", "cyl_ord=5", "cyl_ord=6",
        "cyl_ord=8", "cyl_ord=4", "cyl_ord=5", "cyl_ord=6", "cyl_ord=8",
        "cyl_ord=4", "cyl_ord=5", "cyl_ord=6", "cyl_ord=8", "cyl_ord=4",
        "cyl_ord=5", "cyl_ord=6", "cyl_ord=8", "cyl_ord=4", "cyl_ord=5",
        "cyl_ord=6", "cyl_ord=8", "cyl_ord=4", "cyl_ord=5", "cyl_ord=6",
        "cyl_ord=8", "cyl_ord=4", "cyl_ord=5", "cyl_ord=6", "cyl_ord=8",
        "cyl_ord=4", "cyl_ord=5", "cyl_ord=6", "cyl_ord=8", "cyl_ord=4",
        "cyl_ord=5", "cyl_ord=6", "cyl_ord=8"
      )
    )
  })
)
