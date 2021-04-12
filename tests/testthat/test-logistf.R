if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("logistf")
  && getRversion() >= "3.6.0")) {
  data(sex2)
  m1 <- logistf(case ~ age + oc, data = sex2)

  test_that("ggpredict, logistf", {
    pr <- ggpredict(m1, "age")
    expect_equal(pr$predicted[1], 0.5763746, tolerance = 1e-4)
  })

  test_that("ggeffect, logistf", {
    pr <- ggeffect(m1, "age")
    expect_equal(pr$predicted[1], 0.5762638, tolerance = 1e-4)
  })

  test_that("ggemmeans, logistf", {
    expect_null(ggemmeans(m1, "age"))
  })
}
