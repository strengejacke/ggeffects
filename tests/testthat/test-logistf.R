if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("logistf")
)) {
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

  set.seed(123)
  sex2$oc <- sample(1:3, nrow(sex2), replace = T)
  sex2$ac <- sample(c("a", "b", "c", "d"), nrow(sex2), replace = T)
  m2 <- logistf(case ~ age + oc + ac, data = sex2)

  test_that("ggpredict, logistf", {
    pr <- ggpredict(m2, c("age", "oc", "ac"))
    expect_equal(
      pr$predicted,
      c(0.6506, 0.4752, 0.5609, 0.419, 0.6917, 0.5218, 0.6061, 0.4649,
        0.73, 0.5679, 0.6496, 0.5114, 0.4398, 0.2763, 0.35, 0.2332, 0.4861,
        0.315, 0.3935, 0.2681, 0.5326, 0.3566, 0.4387, 0.3062),
      tolerance = 1e-2
    )
  })
}
