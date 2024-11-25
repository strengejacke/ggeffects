skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("MuMIn")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("betareg")

test_that("ggpredict", {
  data(FoodExpenditure, package = "betareg")
  m <- glmmTMB::glmmTMB(
    I(food / income) ~ income + (1 | persons),
    ziformula = ~1,
    data = FoodExpenditure,
    na.action = "na.fail",
    family = glmmTMB::beta_family()
  )
  set.seed(123)
  dr <- MuMIn::dredge(m)
  avg <- MuMIn::model.avg(object = dr, fit = TRUE)
  out <- predict_response(avg, "income")

  expect_equal(
    out$predicted,
    c(
      0.37697, 0.3619, 0.34709, 0.33258, 0.31837, 0.3045, 0.29097,
      0.2778, 0.26501, 0.2526, 0.24058, 0.22895, 0.21773, 0.20691
    ),
    tolerance = 1e-3
  )
  expect_equal(
    out$conf.low,
    c(
      0.31458, 0.30437, 0.29375, 0.28268, 0.27111, 0.25903, 0.24651,
      0.23365, 0.22058, 0.20748, 0.19449, 0.18176, 0.1694, 0.1575
    ),
    tolerance = 1e-3
  )
})
