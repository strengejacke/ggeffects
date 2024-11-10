skip_on_os(c("mac", "solaris"))
skip_if_not_installed("mlogit")
skip_if_not_installed("dfidx")

test_that("ggpredict mlogit", {
  data("Fishing", package = "mlogit")
  Fish <- dfidx::dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")
  Fish$mode <- as.numeric(Fish$mode)

  m <- mlogit::mlogit(mode ~ price + catch, data = Fish)
  out <- predict_response(m, "price")
  expect_equal(
    out$predicted[1:5],
    c(0.81127, 0.16256, 0.02552, 0.00065, 0.81127),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
