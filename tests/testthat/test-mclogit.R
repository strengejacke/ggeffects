skip_on_os(c("mac", "solaris"))
skip_if_not_installed("mclogit")

test_that("ggpredict mclogit", {
  data(Transport, package = "mclogit")
  m <- mclogit::mclogit(
    cbind(resp, suburb) ~ distance + cost,
    data = Transport
  )
  out <- predict_response(m, "cost")

  expect_equal(
    out$predicted,
    c(0.5721, 0.42669, 0.00121),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  d <- data_grid(m, "cost")
  d$suburb <- 1
  d$resp <- 1
  expect_equal(
    out$predicted,
    predict(m, newdata = d, type = "response"),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
