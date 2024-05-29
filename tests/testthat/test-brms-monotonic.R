skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("brms")

test_that("ggpredict, brms-mo()", {
  m <- suppressWarnings(insight::download_model("brms_mo1"))
  skip_if(is.null(m))

  out <- predict_response(m, "income")
  expect_equal(out$predicted, c(30.98621, 59.47271, 70.79805, 73.5196), tolerance = 1e-3)
  expect_identical(as.character(out$x), c("below_20", "20_to_40", "40_to_100", "greater_100"))
})

skip_if_not_installed("withr")
skip_if_not_installed("vdiffr")

withr::with_environment(
  new.env(),
  test_that("plot brms monotonic", {
    m <- suppressWarnings(insight::download_model("brms_mo1"))
    skip_if(is.null(m))
    set.seed(123)
    vdiffr::expect_doppelganger(
      "plot brms monotonic",
      suppressWarnings(plot(predict_response(m, "income"), show_data = TRUE, jitter = TRUE))
    )
  })
)
