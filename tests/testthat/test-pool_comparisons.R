skip_on_cran()
skip_if_not_installed("mice")
skip_if_not_installed("marginaleffects")

test_that("ggeffects, pool comparisons", {
  data("nhanes2", package = "mice")
  set.seed(123)
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  comparisons <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    hypothesis_test(m, "age")
  })
  pool_com <- pool_comparisons(comparisons)
  expect_equal(pool_com$Contrast, c(4.6464, 6.70639, 2.05999), tolerance = 1e-3)
  expect_equal(pool_com$conf.low, c(-0.37721, -1.3018, -4.41723), tolerance = 1e-3)
})
