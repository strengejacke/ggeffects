skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("glmmTMB")
skip_if_not_installed("pscl")
skip_if_not_installed("marginaleffects")

test_that("ggpredict, print zero-inflated, glmmTMB", {
  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson(),
    data = Salamanders
  )

  out <- predict_response(m, "mined")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")
  out <- predict_response(m, "mined", margin = "empirical")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")

  set.seed(123)
  out <- predict_response(m, "mined", type = "zero_inflated")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")
  set.seed(123)
  out <- predict_response(m, "mined", type = "zero_inflated", margin = "empirical")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")

  out <- predict_response(m, "mined", type = "zi_prob")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")
  out <- predict_response(m, "mined", type = "zi_prob", margin = "empirical")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")
})


test_that("ggpredict, print zero-inflated, pscl", {
  data(Salamanders, package = "glmmTMB")
  m <- pscl::zeroinfl(count ~ mined | mined, data = Salamanders)

  out <- predict_response(m, "mined")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")
  out <- predict_response(m, "mined", margin = "empirical")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")

  set.seed(123)
  out <- predict_response(m, "mined", type = "zero_inflated")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")
  set.seed(123)
  out <- predict_response(m, "mined", type = "zero_inflated", margin = "empirical")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")

  out <- predict_response(m, "mined", type = "zi_prob")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")
  out <- predict_response(m, "mined", type = "zi_prob", margin = "empirical")
  expect_snapshot(print(out), variant = "windows")
  expect_snapshot(print(test_predictions(out)), variant = "windows")
})
