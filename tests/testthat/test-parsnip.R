skip_on_os(c("mac", "solaris"))
skip_if_not_installed("effects")
skip_if_not_installed("emmeans")
skip_if_not_installed("parsnip")
skip_if_not_installed("sjlabelled")

# lm, linear regression ----
data(efc, package = "ggeffects")
fit <- parsnip::fit(parsnip::linear_reg(), barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

test_that("validate ggpredict parsnip against predict", {
  nd <- data_grid(fit, "c12hour [10, 50, 100]")
  pr <- predict(fit, new_data = nd)
  predicted <- ggpredict(fit, "c12hour [10, 50, 100]")
  expect_equal(predicted$predicted, pr[[".pred"]], tolerance = 1e-3, ignore_attr = TRUE)
})

test_that("ggpredict, parsnip print", {
  x <- ggpredict(fit, c("c12hour", "c161sex", "c172code"))
  out <- utils::capture.output(print(x, verbose = FALSE))
  expect_identical(
    out,
    c(
      "# Predicted values of Total score BARTHEL INDEX", "", "#  c161sex = Male",
      "# c172code = [1] low level of education", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     73.95 | [69.35, 78.56]",
      "     45 |     62.56 | [58.22, 66.89]", "     85 |     52.42 | [47.89, 56.96]",
      "    170 |     30.89 | [24.84, 36.95]", "", "#  c161sex = Female",
      "# c172code = [1] low level of education", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     75.00 | [71.40, 78.59]",
      "     45 |     63.60 | [60.45, 66.74]", "     85 |     53.46 | [50.12, 56.80]",
      "    170 |     31.93 | [26.82, 37.05]", "", "#  c161sex = Male",
      "# c172code = [2] intermediate level of education", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     74.67 | [71.05, 78.29]",
      "     45 |     63.27 | [59.88, 66.67]", "     85 |     53.14 | [49.39, 56.89]",
      "    170 |     31.61 | [25.97, 37.25]", "", "#  c161sex = Female",
      "# c172code = [2] intermediate level of education", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     75.71 | [73.31, 78.12]",
      "     45 |     64.32 | [62.41, 66.22]", "     85 |     54.18 | [51.81, 56.56]",
      "    170 |     32.65 | [27.94, 37.37]", "", "#  c161sex = Male",
      "# c172code = [3] high level of education", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     75.39 | [71.03, 79.75]",
      "     45 |     63.99 | [59.72, 68.26]", "     85 |     53.86 | [49.22, 58.50]",
      "    170 |     32.33 | [25.94, 38.72]", "", "#  c161sex = Female",
      "# c172code = [3] high level of education", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     76.43 | [72.88, 79.98]",
      "     45 |     65.03 | [61.67, 68.39]", "     85 |     54.90 | [51.15, 58.65]",
      "    170 |     33.37 | [27.69, 39.05]", "", "Adjusted for:",
      "* neg_c_7 = 11.84"
    )
  )
  x <- ggpredict(fit, c("c12hour", "c161sex", "neg_c_7"), verbose = FALSE)
  out <- utils::capture.output(print(x))
  expect_identical(
    out,
    c(
      "# Predicted values of Total score BARTHEL INDEX", "", "# c161sex = Male",
      "# neg_c_7 = 11.8", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     74.74 | [71.11, 78.36]",
      "     45 |     63.34 | [59.94, 66.74]", "     85 |     53.21 | [49.46, 56.96]",
      "    170 |     31.68 | [26.04, 37.31]", "", "# c161sex = Female",
      "# neg_c_7 = 11.8", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     75.78 | [73.38, 78.19]",
      "     45 |     64.38 | [62.48, 66.28]", "     85 |     54.25 | [51.88, 56.62]",
      "    170 |     32.72 | [28.01, 37.43]", "", "# c161sex = Male",
      "# neg_c_7 = 15.7", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     65.78 | [61.53, 70.03]",
      "     45 |     54.38 | [50.49, 58.27]", "     85 |     44.25 | [40.20, 48.30]",
      "    170 |     22.72 | [17.10, 28.33]", "", "# c161sex = Female",
      "# neg_c_7 = 15.7", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     66.82 | [63.70, 69.94]",
      "     45 |     55.42 | [52.93, 57.91]", "     85 |     45.29 | [42.65, 47.94]",
      "    170 |     23.76 | [19.17, 28.34]", "", "# c161sex = Male",
      "# neg_c_7 = 8", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     83.47 | [79.72, 87.22]",
      "     45 |     72.07 | [68.36, 75.78]", "     85 |     61.94 | [57.76, 66.12]",
      "    170 |     40.41 | [34.27, 46.55]", "", "# c161sex = Female",
      "# neg_c_7 = 8", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     84.51 | [81.74, 87.27]",
      "     45 |     73.11 | [70.51, 75.72]", "     85 |     62.98 | [59.82, 66.14]",
      "    170 |     41.45 | [36.06, 46.84]", "", "Adjusted for:",
      "* c172code = 1.97"
    )
  )
})

test_that("ggemmeans, parsnip", {
  expect_s3_class(ggemmeans(fit, "c12hour [meansd]"), "data.frame")
  expect_s3_class(ggemmeans(fit, "c12hour [minmax]"), "data.frame")
})

test_that("test_predictions, parsnip", {
  skip_on_os("linux")
  out <- test_predictions(fit, "c172code")
  expect_equal(out$Slope, 0.71836, tolerance = 1e-3)
  expect_equal(out$conf.low, -1.928975, tolerance = 1e-3)
})
