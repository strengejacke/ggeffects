skip_on_cran()
skip_if_not_installed("haven")
skip_if_not_installed("datawizard")

test_that("ggpredict, print", {
  # lm, linear regression ----

  data(efc, package = "ggeffects")
  efc$c172code <- datawizard::to_factor(efc$c172code)
  efc$e42dep <- datawizard::to_factor(efc$e42dep)
  efc$c82cop1 <- as.numeric(efc$c82cop1)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c82cop1 + e42dep + c161sex + c172code, data = efc)

  expect_message({
    junk <- capture.output(print(ggpredict(fit, terms = "c12hour")))
  })
  expect_silent({
    junk <- capture.output(print(ggpredict(fit, terms = "c12hour"), n = Inf))
  })
  ggpredict(fit, terms = "c172code")
  ggpredict(fit, terms = "c161sex")
  ggpredict(fit, terms = c("c12hour", "c172code"))
  ggpredict(fit, terms = c("c12hour", "c161sex"))
  ggpredict(fit, terms = c("e42dep", "c161sex"))
  ggpredict(fit, terms = c("e42dep", "c172code"))
  ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
  ggpredict(fit, terms = c("e42dep", "c172code", "c161sex"))
  ggpredict(fit, terms = c("c12hour", "c172code", "e42dep"))
  ggpredict(fit, terms = c("c161sex", "c172code", "e42dep"))
  ggpredict(fit, terms = c("c12hour", "neg_c_7"))
  ggpredict(fit, terms = c("c12hour", "neg_c_7 [all]"))
  ggpredict(fit, terms = c("c12hour", "neg_c_7 [quart2]"))
  ggpredict(fit, terms = c("c12hour", "neg_c_7 [quart2]", "c161sex"))
  ggpredict(fit, terms = c("c12hour", "neg_c_7", "c161sex"))

  expect_snapshot(print(ggpredict(fit, terms = c("c12hour", "neg_c_7", "c161sex"))))
  expect_snapshot(print(ggpredict(fit, terms = c("c12hour", "neg_c_7", "c161sex")), n = Inf))

  out <- utils::capture.output(ggpredict(fit, terms = c("c12hour", "neg_c_7 [quart2]", "c82cop1")))
  expect_equal(
    out,
    c(
      "# Predicted values of Total score BARTHEL INDEX", "", "# neg_c_7 = 9",
      "# c82cop1 = 1", "", "c12hour | Predicted |          95% CI",
      "-------------------------------------", "      0 |     95.03 | [87.81, 102.26]",
      "     45 |     91.98 | [84.67,  99.30]", "     85 |     89.28 | [81.71,  96.84]",
      "    170 |     83.52 | [74.96,  92.08]", "", "# neg_c_7 = 11",
      "# c82cop1 = 1", "", "c12hour | Predicted |          95% CI",
      "-------------------------------------", "      0 |     93.03 | [85.95, 100.11]",
      "     45 |     89.98 | [82.82,  97.14]", "     85 |     87.27 | [79.87,  94.68]",
      "    170 |     81.52 | [73.11,  89.92]", "", "# neg_c_7 = 14",
      "# c82cop1 = 1", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     90.02 | [83.03, 97.01]",
      "     45 |     86.98 | [79.92, 94.03]", "     85 |     84.27 | [76.98, 91.56]",
      "    170 |     78.51 | [70.24, 86.78]", "", "# neg_c_7 = 9",
      "# c82cop1 = 2", "", "c12hour | Predicted |          95% CI",
      "-------------------------------------", "      0 |     94.45 | [88.65, 100.24]",
      "     45 |     91.40 | [85.52,  97.28]", "     85 |     88.69 | [82.53,  94.86]",
      "    170 |     82.93 | [75.63,  90.24]", "", "# neg_c_7 = 11",
      "# c82cop1 = 2", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     92.44 | [86.73, 98.15]",
      "     45 |     89.40 | [83.62, 95.18]", "     85 |     86.69 | [80.63, 92.74]",
      "    170 |     80.93 | [73.74, 88.13]", "", "# neg_c_7 = 14",
      "# c82cop1 = 2", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     89.44 | [83.70, 95.18]",
      "     45 |     86.39 | [80.61, 92.18]", "     85 |     83.68 | [77.64, 89.73]",
      "    170 |     77.93 | [70.77, 85.08]", "", "# neg_c_7 = 9",
      "# c82cop1 = 3", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     93.86 | [88.88, 98.85]",
      "     45 |     90.82 | [85.77, 95.86]", "     85 |     88.11 | [82.76, 93.46]",
      "    170 |     82.35 | [75.76, 88.94]", "", "# neg_c_7 = 11",
      "# c82cop1 = 3", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     91.86 | [86.87, 96.85]",
      "     45 |     88.81 | [83.78, 93.85]", "     85 |     86.10 | [80.78, 91.43]",
      "    170 |     80.35 | [73.81, 86.89]", "", "# neg_c_7 = 14",
      "# c82cop1 = 3", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     88.86 | [83.67, 94.04]",
      "     45 |     85.81 | [80.61, 91.01]", "     85 |     83.10 | [77.64, 88.56]",
      "    170 |     77.34 | [70.72, 83.96]", "", "# neg_c_7 = 9",
      "# c82cop1 = 4", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     93.28 | [88.18, 98.37]",
      "     45 |     90.23 | [85.11, 95.35]", "     85 |     87.52 | [82.13, 92.91]",
      "    170 |     81.77 | [75.20, 88.34]", "", "# neg_c_7 = 11",
      "# c82cop1 = 4", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     91.28 | [86.07, 96.48]",
      "     45 |     88.23 | [83.02, 93.44]", "     85 |     85.52 | [80.06, 90.98]",
      "    170 |     79.76 | [73.16, 86.37]", "", "# neg_c_7 = 14",
      "# c82cop1 = 4", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     88.27 | [82.74, 93.80]",
      "     45 |     85.22 | [79.70, 90.74]", "     85 |     82.52 | [76.78, 88.25]",
      "    170 |     76.76 | [69.96, 83.56]", "", "Adjusted for:",
      "*   e42dep =            independent", "*  c161sex =                   1.76",
      "* c172code = low level of education"
    ),
    ignore_attr = TRUE
  )

  out <- utils::capture.output(ggpredict(fit, terms = c("c12hour", "neg_c_7", "c82cop1")))
  expect_equal(
    out,
    c(
      "# Predicted values of Total score BARTHEL INDEX", "", "# neg_c_7 = 8",
      "# c82cop1 = 1", "", "c12hour | Predicted |          95% CI",
      "-------------------------------------", "      0 |     96.03 | [88.71, 103.35]",
      "     45 |     92.99 | [85.57, 100.40]", "     85 |     90.28 | [82.61,  97.95]",
      "    170 |     84.52 | [75.86,  93.18]", "", "# neg_c_7 = 11.8",
      "# c82cop1 = 1", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     92.23 | [85.19, 99.27]",
      "     45 |     89.18 | [82.06, 96.29]", "     85 |     86.47 | [79.11, 93.83]",
      "    170 |     80.71 | [72.36, 89.07]", "", "# neg_c_7 = 15.7",
      "# c82cop1 = 1", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     88.32 | [81.31, 95.33]",
      "     45 |     85.27 | [78.21, 92.34]", "     85 |     82.57 | [75.28, 89.85]",
      "    170 |     76.81 | [68.55, 85.06]", "", "# neg_c_7 = 8",
      "# c82cop1 = 2", "", "c12hour | Predicted |          95% CI",
      "-------------------------------------", "      0 |     95.45 | [89.58, 101.32]",
      "     45 |     92.40 | [86.44,  98.36]", "     85 |     89.69 | [83.44,  95.94]",
      "    170 |     83.94 | [76.55,  91.33]", "", "# neg_c_7 = 11.8",
      "# c82cop1 = 2", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     91.64 | [85.94, 97.34]",
      "     45 |     88.60 | [82.83, 94.36]", "     85 |     85.89 | [79.85, 91.92]",
      "    170 |     80.13 | [72.96, 87.30]", "", "# neg_c_7 = 15.7",
      "# c82cop1 = 2", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     87.74 | [81.90, 93.58]",
      "     45 |     84.69 | [78.81, 90.57]", "     85 |     81.98 | [75.86, 88.10]",
      "    170 |     76.22 | [69.02, 83.42]", "", "# neg_c_7 = 8",
      "# c82cop1 = 3", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     94.86 | [89.84, 99.89]",
      "     45 |     91.82 | [86.73, 96.91]", "     85 |     89.11 | [83.71, 94.50]",
      "    170 |     83.35 | [76.72, 89.99]", "", "# neg_c_7 = 11.8",
      "# c82cop1 = 3", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     91.06 | [86.04, 96.08]",
      "     45 |     88.01 | [82.95, 93.07]", "     85 |     85.30 | [79.96, 90.64]",
      "    170 |     79.55 | [73.00, 86.09]", "", "# neg_c_7 = 15.7",
      "# c82cop1 = 3", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     87.15 | [81.77, 92.53]",
      "     45 |     84.11 | [78.72, 89.49]", "     85 |     81.40 | [75.77, 87.02]",
      "    170 |     75.64 | [68.90, 82.38]", "", "# neg_c_7 = 8",
      "# c82cop1 = 4", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     94.28 | [89.20, 99.36]",
      "     45 |     91.23 | [86.12, 96.34]", "     85 |     88.52 | [83.14, 93.91]",
      "    170 |     82.77 | [76.19, 89.35]", "", "# neg_c_7 = 11.8",
      "# c82cop1 = 4", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     90.47 | [85.20, 95.75]",
      "     45 |     87.43 | [82.15, 92.70]", "     85 |     84.72 | [79.20, 90.24]",
      "    170 |     78.96 | [72.32, 85.60]", "", "# neg_c_7 = 15.7",
      "# c82cop1 = 4", "", "c12hour | Predicted |         95% CI",
      "------------------------------------", "      0 |     86.57 | [80.77, 92.36]",
      "     45 |     83.52 | [77.75, 89.29]", "     85 |     80.81 | [74.84, 86.78]",
      "    170 |     75.06 | [68.08, 82.04]", "", "Adjusted for:",
      "*   e42dep =            independent", "*  c161sex =                   1.76",
      "* c172code = low level of education"
    ),
    ignore_attr = TRUE
  )
})

test_that("ggpredict, print factors", {
  LEV <- c(
    "climate", "cutwelfare", "discipline", "freedom", "ineqincOK", "leader",
    "police", "politduty", "refugees", "Russia", "taxesdown", "worse-off"
  )
  n <- 100
  set.seed(1)
  data <- data.frame(
    bin_choice = sample(c(0, 1), size = n, replace = TRUE),
    Wshort = factor(sample(LEV, size = n, replace = TRUE), levels = LEV)
  )
  model.contcons <- glm(bin_choice ~ Wshort, data = data, family = binomial())

  pr <- ggemmeans(model.contcons, "Wshort [all]")
  expect_snapshot(print(pr))

  pr <- ggemmeans(model.contcons, "Wshort")
  expect_snapshot(print(pr))
})
