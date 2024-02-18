skip_on_os(c("mac", "solaris"))
skip_if_not_installed("effects")
skip_if_not_installed("emmeans")
skip_if_not_installed("sjlabelled")

# lm, linear regression ----

data(efc, package = "ggeffects")
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

test_that("validate ggpredict lm against predict", {
  nd <- data_grid(fit, "c12hour [10, 50, 100]")
  pr <- predict(fit, newdata = nd, se.fit = TRUE)
  expected <- pr$fit + stats::qt(0.975, df.residual(fit)) * pr$se.fit
  # works with "ggpredict()"
  predicted <- ggpredict(fit, "c12hour [10, 50, 100]")
  expect_equal(predicted$conf.high, expected, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(predicted$predicted, pr$fit, tolerance = 1e-3, ignore_attr = TRUE)
  # works with "predict_response()"
  predicted2 <- predict_response(fit, "c12hour [10, 50, 100]")
  expect_equal(predicted2$conf.high, expected, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(predicted2$predicted, pr$fit, tolerance = 1e-3, ignore_attr = TRUE)
  # predict_response() and ggpredict() should be identical
  expect_identical(predicted, predicted2)
})

test_that("ggpredict, lm", {
  expect_s3_class(ggpredict(fit, "c12hour"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex")), "data.frame")
})

test_that("ggpredict, lm print", {
  x <- ggpredict(fit, c("c12hour", "c161sex", "c172code"))
  out <- utils::capture.output(print(x))
  expect_identical(
    out,
    c(
      "# Predicted values of Total score BARTHEL INDEX", "", "c161sex: Male",
      "c172code: [1] low level of education", "", "c12hour | Predicted |       95% CI",
      "----------------------------------", "      0 |     75.00 | 71.40, 78.59",
      "     45 |     63.60 | 60.45, 66.74", "     85 |     53.46 | 50.12, 56.80",
      "    170 |     31.93 | 26.82, 37.05", "", "c161sex: Male", "c172code: [2] intermediate level of education",
      "", "c12hour | Predicted |       95% CI", "----------------------------------",
      "      0 |     75.71 | 73.31, 78.12", "     45 |     64.32 | 62.41, 66.22",
      "     85 |     54.18 | 51.81, 56.56", "    170 |     32.65 | 27.94, 37.37",
      "", "c161sex: Male", "c172code: [3] high level of education",
      "", "c12hour | Predicted |       95% CI", "----------------------------------",
      "      0 |     76.43 | 72.88, 79.98", "     45 |     65.03 | 61.67, 68.39",
      "     85 |     54.90 | 51.15, 58.65", "    170 |     33.37 | 27.69, 39.05",
      "", "c161sex: Female", "c172code: [1] low level of education",
      "", "c12hour | Predicted |       95% CI", "----------------------------------",
      "      0 |     73.95 | 69.35, 78.56", "     45 |     62.56 | 58.22, 66.89",
      "     85 |     52.42 | 47.89, 56.96", "    170 |     30.89 | 24.84, 36.95",
      "", "c161sex: Female", "c172code: [2] intermediate level of education",
      "", "c12hour | Predicted |       95% CI", "----------------------------------",
      "      0 |     74.67 | 71.05, 78.29", "     45 |     63.27 | 59.88, 66.67",
      "     85 |     53.14 | 49.39, 56.89", "    170 |     31.61 | 25.97, 37.25",
      "", "c161sex: Female", "c172code: [3] high level of education",
      "", "c12hour | Predicted |       95% CI", "----------------------------------",
      "      0 |     75.39 | 71.03, 79.75", "     45 |     63.99 | 59.72, 68.26",
      "     85 |     53.86 | 49.22, 58.50", "    170 |     32.33 | 25.94, 38.72",
      "", "Adjusted for:", "* neg_c_7 = 11.84"
    )
  )
  x <- ggpredict(fit, c("c12hour", "c161sex", "neg_c_7"))
  out <- utils::capture.output(print(x))
  expect_identical(
    out,
    c(
      "# Predicted values of Total score BARTHEL INDEX", "", "c161sex: Male",
      "neg_c_7: 8", "", "c12hour | Predicted |       95% CI", "----------------------------------",
      "      0 |     75.78 | 73.38, 78.19", "     45 |     64.38 | 62.48, 66.28",
      "     85 |     54.25 | 51.88, 56.62", "    170 |     32.72 | 28.01, 37.43",
      "", "c161sex: Male", "neg_c_7: 11.8", "", "c12hour | Predicted |       95% CI",
      "----------------------------------", "      0 |     66.82 | 63.70, 69.94",
      "     45 |     55.42 | 52.93, 57.91", "     85 |     45.29 | 42.65, 47.94",
      "    170 |     23.76 | 19.17, 28.34", "", "c161sex: Male", "neg_c_7: 15.7",
      "", "c12hour | Predicted |       95% CI", "----------------------------------",
      "      0 |     84.51 | 81.74, 87.27", "     45 |     73.11 | 70.51, 75.72",
      "     85 |     62.98 | 59.82, 66.14", "    170 |     41.45 | 36.06, 46.84",
      "", "c161sex: Female", "neg_c_7: 8", "", "c12hour | Predicted |       95% CI",
      "----------------------------------", "      0 |     74.74 | 71.11, 78.36",
      "     45 |     63.34 | 59.94, 66.74", "     85 |     53.21 | 49.46, 56.96",
      "    170 |     31.68 | 26.04, 37.31", "", "c161sex: Female",
      "neg_c_7: 11.8", "", "c12hour | Predicted |       95% CI", "----------------------------------",
      "      0 |     65.78 | 61.53, 70.03", "     45 |     54.38 | 50.49, 58.27",
      "     85 |     44.25 | 40.20, 48.30", "    170 |     22.72 | 17.10, 28.33",
      "", "c161sex: Female", "neg_c_7: 15.7", "", "c12hour | Predicted |       95% CI",
      "----------------------------------", "      0 |     83.47 | 79.72, 87.22",
      "     45 |     72.07 | 68.36, 75.78", "     85 |     61.94 | 57.76, 66.12",
      "    170 |     40.41 | 34.27, 46.55", "", "Adjusted for:", "* c172code = 1.97"
    )
  )
})

test_that("ggpredict, lm by", {
  expect_identical(nrow(ggpredict(fit, "c12hour [10:20]")), 11L)
  expect_identical(nrow(ggpredict(fit, "c12hour [10:20 by=.2]")), 51L)
  expect_identical(nrow(ggpredict(fit, "c12hour [10:20 by = .2]")), 51L)
  expect_identical(nrow(ggpredict(fit, "c12hour [10:20by=.2]")), 51L)
})

test_that("ggpredict, lm-vcov", {
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), vcov_fun = "vcovHC", vcov_type = "HC1"), "data.frame")
})

test_that("ggpredict, lm-prediction-interval", {
  pr <- ggpredict(fit, c("c12hour", "c161sex"), interval = "predict")
  expect_equal(pr$conf.low[1], 27.36046, tolerance = 1e-4)
  pr <- ggpredict(fit, c("c12hour", "c161sex"), interval = "conf")
  expect_equal(pr$conf.low[1], 71.0235294, tolerance = 1e-4)
  pr <- ggpredict(fit, c("c12hour", "c161sex"), interval = "predict", vcov_fun = "vcovHC", vcov_type = "HC1")
  expect_equal(pr$conf.low[1], 27.37019, tolerance = 1e-4)

  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), interval = "predict", ci_level = NA), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), interval = "conf", ci_level = NA), "data.frame")
})

test_that("ggpredict, lm-noci", {
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), ci_level = NA), "data.frame")
})

test_that("ggpredict, lm, ci_level", {
  expect_s3_class(ggpredict(fit, "c12hour", ci_level = 0.8), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), ci_level = 0.8), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), ci_level = 0.8), "data.frame")
})

test_that("ggpredict, lm, typical", {
  expect_s3_class(ggpredict(fit, "c12hour", ci_level = 0.8, typical = "median"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), ci_level = 0.8, typical = "median"), "data.frame")
  expect_s3_class(
    ggpredict(fit, c("c12hour", "c161sex", "c172code"), ci_level = 0.8, typical = "median"),
    "data.frame"
  )
})

test_that("ggpredict, lm, condition", {
  expect_s3_class(
    ggpredict(fit, "c172code", condition = c(c12hour = 40), ci_level = 0.8, typical = "median"),
    "data.frame"
  )
  expect_s3_class(
    ggpredict(
      fit,
      c("c172code", "c161sex"),
      condition = c(c12hour = 40),
      ci_level = 0.8,
      typical = "median"
    ),
    "data.frame"
  )
})

test_that("ggpredict, lm, pretty", {
  expect_s3_class(
    ggpredict(fit, "c12hour", full.data = TRUE, ci_level = 0.8, typical = "median"),
    "data.frame"
  )
  expect_s3_class(
    ggpredict(fit, c("c12hour", "c161sex"), full.data = TRUE, ci_level = 0.8, typical = "median"),
    "data.frame"
  )
})

test_that("ggpredict, lm, full.data", {
  expect_s3_class(ggpredict(fit, "c172code", full.data = TRUE, ci_level = 0.8, typical = "median"), "data.frame")
  expect_s3_class(
    ggpredict(fit, c("c172code", "c161sex"), full.data = TRUE, ci_level = 0.8, typical = "median"),
    "data.frame"
  )
})

test_that("ggeffect, lm", {
  expect_s3_class(ggeffect(fit, "c12hour"), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c161sex")), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
})

test_that("ggemmeans, lm", {
  expect_s3_class(ggemmeans(fit, "c12hour"), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex")), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
})

test_that("ggemmeans, lm, ci_level", {
  expect_s3_class(ggemmeans(fit, "c12hour", ci_level = 0.8), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex"), ci_level = 0.8), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex", "c172code"), ci_level = 0.8), "data.frame")
})

test_that("ggemmeans, lm, typical", {
  expect_s3_class(ggemmeans(fit, "c12hour", ci_level = 0.8, typical = "median"), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex"), ci_level = 0.8, typical = "median"), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex", "c172code"), ci_level = 0.8, typical = "median"), "data.frame")
})

test_that("ggemmeans, lm, condition", {
  expect_s3_class(ggemmeans(fit, "c172code", condition = c(c12hour = 40), ci_level = 0.8, typical = "median"), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c172code", "c161sex"), condition = c(c12hour = 40), ci_level = 0.8, typical = "median"), "data.frame")
})

test_that("ggemmeans, lm, pretty", {
  expect_s3_class(ggemmeans(fit, "c12hour", full.data = TRUE, ci_level = 0.8, typical = "median"), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex"), full.data = TRUE, ci_level = 0.8, typical = "median"), "data.frame")
})


data(efc, package = "ggeffects")
efc$c172code <- sjlabelled::to_label(efc$c172code)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

test_that("ggpredict, lm", {
  expect_s3_class(ggpredict(fit, "c12hour [20,30,40]"), "data.frame")
  expect_s3_class(ggpredict(fit, "c12hour [30:60]"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour  [30:60]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
})

test_that("ggpredict, lm", {
  expect_s3_class(ggpredict(fit, "c12hour [meansd]"), "data.frame")
  expect_s3_class(ggpredict(fit, "c12hour [minmax]"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour [quart]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour [zeromax]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour [quart2]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
})


test_that("ggeffect, lm", {
  expect_s3_class(ggeffect(fit, "c12hour [20,30,40]"), "data.frame")
  expect_s3_class(ggeffect(fit, "c12hour [30:60]"), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour  [30:60]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
})

test_that("ggeffect, lm", {
  expect_s3_class(ggeffect(fit, "c12hour [meansd]"), "data.frame")
  expect_s3_class(ggeffect(fit, "c12hour [minmax]"), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour [quart]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour [zeromax]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour [quart2]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
})


test_that("ggemmeans, lm", {
  expect_s3_class(ggemmeans(fit, "c12hour [20,30,40]"), "data.frame")
  expect_s3_class(ggemmeans(fit, "c12hour [30:60]"), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour  [30:60]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
  out1 <- ggemmeans(fit, "c12hour [20,30,40]")
  out2 <- emmeans::emmeans(
    fit,
    "c12hour",
    at = list(c12hour = c(20, 30, 40),
              c161sex = mean(efc$c161sex, na.rm = TRUE),
              neg_c_7 = mean(efc$neg_c_7, na.rm = TRUE))
  )
  expect_equal(out1$predicted, as.data.frame(out2)$emmean, tolerance = 1e-1)
  # predict_response() works
  out3 <- predict_response(fit, "c12hour [20,30,40]", margin = "marginalmeans")
  expect_equal(out1$predicted, out3$predicted, tolerance = 1e-1)
  # ggemmeans() and predict_response() should be identical
  expect_identical(out1, out3)
})

test_that("ggemmeans, lm", {
  expect_s3_class(ggemmeans(fit, "c12hour [meansd]"), "data.frame")
  expect_s3_class(ggemmeans(fit, "c12hour [minmax]"), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour [quart]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour [zeromax]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour [quart2]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
})


data(efc, package = "ggeffects")
efc$c172code <- sjlabelled::to_label(efc$c172code)
fit <- lm(barthtot ~ log(c12hour) + c161sex + c172code, data = efc)

test_that("ggpredict, lm, log", {
  expect_warning(ggpredict(fit, "c12hour [meansd]"))
  expect_s3_class(ggpredict(fit, "c12hour [minmax]"), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour", "c172code [high level of education,low level of education]")), "data.frame")
  expect_s3_class(ggpredict(fit, c("c12hour [exp]", "c172code [high level of education,low level of education]")), "data.frame")
})


test_that("ggeffect, lm, log", {
  expect_s3_class(ggeffect(fit, "c12hour [meansd]"), "data.frame")
  expect_s3_class(ggeffect(fit, "c12hour [minmax]"), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c172code [high level of education,low level of education]")), "data.frame")
  expect_s3_class(suppressWarnings(ggeffect(fit, c("c12hour [exp]", "c172code [high level of education,low level of education]"))), "data.frame")
})

test_that("ggeffect, lm, no_space", {
  expect_s3_class(ggeffect(fit, "c12hour[meansd]"), "data.frame")
  expect_s3_class(ggeffect(fit, "c12hour[minmax]"), "data.frame")
  expect_s3_class(ggeffect(fit, c("c12hour", "c172code[high level of education,low level of education]")), "data.frame")
  expect_s3_class(suppressWarnings(ggeffect(fit, c("c12hour[exp]", "c172code[high level of education,low level of education]"))), "data.frame")
})


test_that("ggemmeans, lm, log", {
  expect_s3_class(ggemmeans(fit, "c12hour [meansd]"), "data.frame")
  expect_s3_class(ggemmeans(fit, "c12hour [minmax]"), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c172code [high level of education,low level of education]")), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour [exp]", "c172code [high level of education,low level of education]")), "data.frame")
})

test_that("ggemmeans, lm, no_space", {
  expect_s3_class(ggemmeans(fit, "c12hour[meansd]"), "data.frame")
  expect_s3_class(ggemmeans(fit, "c12hour[minmax]"), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour", "c172code[high level of education,low level of education]")), "data.frame")
  expect_s3_class(ggemmeans(fit, c("c12hour[exp]", "c172code[high level of education,low level of education]")), "data.frame")
})


test_that("ggpredict, lm formula", {
  expect_s3_class(ggpredict(fit, ~ c12hour), "data.frame")
  expect_s3_class(ggpredict(fit, ~ c12hour + c161sex), "data.frame")
  expect_s3_class(ggpredict(fit, ~ c12hour + c161sex + c172code), "data.frame")
})


d <- subset(efc, select = c(barthtot, c12hour, neg_c_7, c172code))
d <- na.omit(d)
m1 <- lm(barthtot ~ c12hour + poly(neg_c_7, 2) + c172code, data = d)
m2 <- lm(barthtot ~ c12hour + poly(neg_c_7, 3, raw = TRUE) + c172code, data = d)
m3 <- lm(barthtot ~ scale(c12hour) + poly(neg_c_7, 2) + c172code, data = d)

test_that("ggpredict, lm", {
  expect_s3_class(ggpredict(m1, "neg_c_7"), "data.frame")
  expect_s3_class(ggpredict(m2, "neg_c_7"), "data.frame")
  expect_s3_class(ggpredict(m3, "neg_c_7"), "data.frame")
  expect_s3_class(ggpredict(m3, "c12hour"), "data.frame")
})

test_that("ggemmeans, lm", {
  expect_s3_class(ggemmeans(m1, "neg_c_7"), "data.frame")
  expect_s3_class(ggemmeans(m2, "neg_c_7"), "data.frame")
  expect_s3_class(ggemmeans(m3, "neg_c_7"), "data.frame")
  expect_s3_class(ggemmeans(m3, "c12hour"), "data.frame")
})

data(efc, package = "ggeffects")
fit <- lm(barthtot ~ c12hour + neg_c_7, data = efc)

test_that("ggemmeans, lm", {
  p1 <- ggemmeans(fit, "neg_c_7")
  p2 <- ggeffect(fit, "neg_c_7")
  p3 <- ggpredict(fit, "neg_c_7")
  p4 <- predict_response(fit, "neg_c_7", margin = "marginalmeans")
  expect_equal(p1$predicted[1], 78.2641, tolerance = 1e-3)
  expect_equal(p2$predicted[1], 78.2641, tolerance = 1e-3)
  expect_equal(p3$predicted[1], 78.2641, tolerance = 1e-3)
  expect_equal(p4$predicted[1], p1$predicted[1], tolerance = 1e-3)
})

test_that("ggemmeans, lm", {
  p1 <- ggemmeans(fit, "neg_c_7 [5,10]")
  p2 <- ggeffect(fit, "neg_c_7 [5,10]")
  p3 <- ggpredict(fit, "neg_c_7 [5,10]")
  expect_equal(p1$predicted[1], 80.58504, tolerance = 1e-3)
  expect_equal(p2$predicted[1], 80.58504, tolerance = 1e-3)
  expect_equal(p3$predicted[1], 80.58504, tolerance = 1e-3)
})

skip_if_not_installed("marginaleffects")

test_that("ggaverage, lm", {
  data(efc, package = "ggeffects")
  fit <- lm(neg_c_7 ~ barthtot + grp + c12hour + nur_pst, data = efc)
  out1 <- ggaverage(fit, "nur_pst")
  out2 <- marginaleffects::avg_predictions(fit, variables = "nur_pst")
  expect_equal(out1$predicted, out2$estimate[order(out2$nur_pst)], tolerance = 1e-4)
})

test_that("difference in predictions identical", {
  data(efc, package = "ggeffects")
  fit <- lm(neg_c_7 ~ barthtot + grp + c12hour + nur_pst, data = efc)
  out1 <- predict_response(fit, "nur_pst", margin = "mean_reference")
  out2 <- predict_response(fit, "nur_pst", margin = "mean_mode")
  out3 <- predict_response(fit, "nur_pst", margin = "marginalmeans")
  out4 <- predict_response(fit, "nur_pst", margin = "empirical")
  expect_equal(diff(out1$predicted), diff(out2$predicted), tolerance = 1e-4)
  expect_equal(diff(out2$predicted), diff(out3$predicted), tolerance = 1e-4)
  expect_equal(diff(out3$predicted), diff(out4$predicted), tolerance = 1e-4)
})
