if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjlabelled") &&
  require("sjmisc")
)) {
  # lm, linear regression ----

  data(efc)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

  test_that("ggpredict, lm", {
    expect_s3_class(ggpredict(fit, "c12hour"), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex")), "data.frame")
  })

  test_that("ggpredict, lm print", {
    x <- ggpredict(fit, c("c12hour", "c161sex", "c172code"))
    out <- utils::capture.output(print(x))
    expect_equal(
      out,
      c("# Predicted values of Total score BARTHEL INDEX", "# x = average number of hours of care per week",
        "", "#  c161sex = Male", "# c172code = [1] low level of education",
        "", "  x | Predicted |         95% CI", "--------------------------------",
        "  0 |     73.95 | [69.35, 78.55]", " 45 |     62.56 | [58.23, 66.88]",
        " 85 |     52.42 | [47.90, 56.95]", "170 |     30.89 | [24.85, 36.94]",
        "", "#  c161sex = Female", "# c172code = [1] low level of education",
        "", "  x | Predicted |         95% CI", "--------------------------------",
        "  0 |     75.00 | [71.41, 78.59]", " 45 |     63.60 | [60.46, 66.74]",
        " 85 |     53.46 | [50.13, 56.80]", "170 |     31.93 | [26.83, 37.04]",
        "", "#  c161sex = Male", "# c172code = [2] intermediate level of education",
        "", "  x | Predicted |         95% CI", "--------------------------------",
        "  0 |     74.67 | [71.06, 78.29]", " 45 |     63.27 | [59.88, 66.66]",
        " 85 |     53.14 | [49.40, 56.89]", "170 |     31.61 | [25.98, 37.24]",
        "", "#  c161sex = Female", "# c172code = [2] intermediate level of education",
        "", "  x | Predicted |         95% CI", "--------------------------------",
        "  0 |     75.71 | [73.31, 78.12]", " 45 |     64.32 | [62.42, 66.21]",
        " 85 |     54.18 | [51.81, 56.55]", "170 |     32.65 | [27.94, 37.36]",
        "", "#  c161sex = Male", "# c172code = [3] high level of education",
        "", "  x | Predicted |         95% CI", "--------------------------------",
        "  0 |     75.39 | [71.04, 79.74]", " 45 |     63.99 | [59.73, 68.26]",
        " 85 |     53.86 | [49.23, 58.49]", "170 |     32.33 | [25.95, 38.71]",
        "", "#  c161sex = Female", "# c172code = [3] high level of education",
        "", "  x | Predicted |         95% CI", "--------------------------------",
        "  0 |     76.43 | [72.89, 79.98]", " 45 |     65.03 | [61.68, 68.39]",
        " 85 |     54.90 | [51.16, 58.65]", "170 |     33.37 | [27.70, 39.05]",
        "", "Adjusted for:", "* neg_c_7 = 11.84")
    )
    x <- ggpredict(fit, c("c12hour", "c161sex", "neg_c_7"))
    out <- utils::capture.output(print(x))
    expect_equal(
      out,
      c("# Predicted values of Total score BARTHEL INDEX", "# x = average number of hours of care per week",
        "", "# c161sex = Male", "# neg_c_7 = 11.8", "", "  x | Predicted |         95% CI",
        "--------------------------------", "  0 |     74.74 | [71.12, 78.36]",
        " 45 |     63.34 | [59.95, 66.73]", " 85 |     53.21 | [49.46, 56.95]",
        "170 |     31.68 | [26.05, 37.30]", "", "# c161sex = Female",
        "# neg_c_7 = 11.8", "", "  x | Predicted |         95% CI", "--------------------------------",
        "  0 |     75.78 | [73.38, 78.18]", " 45 |     64.38 | [62.49, 66.28]",
        " 85 |     54.25 | [51.89, 56.61]", "170 |     32.72 | [28.01, 37.42]",
        "", "# c161sex = Male", "# neg_c_7 = 15.7", "", "  x | Predicted |         95% CI",
        "--------------------------------", "  0 |     65.78 | [61.54, 70.02]",
        " 45 |     54.38 | [50.50, 58.26]", " 85 |     44.25 | [40.20, 48.29]",
        "170 |     22.72 | [17.11, 28.33]", "", "# c161sex = Female",
        "# neg_c_7 = 15.7", "", "  x | Predicted |         95% CI", "--------------------------------",
        "  0 |     66.82 | [63.70, 69.94]", " 45 |     55.42 | [52.94, 57.91]",
        " 85 |     45.29 | [42.65, 47.93]", "170 |     23.76 | [19.18, 28.34]",
        "", "# c161sex = Male", "# neg_c_7 = 8", "", "  x | Predicted |         95% CI",
        "--------------------------------", "  0 |     83.47 | [79.72, 87.21]",
        " 45 |     72.07 | [68.36, 75.78]", " 85 |     61.94 | [57.76, 66.11]",
        "170 |     40.41 | [34.28, 46.54]", "", "# c161sex = Female",
        "# neg_c_7 = 8", "", "  x | Predicted |         95% CI", "--------------------------------",
        "  0 |     84.51 | [81.75, 87.27]", " 45 |     73.11 | [70.51, 75.71]",
        " 85 |     62.98 | [59.83, 66.13]", "170 |     41.45 | [36.07, 46.83]",
        "", "Adjusted for:", "* c172code = 1.97")
    )
  })

  test_that("ggpredict, lm by", {
    expect_equal(nrow(ggpredict(fit, "c12hour [10:20]")), 11)
    expect_equal(nrow(ggpredict(fit, "c12hour [10:20 by=.2]")), 51)
    expect_equal(nrow(ggpredict(fit, "c12hour [10:20 by = .2]")), 51)
    expect_equal(nrow(ggpredict(fit, "c12hour [10:20by=.2]")), 51)
  })

  test_that("ggpredict, lm-vcov", {
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), vcov.fun = "vcovHC", vcov.type = "HC1"), "data.frame")
  })

  test_that("ggpredict, lm-prediction-interval", {
    pr <- ggpredict(fit, c("c12hour", "c161sex"), interval = "predict")
    expect_equal(pr$conf.low[1], 64.38495, tolerance = 1e-4)
    pr <- ggpredict(fit, c("c12hour", "c161sex"), interval = "conf")
    expect_equal(pr$conf.low[1], 71.02894, tolerance = 1e-4)
    pr <- ggpredict(fit, c("c12hour", "c161sex"), interval = "predict", vcov.fun = "vcovHC", vcov.type = "HC1")
    expect_equal(pr$conf.low[1], 64.42971, tolerance = 1e-4)

    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), interval = "predict", ci.lvl = NA), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), interval = "conf", ci.lvl = NA), "data.frame")
  })

  test_that("ggpredict, lm-noci", {
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), ci.lvl = NA), "data.frame")
  })

  test_that("ggpredict, lm, ci.lvl", {
    expect_s3_class(ggpredict(fit, "c12hour", ci.lvl = .8), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), ci.lvl = .8), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), ci.lvl = .8), "data.frame")
  })

  test_that("ggpredict, lm, typical", {
    expect_s3_class(ggpredict(fit, "c12hour", ci.lvl = .8, typical = "median"), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), ci.lvl = .8, typical = "median"), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), ci.lvl = .8, typical = "median"), "data.frame")
  })

  test_that("ggpredict, lm, condition", {
    expect_s3_class(ggpredict(fit, "c172code", condition = c(c12hour = 40), ci.lvl = .8, typical = "median"), "data.frame")
    expect_s3_class(ggpredict(fit, c("c172code", "c161sex"), condition = c(c12hour = 40), ci.lvl = .8, typical = "median"), "data.frame")
  })

  test_that("ggpredict, lm, pretty", {
    expect_s3_class(ggpredict(fit, "c12hour", full.data = TRUE, ci.lvl = .8, typical = "median"), "data.frame")
    expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), full.data = TRUE, ci.lvl = .8, typical = "median"), "data.frame")
  })

  test_that("ggpredict, lm, full.data", {
    expect_s3_class(ggpredict(fit, "c172code", full.data = TRUE, ci.lvl = .8, typical = "median"), "data.frame")
    expect_s3_class(ggpredict(fit, c("c172code", "c161sex"), full.data = TRUE, ci.lvl = .8, typical = "median"), "data.frame")
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

  test_that("ggemmeans, lm, ci.lvl", {
    expect_s3_class(ggemmeans(fit, "c12hour", ci.lvl = .8), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex"), ci.lvl = .8), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex", "c172code"), ci.lvl = .8), "data.frame")
  })

  test_that("ggemmeans, lm, typical", {
    expect_s3_class(ggemmeans(fit, "c12hour", ci.lvl = .8, typical = "median"), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex"), ci.lvl = .8, typical = "median"), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex", "c172code"), ci.lvl = .8, typical = "median"), "data.frame")
  })

  test_that("ggemmeans, lm, condition", {
    expect_s3_class(ggemmeans(fit, "c172code", condition = c(c12hour = 40), ci.lvl = .8, typical = "median"), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c172code", "c161sex"), condition = c(c12hour = 40), ci.lvl = .8, typical = "median"), "data.frame")
  })

  test_that("ggemmeans, lm, pretty", {
    expect_s3_class(ggemmeans(fit, "c12hour", full.data = TRUE, ci.lvl = .8, typical = "median"), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex"), full.data = TRUE, ci.lvl = .8, typical = "median"), "data.frame")
  })




  data(efc)
  efc$c172code <- to_label(efc$c172code)
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
  })

  test_that("ggemmeans, lm", {
    expect_s3_class(ggemmeans(fit, "c12hour [meansd]"), "data.frame")
    expect_s3_class(ggemmeans(fit, "c12hour [minmax]"), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour [quart]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour [zeromax]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
    expect_s3_class(ggemmeans(fit, c("c12hour [quart2]", "c161sex", "c172code [high level of education,low level of education]")), "data.frame")
  })


  data(efc)
  efc$c172code <- to_label(efc$c172code)
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

  data(efc)
  fit <- lm(barthtot ~ c12hour + neg_c_7, data = efc)

  test_that("ggemmeans, lm", {
    p1 <- ggemmeans(fit, "neg_c_7")
    p2 <- ggeffect(fit, "neg_c_7")
    p3 <- ggpredict(fit, "neg_c_7")
    expect_equal(p1$predicted[1], 78.2641, tolerance = 1e-3)
    expect_equal(p2$predicted[1], 78.2641, tolerance = 1e-3)
    expect_equal(p3$predicted[1], 78.2641, tolerance = 1e-3)
  })

  test_that("ggemmeans, lm", {
    p1 <- ggemmeans(fit, "neg_c_7 [5,10]")
    p2 <- ggeffect(fit, "neg_c_7 [5,10]")
    p3 <- ggpredict(fit, "neg_c_7 [5,10]")
    expect_equal(p1$predicted[1], 80.58504, tolerance = 1e-3)
    expect_equal(p2$predicted[1], 80.58504, tolerance = 1e-3)
    expect_equal(p3$predicted[1], 80.58504, tolerance = 1e-3)
  })
}
