if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjlabelled") &&
  require("sjmisc")
)) {
  context("ggeffects, linear model")

  # lm, linear regression ----

  data(efc)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

  test_that("ggpredict, lm", {
    ggpredict(fit, "c12hour")
    ggpredict(fit, c("c12hour", "c161sex"))
    x <- ggpredict(fit, c("c12hour", "c161sex", "c172code"))
    print(x)
    x <- ggpredict(fit, c("c12hour", "c161sex", "neg_c_7"))
    print(x)
  })

  test_that("ggpredict, lm-vcov", {
    ggpredict(fit, c("c12hour", "c161sex"), vcov.fun = "vcovHC", vcov.type = "HC1")
  })

  test_that("ggpredict, lm-noci", {
    ggpredict(fit, c("c12hour", "c161sex"), ci.lvl = NA)
  })

  test_that("ggpredict, lm, ci.lvl", {
    ggpredict(fit, "c12hour", ci.lvl = .8)
    ggpredict(fit, c("c12hour", "c161sex"), ci.lvl = .8)
    ggpredict(fit, c("c12hour", "c161sex", "c172code"), ci.lvl = .8)
  })

  test_that("ggpredict, lm, typical", {
    ggpredict(fit, "c12hour", ci.lvl = .8, typical = "median")
    ggpredict(fit, c("c12hour", "c161sex"), ci.lvl = .8, typical = "median")
    ggpredict(fit, c("c12hour", "c161sex", "c172code"), ci.lvl = .8, typical = "median")
  })

  test_that("ggpredict, lm, x.as.factor", {
    ggpredict(fit, "c172code", ci.lvl = .8, typical = "median", x.as.factor = TRUE)
    ggpredict(fit, c("c172code", "c161sex"), ci.lvl = .8, typical = "median", x.as.factor = TRUE)
  })

  test_that("ggpredict, lm, condition", {
    ggpredict(fit, "c172code", condition = c(c12hour = 40), ci.lvl = .8, typical = "median", x.as.factor = TRUE)
    ggpredict(fit, c("c172code", "c161sex"), condition = c(c12hour = 40), ci.lvl = .8, typical = "median", x.as.factor = TRUE)
  })

  test_that("ggpredict, lm, pretty", {
    ggpredict(fit, "c12hour", full.data = TRUE, ci.lvl = .8, typical = "median", x.as.factor = TRUE)
    ggpredict(fit, c("c12hour", "c161sex"), full.data = TRUE, ci.lvl = .8, typical = "median", x.as.factor = TRUE)
  })

  test_that("ggpredict, lm, full.data", {
    ggpredict(fit, "c172code", full.data = TRUE, ci.lvl = .8, typical = "median", x.as.factor = TRUE)
    ggpredict(fit, c("c172code", "c161sex"), full.data = TRUE, ci.lvl = .8, typical = "median", x.as.factor = TRUE)
  })

  test_that("ggaverage, lm", {
    ggaverage(fit, "c12hour")
    ggaverage(fit, c("c12hour", "c161sex"))
    ggaverage(fit, c("c12hour", "c161sex", "c172code"))
  })

  test_that("ggeffect, lm", {
    ggeffect(fit, "c12hour")
    ggeffect(fit, c("c12hour", "c161sex"))
    ggeffect(fit, c("c12hour", "c161sex", "c172code"))
  })

  test_that("ggemmeans, lm", {
    ggemmeans(fit, "c12hour")
    ggemmeans(fit, c("c12hour", "c161sex"))
    ggemmeans(fit, c("c12hour", "c161sex", "c172code"))
  })

  test_that("ggemmeans, lm, ci.lvl", {
    ggemmeans(fit, "c12hour", ci.lvl = .8)
    ggemmeans(fit, c("c12hour", "c161sex"), ci.lvl = .8)
    ggemmeans(fit, c("c12hour", "c161sex", "c172code"), ci.lvl = .8)
  })

  test_that("ggemmeans, lm, typical", {
    ggemmeans(fit, "c12hour", ci.lvl = .8, typical = "median")
    ggemmeans(fit, c("c12hour", "c161sex"), ci.lvl = .8, typical = "median")
    ggemmeans(fit, c("c12hour", "c161sex", "c172code"), ci.lvl = .8, typical = "median")
  })

  test_that("ggemmeans, lm, x.as.factor", {
    ggemmeans(fit, "c172code", ci.lvl = .8, typical = "median", x.as.factor = TRUE)
    ggemmeans(fit, c("c172code", "c161sex"), ci.lvl = .8, typical = "median", x.as.factor = TRUE)
  })

  test_that("ggemmeans, lm, condition", {
    ggemmeans(fit, "c172code", condition = c(c12hour = 40), ci.lvl = .8, typical = "median", x.as.factor = TRUE)
    ggemmeans(fit, c("c172code", "c161sex"), condition = c(c12hour = 40), ci.lvl = .8, typical = "median", x.as.factor = TRUE)
  })

  test_that("ggemmeans, lm, pretty", {
    ggemmeans(fit, "c12hour", full.data = TRUE, ci.lvl = .8, typical = "median", x.as.factor = TRUE)
    ggemmeans(fit, c("c12hour", "c161sex"), full.data = TRUE, ci.lvl = .8, typical = "median", x.as.factor = TRUE)
  })




  data(efc)
  efc$c172code <- to_label(efc$c172code)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

  test_that("ggpredict, lm", {
    ggpredict(fit, "c12hour [20,30,40]")
    ggpredict(fit, "c12hour [30:60]")
    ggpredict(fit, c("c12hour  [30:60]", "c161sex", "c172code [high level of education,low level of education]"))
  })

  test_that("ggpredict, lm", {
    ggpredict(fit, "c12hour [meansd]")
    ggpredict(fit, "c12hour [minmax]")
    ggpredict(fit, c("c12hour [quart]", "c161sex", "c172code [high level of education,low level of education]"))
    ggpredict(fit, c("c12hour [zeromax]", "c161sex", "c172code [high level of education,low level of education]"))
    ggpredict(fit, c("c12hour [quart2]", "c161sex", "c172code [high level of education,low level of education]"))
  })


  test_that("ggeffect, lm", {
    ggeffect(fit, "c12hour [20,30,40]")
    ggeffect(fit, "c12hour [30:60]")
    ggeffect(fit, c("c12hour  [30:60]", "c161sex", "c172code [high level of education,low level of education]"))
  })

  test_that("ggeffect, lm", {
    ggeffect(fit, "c12hour [meansd]")
    ggeffect(fit, "c12hour [minmax]")
    ggeffect(fit, c("c12hour [quart]", "c161sex", "c172code [high level of education,low level of education]"))
    ggeffect(fit, c("c12hour [zeromax]", "c161sex", "c172code [high level of education,low level of education]"))
    ggeffect(fit, c("c12hour [quart2]", "c161sex", "c172code [high level of education,low level of education]"))
  })


  test_that("ggemmeans, lm", {
    ggemmeans(fit, "c12hour [20,30,40]")
    ggemmeans(fit, "c12hour [30:60]")
    ggemmeans(fit, c("c12hour  [30:60]", "c161sex", "c172code [high level of education,low level of education]"))
  })

  test_that("ggemmeans, lm", {
    ggemmeans(fit, "c12hour [meansd]")
    ggemmeans(fit, "c12hour [minmax]")
    ggemmeans(fit, c("c12hour [quart]", "c161sex", "c172code [high level of education,low level of education]"))
    ggemmeans(fit, c("c12hour [zeromax]", "c161sex", "c172code [high level of education,low level of education]"))
    ggemmeans(fit, c("c12hour [quart2]", "c161sex", "c172code [high level of education,low level of education]"))
  })


  data(efc)
  efc$c172code <- to_label(efc$c172code)
  fit <- lm(barthtot ~ log(c12hour) + c161sex + c172code, data = efc)

  test_that("ggpredict, lm, log", {
    ggpredict(fit, "c12hour [meansd]")
    ggpredict(fit, "c12hour [minmax]")
    ggpredict(fit, c("c12hour", "c172code [high level of education,low level of education]"))
    ggpredict(fit, c("c12hour [exp]", "c172code [high level of education,low level of education]"))
  })


  test_that("ggeffect, lm, log", {
    ggeffect(fit, "c12hour [meansd]")
    ggeffect(fit, "c12hour [minmax]")
    ggeffect(fit, c("c12hour", "c172code [high level of education,low level of education]"))
    ggeffect(fit, c("c12hour [exp]", "c172code [high level of education,low level of education]"))
  })

  test_that("ggeffect, lm, no_space", {
    ggeffect(fit, "c12hour[meansd]")
    ggeffect(fit, "c12hour[minmax]")
    ggeffect(fit, c("c12hour", "c172code[high level of education,low level of education]"))
    ggeffect(fit, c("c12hour[exp]", "c172code[high level of education,low level of education]"))
  })


  test_that("ggemmeans, lm, log", {
    ggemmeans(fit, "c12hour [meansd]")
    ggemmeans(fit, "c12hour [minmax]")
    ggemmeans(fit, c("c12hour", "c172code [high level of education,low level of education]"))
    ggemmeans(fit, c("c12hour [exp]", "c172code [high level of education,low level of education]"))
  })

  test_that("ggemmeans, lm, no_space", {
    ggemmeans(fit, "c12hour[meansd]")
    ggemmeans(fit, "c12hour[minmax]")
    ggemmeans(fit, c("c12hour", "c172code[high level of education,low level of education]"))
    ggemmeans(fit, c("c12hour[exp]", "c172code[high level of education,low level of education]"))
  })


  test_that("ggpredict, lm formula", {
    ggpredict(fit, ~ c12hour)
    ggpredict(fit, ~ c12hour + c161sex)
    ggpredict(fit, ~ c12hour + c161sex + c172code)
  })


  d <- subset(efc, select = c(barthtot, c12hour, neg_c_7, c172code))
  d <- na.omit(d)
  m1 <- lm(barthtot ~ c12hour + poly(neg_c_7, 2) + c172code, data = d)
  m2 <- lm(barthtot ~ c12hour + poly(neg_c_7, 3, raw = TRUE) + c172code, data = d)
  m3 <- lm(barthtot ~ scale(c12hour) + poly(neg_c_7, 2) + c172code, data = d)

  test_that("ggpredict, lm", {
    ggpredict(m1, "neg_c_7")
    ggpredict(m2, "neg_c_7")
    ggpredict(m3, "neg_c_7")
    ggpredict(m3, "c12hour")
  })

  test_that("ggemmeans, lm", {
    ggemmeans(m1, "neg_c_7")
    ggemmeans(m2, "neg_c_7")
    ggemmeans(m3, "neg_c_7")
    ggemmeans(m3, "c12hour")
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
