stopifnot(require("testthat"),
          require("ggeffects"),
          require("sjlabelled"),
          require("sjmisc"))

context("ggeffects, linear model")

# lm, linear regression ----

data(efc)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

test_that("ggpredict, lm", {
  ggpredict(fit, "c12hour")
  ggpredict(fit, c("c12hour", "c161sex"))
  ggpredict(fit, c("c12hour", "c161sex", "c172code"))
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
