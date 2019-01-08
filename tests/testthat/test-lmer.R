if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjlabelled") &&
  require("lme4") &&
  require("sjmisc")
)) {
  context("ggeffects, lmer")

  # lmer ----

  data(efc)
  efc$grp = to_label(efc$e15relat)
  fit <- lmer(neg_c_7 ~ c12hour + e42dep + c161sex + c172code + (1|grp), data = efc)

  test_that("ggpredict, lmer", {
    ggpredict(fit, "c12hour")
    ggpredict(fit, c("c12hour", "c161sex"))
    ggpredict(fit, c("c12hour", "c161sex", "c172code"))
    ggpredict(fit, "c12hour", type = "re")
    ggpredict(fit, c("c12hour", "c161sex"), type = "re")
    ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "re")
  })

  test_that("ggpredict, lmer-simulate", {
    ggpredict(fit, "c12hour", type = "sim")
    ggpredict(fit, c("c12hour", "c161sex"), type = "sim")
    ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "sim")
  })

  test_that("ggaverage, lmer", {
    ggaverage(fit, "c12hour")
    ggaverage(fit, c("c12hour", "c161sex"))
    ggaverage(fit, c("c12hour", "c161sex", "c172code"))
  })

  test_that("ggeffect, lmer", {
    ggeffect(fit, "c12hour")
    ggeffect(fit, c("c12hour", "c161sex"))
    ggeffect(fit, c("c12hour", "c161sex", "c172code"))
  })

  data(efc)

  efc$cluster <- as.factor(efc$e15relat)
  efc <- std(efc, c160age, e42dep)

  m <- lmer(
    neg_c_7 ~ c160age_z * e42dep_z + c161sex + (1 | cluster),
    data = efc)

  test_that("ggeffect, lmer", {
    ggpredict(m, terms = c("c160age_z", "e42dep_z [-1.17,2.03]"))
  })


  data(efc)
  efc$cluster <- as.factor(efc$e15relat)
  efc <- as_label(efc, e42dep, c172code, c161sex)
  efc$c172code[efc$c172code == "intermediate level of education"] <- NA

  m <- lmer(
    neg_c_7 ~ c172code + e42dep + c161sex + (1 | cluster),
    data = efc
  )

  test_that("ggeffect, lmer", {
    ggpredict(m, terms = "e42dep")
  })
}
