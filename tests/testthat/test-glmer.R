.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest) {
  if (suppressWarnings(
    require("testthat") &&
    require("ggeffects") &&
    require("lme4") &&
    require("glmmTMB")
  )) {
    context("ggeffects, glmer")

    # glmer ----

    data(efc_test)
    fit <- glmer(
      negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
        data = efc_test,
        family = binomial(link = "logit")
      )

    test_that("ggpredict, glmer", {
      ggpredict(fit, "c12hour")
      ggpredict(fit, c("c12hour", "c161sex"))
      ggpredict(fit, c("c12hour", "c161sex", "c172code"))
      ggpredict(fit, "c12hour", type = "re")
      ggpredict(fit, c("c12hour", "c161sex"), type = "re")
      ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "re")
    })

    test_that("ggaverage, glmer", {
      ggaverage(fit, "c12hour")
      ggaverage(fit, c("c12hour", "c161sex"))
      ggaverage(fit, c("c12hour", "c161sex", "c172code"))
    })

    test_that("ggeffect, glmer", {
      ggeffect(fit, "c12hour")
      ggeffect(fit, c("c12hour", "c161sex"))
      ggeffect(fit, c("c12hour", "c161sex", "c172code"))
    })

    test_that("ggemmeans, glmer", {
      ggemmeans(fit, "c12hour")
      ggemmeans(fit, c("c12hour", "c161sex"))
      ggemmeans(fit, c("c12hour", "c161sex", "c172code"))
    })


    m <- insight::download_model("merMod_5")
    dd <- insight::get_data(m)

    test_that("ggpredict, glmer.nb", {
      ggpredict(m, "f1")
      ggpredict(m, "f1", type = "re")
      ggpredict(m, c("f1", "f2"))
      ggpredict(m, c("f1", "f2"), type = "re")
      ggemmeans(m, "f1")
      ggemmeans(m, c("f1", "f2"))
    })

    test_that("compare, glmer.nb", {
      p1 <- ggpredict(m, c("f1", "f2"))
      p2 <- ggemmeans(m, c("f1", "f2"))
      expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)
    })

    test_that("ggpredict, glmer.nb-simulate", {
      ggpredict(m, c("f1", "f2"), type = "sim")
    })


    data(cbpp)
    cbpp$trials <- cbpp$size - cbpp$incidence

    m1 <- glmer(cbind(incidence, trials) ~ period + (1 | herd), data = cbpp, family = binomial)
    m2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), data = cbpp, family = binomial)

    test_that("ggpredict, glmer, cbind", {
      ggpredict(m1, "period")
      ggpredict(m2, "period")
      ggpredict(m1, "period", type = "re")
      ggpredict(m2, "period", type = "re")
      ggemmeans(m1, "period")
      ggemmeans(m2, "period")
    })

    test_that("compare, glmer, cbind", {
      p1 <- ggpredict(m1, "period")
      p2 <- ggemmeans(m1, "period")
      expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)
    })
  }
}
