.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest) {
  if (suppressWarnings(
    require("testthat") &&
    require("ggeffects") &&
    require("lme4") &&
    require("glmmTMB")
  )) {

    # glmer ----

    data(efc_test)
    fit <- glmer(
      negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
        data = efc_test,
        family = binomial(link = "logit")
      )

    test_that("ggpredict, glmer", {
      pr <- ggpredict(fit, "c12hour")
      expect_equivalent(
        pr$predicted,
        c(0.34217, 0.34406, 0.34596, 0.34787, 0.34978, 0.3517, 0.35362,
          0.35554, 0.35747, 0.35941, 0.36135, 0.36329, 0.36524, 0.36719,
          0.36915, 0.37111, 0.37307, 0.37504, 0.37702, 0.37899, 0.38098,
          0.38296, 0.38495, 0.38694, 0.38894, 0.39094, 0.39295, 0.39496,
          0.39697, 0.39898, 0.401, 0.40302, 0.40505, 0.40708, 0.40911),
        tolerance = 1e-3)
      expect_s3_class(ggpredict(fit, "c12hour"), "data.frame")
      expect_s3_class(ggpredict(fit, c("c12hour", "c161sex")), "data.frame")
      expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
      expect_s3_class(ggpredict(fit, "c12hour", type = "re"), "data.frame")
      expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), type = "re"), "data.frame")
      expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "re"), "data.frame")
    })

    test_that("ggeffect, glmer", {
      pr <- ggeffect(fit, "c12hour")
      expect_equivalent(
        pr$predicted,
        c(0.34217, 0.34406, 0.34596, 0.34787, 0.34978, 0.3517, 0.35362,
          0.35554, 0.35747, 0.35941, 0.36135, 0.36329, 0.36524, 0.36719,
          0.36915, 0.37111, 0.37307, 0.37504, 0.37702, 0.37899, 0.38098,
          0.38296, 0.38495, 0.38694, 0.38894, 0.39094, 0.39295, 0.39496,
          0.39697, 0.39898, 0.401, 0.40302, 0.40505, 0.40708, 0.40911),
        tolerance = 1e-3)
      expect_s3_class(ggeffect(fit, "c12hour"), "data.frame")
      expect_s3_class(ggeffect(fit, c("c12hour", "c161sex")), "data.frame")
      expect_s3_class(ggeffect(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
    })

    test_that("ggemmeans, glmer", {
      pr <- ggemmeans(fit, "c12hour")
      expect_equivalent(
        pr$predicted,
        c(0.34217, 0.34406, 0.34596, 0.34787, 0.34978, 0.3517, 0.35362,
          0.35554, 0.35747, 0.35941, 0.36135, 0.36329, 0.36524, 0.36719,
          0.36915, 0.37111, 0.37307, 0.37504, 0.37702, 0.37899, 0.38098,
          0.38296, 0.38495, 0.38694, 0.38894, 0.39094, 0.39295, 0.39496,
          0.39697, 0.39898, 0.401, 0.40302, 0.40505, 0.40708, 0.40911),
        tolerance = 1e-3)
      expect_s3_class(ggemmeans(fit, "c12hour"), "data.frame")
      expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex")), "data.frame")
      expect_s3_class(ggemmeans(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
    })



    m <- insight::download_model("merMod_5")
    dd <<- insight::get_data(m)

    test_that("ggpredict, glmer.nb", {
      expect_s3_class(ggpredict(m, "f1"), "data.frame")
      expect_s3_class(ggpredict(m, "f1", type = "re"), "data.frame")
      expect_s3_class(ggpredict(m, c("f1", "f2")), "data.frame")
      expect_s3_class(ggpredict(m, c("f1", "f2"), type = "re"), "data.frame")
      expect_message(ggemmeans(m, "f1"))
      expect_s3_class(ggemmeans(m, c("f1", "f2")), "data.frame")
    })

    test_that("ggpredict, glmer.nb-simulate", {
      expect_s3_class(ggpredict(m, c("f1", "f2"), type = "sim"), "data.frame")
    })



    data(cbpp)
    cbpp$trials <- cbpp$size - cbpp$incidence

    m1 <- glmer(cbind(incidence, trials) ~ period + (1 | herd), data = cbpp, family = binomial)
    m2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), data = cbpp, family = binomial)

    test_that("ggpredict, glmer, cbind", {
      expect_s3_class(ggpredict(m1, "period"), "data.frame")
      expect_s3_class(ggpredict(m2, "period"), "data.frame")
      expect_s3_class(ggpredict(m1, "period", type = "re"), "data.frame")
      expect_s3_class(ggpredict(m2, "period", type = "re"), "data.frame")
      expect_s3_class(ggemmeans(m1, "period"), "data.frame")
      expect_s3_class(ggemmeans(m2, "period"), "data.frame")
    })

    test_that("compare, glmer, cbind", {
      p1 <- ggpredict(m1, "period")
      p2 <- ggemmeans(m1, "period")
      expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)
    })
  }
}
