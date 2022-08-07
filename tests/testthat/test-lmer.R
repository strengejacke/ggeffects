.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest) {

  if (suppressWarnings(
    require("testthat") &&
    require("ggeffects") &&
    require("sjlabelled") &&
    require("lme4") &&
    require("sjmisc")
  )) {
    # lmer ----

    data(efc)
    efc$grp <- to_label(efc$e15relat)
    fit <- lmer(neg_c_7 ~ c12hour + e42dep + c161sex + c172code + (1|grp), data = efc)

    test_that("ggpredict, lmer", {
      expect_s3_class(ggpredict(fit, "c12hour"), "data.frame")
      expect_s3_class(ggpredict(fit, c("c12hour", "c161sex")), "data.frame")
      expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
      expect_s3_class(ggpredict(fit, "c12hour", type = "re"), "data.frame")
      expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), type = "re"), "data.frame")
      expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "re"), "data.frame")
    })

    test_that("ggpredict, lmer", {
      pr <- ggpredict(fit, "c12hour")
      expect_equal(pr$std.error[1:5], c(0.2911, 0.2852, 0.2799, 0.2752, 0.2713), tolerance = 1e-3)
      pr <- ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "re")
      expect_equal(pr$std.error[1:5], c(3.5882, 3.58185, 3.58652, 3.58162, 3.57608), tolerance = 1e-3)
    })

    test_that("ggpredict, lmer-simulate", {
      expect_s3_class(ggpredict(fit, "c12hour", type = "sim"), "data.frame")
      expect_s3_class(ggpredict(fit, c("c12hour", "c161sex"), type = "sim"), "data.frame")
      expect_s3_class(ggpredict(fit, c("c12hour", "c161sex", "c172code"), type = "sim"), "data.frame")
    })

    test_that("ggeffect, lmer", {
      expect_s3_class(ggeffect(fit, "c12hour"), "data.frame")
      expect_s3_class(ggeffect(fit, c("c12hour", "c161sex")), "data.frame")
      expect_s3_class(ggeffect(fit, c("c12hour", "c161sex", "c172code")), "data.frame")
    })

    data(efc)

    efc$cluster <- as.factor(efc$e15relat)
    efc <- std(efc, c160age, e42dep)

    m <- lmer(
      neg_c_7 ~ c160age_z * e42dep_z + c161sex + (1 | cluster),
      data = efc
    )

    test_that("ggeffect, lmer", {
      p1 <- ggpredict(m, terms = c("c160age_z", "e42dep_z [-1.17,2.03]"))
      p2 <- ggemmeans(m, terms = c("c160age_z", "e42dep_z [-1.17,2.03]"))
      expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)
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
      expect_s3_class(ggpredict(m, terms = "e42dep"), "data.frame")
      expect_s3_class(ggemmeans(m, terms = "e42dep"), "data.frame")
    })

    test_that("ggeffect, lmer", {
      p1 <- ggpredict(m, terms = "e42dep")
      p2 <- ggemmeans(m, terms = "e42dep")
      p3 <- ggemmeans(m, terms = "e42dep", condition = c(c161sex = "Male", c172code = "low level of education"))
      expect_equal(p1$predicted[1], 8.902934, tolerance = 1e-3)
      expect_equal(p2$predicted[1], 9.742945, tolerance = 1e-3)
      expect_equal(p1$predicted[1], p3$predicted[1], tolerance = 1e-3)
    })

    m <- suppressWarnings(lmer(
      log(Reaction) ~ Days + I(Days^2) + (1 + Days + exp(Days) | Subject),
      data = sleepstudy
    ))

    test_that("ggeffect, lmer", {
      p1 <- ggpredict(m, terms = "Days")
      p2 <- ggemmeans(m, terms = "Days")
      p3 <- ggeffect(m, terms = "Days")
      expect_equal(p1$predicted[1], 253.5178, tolerance = 1e-3)
      expect_equal(p2$predicted[1], 253.5178, tolerance = 1e-3)
      expect_equal(p3$predicted[1], 5.535434, tolerance = 1e-3)
    })

    test_that("ggeffect, lmer", {
      expect_s3_class(ggpredict(m, terms = c("Days", "Subject [sample=5]"), type = "re"), "data.frame")
    })
  }

}
