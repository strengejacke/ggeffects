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


  data(Owls)
  m <- suppressWarnings(glmer.nb(SiblingNegotiation ~ SexParent + ArrivalTime + (1 | Nest), data = Owls))

  test_that("ggpredict, glmer.nb", {
    ggpredict(m, "SexParent")
    ggpredict(m, "SexParent", type = "re")
    ggpredict(m, c("SexParent", "ArrivalTime"))
    ggpredict(m, c("SexParent", "ArrivalTime"), type = "re")
    ggemmeans(m, "SexParent")
    ggemmeans(m, c("SexParent", "ArrivalTime"))
  })

  test_that("ggpredict, glmer.nb-simulate", {
    ggpredict(m, "SexParent", type = "sim")
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
}
