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

  data(Owls)
  m <- glmer.nb(SiblingNegotiation ~ SexParent + ArrivalTime + (1 | Nest), data = Owls)

  test_that("ggpredict, glmer.nb", {
    ggpredict(m, "SexParent")
    ggpredict(m, "SexParent", type = "re")
    ggpredict(m, c("SexParent", "ArrivalTime"))
    ggpredict(m, c("SexParent", "ArrivalTime"), type = "re")
  })
}
