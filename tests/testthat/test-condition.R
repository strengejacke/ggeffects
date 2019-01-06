if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjmisc")
)) {
  context("ggeffects, condition")

  data(efc)
  efc$e42dep <- to_label(efc$e42dep)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + e42dep + c172code, data = efc)

  test_that("ggpredict, condition", {
    ggpredict(fit, "c172code")
    ggpredict(fit, "c172code", condition = c(c12hour = 40))
    ggpredict(fit, "c172code", condition = c(c12hour = 40, e42dep = "severely dependent"))
    ggpredict(fit, "c172code", condition = c(e42dep = "severely dependent"))
  })

  efc$neg_c_7d <- dicho(efc$neg_c_7)

  m1 <- glm(
    neg_c_7d ~ c12hour + e42dep + c161sex + c172code,
    data = efc,
    family = binomial(link = "logit")
  )

  test_that("ggpredict, glm", {
    ggpredict(m1, "c12hour", condition = c(e42dep = "severely dependent"))
    ggpredict(m1, c("c12hour", "c161sex"), condition = c(e42dep = "severely dependent"))
    ggpredict(m1, c("c12hour", "c161sex", "c172code"), condition = c(e42dep = "severely dependent"))
  })


  data(efc)
  efc$neg_c_7d <- dicho(efc$neg_c_7)

  m2 <- glm(
    neg_c_7d ~ c12hour + e42dep + c161sex + c172code,
    data = efc,
    family = binomial(link = "logit")
  )

  test_that("ggpredict, glm", {
    ggpredict(m2, "c12hour", condition = c(c172code = 1))
    ggpredict(m2, c("c12hour", "c161sex"), condition = c(c172code = 2))
  })

}
