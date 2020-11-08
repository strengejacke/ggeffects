if (suppressWarnings(
  require("testthat") &&
  require("ggeffects")
)) {
  # glm, poisson regression ----
  data(efc)
  fit <- glm(tot_sc_e ~ neg_c_7 + c12hour + e42dep + c161sex + c172code, data = efc, family = poisson(link = "log"))

  test_that("ggpredict, glm", {
    ggpredict(fit, "c12hour")
    ggpredict(fit, c("c12hour", "c161sex"))
    ggpredict(fit, c("c12hour", "c161sex", "c172code"))
  })

  test_that("ggeffect, glm", {
    ggeffect(fit, "c12hour")
    ggeffect(fit, c("c12hour", "c161sex"))
    ggeffect(fit, c("c12hour", "c161sex", "c172code"))
  })
}
