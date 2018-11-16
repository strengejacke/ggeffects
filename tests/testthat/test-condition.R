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
}
