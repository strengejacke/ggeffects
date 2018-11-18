if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjmisc")
)) {
  context("ggeffects, print")

  # lm, linear regression ----

  data(efc)
  efc$c172code <- to_label(efc$c172code)
  efc$e42dep <- to_label(efc$e42dep)
  efc$c82cop1 <- as.numeric(efc$c82cop1)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c82cop1 + e42dep + c161sex + c172code, data = efc)

  test_that("ggpredict, print", {
    ggpredict(fit, terms = "c12hour")
    ggpredict(fit, terms = "c172code")
    ggpredict(fit, terms = "c161sex")
    ggpredict(fit, terms = c("c12hour", "c172code"))
    ggpredict(fit, terms = c("c12hour", "c161sex"))
    ggpredict(fit, terms = c("e42dep", "c161sex"))
    ggpredict(fit, terms = c("e42dep", "c172code"))
    ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
    ggpredict(fit, terms = c("e42dep", "c172code", "c161sex"))
    ggpredict(fit, terms = c("c12hour", "c172code", "e42dep"))
    ggpredict(fit, terms = c("c161sex", "c172code", "e42dep"))
    ggpredict(fit, terms = c("c12hour", "neg_c_7"))
    ggpredict(fit, terms = c("c12hour", "neg_c_7 [all]"))
    ggpredict(fit, terms = c("c12hour", "neg_c_7 [quart2]"))
    ggpredict(fit, terms = c("c12hour", "neg_c_7 [quart2]", "c161sex"))
    ggpredict(fit, terms = c("c12hour", "neg_c_7", "c161sex"))
    ggpredict(fit, terms = c("c12hour", "neg_c_7 [quart2]", "c82cop1"))
    ggpredict(fit, terms = c("c12hour", "neg_c_7", "c82cop1"))
  })
}
