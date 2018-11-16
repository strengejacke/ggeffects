if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjmisc") &&
  require("MASS")
)) {
  context("ggeffects, negbin model")

  data(efc)
  efc$e42dep <- to_label(efc$e42dep)
  fit <-
    glm.nb(
      tot_sc_e ~ neg_c_7 + I(neg_c_7 ^ 2) + neg_c_7:e42dep + I(neg_c_7 ^ 2):e42dep + c12hour + c172code,
      data = efc,
      init.theta = 1.133641349,
      link = log
    )

  test_that("ggpredict, negbin", {
    ggpredict(fit, "neg_c_7")
    ggeffect(fit, "neg_c_7")
    ggpredict(fit, c("neg_c_7", "e42dep"))
    ggeffect(fit, c("neg_c_7", "e42dep"))
  })

  data(efc)
  fit <-
    glm.nb(
      tot_sc_e ~ neg_c_7 + I(neg_c_7 ^ 2) + neg_c_7:e42dep + I(neg_c_7 ^ 2):e42dep + c12hour + c172code,
      data = efc,
      init.theta = 1.133641349,
      link = log
    )

  test_that("ggpredict, negbin", {
    ggpredict(fit, "neg_c_7")
    ggeffect(fit, "neg_c_7")
    ggpredict(fit, c("neg_c_7", "e42dep"))
    ggeffect(fit, c("neg_c_7", "e42dep"))
  })
}
