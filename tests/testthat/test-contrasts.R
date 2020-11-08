if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("lme4") &&
  require("sjlabelled")
)) {
  data(efc)

  efc$e15relat <- as_label(efc$e15relat)
  efc$e42dep <- as_label(efc$e42dep)
  efc$c172code <- as.factor(efc$c172code)

  m <- lmer(neg_c_7 ~ e42dep + c172code + c12hour + c82cop1 + (1 | e15relat), data = efc)

  test_that("ggpredict, contrasts-1", {
    options(contrasts = rep("contr.sum", 2))
    pr <- ggpredict(m, c("c12hour", "e42dep"))
    expect_false(anyNA(pr$std.error))
  })

  test_that("ggpredict, contrasts-2", {
    options(contrasts = rep("contr.sum", 2))
    pr <- ggpredict(m, c("c12hour", "c172code"))
    expect_false(anyNA(pr$std.error))
  })

  test_that("ggpredict, contrasts-3", {
    options(contrasts = rep("contr.sum", 2))
    pr <- ggpredict(m, c("c12hour", "c82cop1"))
    expect_false(anyNA(pr$std.error))
  })

  test_that("ggpredict, contrasts-4", {
    options(contrasts = rep("contr.treatment", 2))
    pr <- ggpredict(m, c("c12hour", "e42dep"))
    expect_false(anyNA(pr$std.error))
  })

  test_that("ggpredict, contrasts-5", {
    options(contrasts = rep("contr.treatment", 2))
    pr <- ggpredict(m, c("c12hour", "c172code"))
    expect_false(anyNA(pr$std.error))
  })

  test_that("ggpredict, contrasts-6", {
    options(contrasts = rep("contr.treatment", 2))
    pr <- ggpredict(m, c("c12hour", "c82cop1"))
    expect_false(anyNA(pr$std.error))
  })
}
