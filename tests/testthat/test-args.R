if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjmisc")
)) {
  context("ggeffects, arguments")

  # lm, linear regression ----

  data(efc)
  efc$c172code <- to_label(efc$c172code)
  efc$e42dep <- to_label(efc$e42dep)
  fit <- lm(barthtot ~ c12hour + c172code + e42dep, data = efc)

  test_that("ggpredict, args", {
    ggpredict(fit, "c12hour", x.as.factor = TRUE)
    ggpredict(fit, "c172code", x.as.factor = TRUE)
    ggpredict(fit, "c172code", x.as.factor = FALSE)
  })

  test_that("ggpredict, args", {
    p1 <- ggpredict(fit, "c172code", x.as.factor = TRUE)
    p2 <- ggpredict(fit, "c172code", x.as.factor = FALSE)
    expect_equal(p1$predicted, p2$predicted)
  })

  test_that("ggpredict, args", {
    p1 <- ggpredict(fit, "e42dep", x.as.factor = TRUE)
    p2 <- ggpredict(fit, "e42dep", x.as.factor = FALSE)
    expect_equal(p1$predicted, p2$predicted)

    p1 <- ggpredict(fit, c("c172code", "e42dep"), x.as.factor = TRUE)
    p2 <- ggpredict(fit, c("c172code", "e42dep"), x.as.factor = FALSE)
    expect_equal(p1$predicted, p2$predicted)

    p1 <- ggpredict(fit, c("e42dep", "c172code"), x.as.factor = TRUE)
    p2 <- ggpredict(fit, c("e42dep", "c172code"), x.as.factor = FALSE)
    expect_equal(p1$predicted, p2$predicted)
  })
}
