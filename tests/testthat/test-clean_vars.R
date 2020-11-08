if (suppressWarnings(
  require("testthat") &&
  require("ggeffects")
)) {
  # lm, linear regression ----

  data(efc)
  efc$c172code <- as.factor(efc$c172code)
  efc$e42dep <- as.factor(efc$e42dep)
  fit <- lm(barthtot ~ c12hour + c172code + e42dep, data = efc)

  test_that("ggpredict, clean_vars", {
    expect_equal(nrow(ggpredict(fit, "c172code")), 3)
    expect_equal(nrow(ggpredict(fit, "c172code [1,3]")), 2)
    expect_equal(nrow(ggpredict(fit, "c172code[1,3]")), 2)
  })

  test_that("ggpredict, clean_vars", {
    expect_equal(nrow(ggemmeans(fit, "c172code")), 3)
    expect_equal(nrow(ggemmeans(fit, "c172code [1,3]")), 2)
    expect_equal(nrow(ggemmeans(fit, "c172code[1,3]")), 2)
  })

  test_that("ggpredict, clean_vars", {
    expect_equal(nrow(ggpredict(fit, "e42dep")), 4)
    expect_equal(nrow(ggpredict(fit, "e42dep [1,3]")), 2)
    expect_equal(nrow(ggpredict(fit, "e42dep[1,3]")), 2)
  })

  test_that("ggpredict, clean_vars", {
    expect_equal(nrow(ggpredict(fit, c("c172code", "e42dep"))), 12)
    expect_equal(nrow(ggpredict(fit, c("c172code [1,3]", "e42dep"))), 8)
    expect_equal(nrow(ggpredict(fit, c("c172code", "e42dep [1,3]"))), 6)
    expect_equal(nrow(ggpredict(fit, c("c172code [1,3]", "e42dep [1,3]"))), 4)
    expect_equal(nrow(ggpredict(fit, c("c172code[1,3]", "e42dep"))), 8)
    expect_equal(nrow(ggpredict(fit, c("c172code", "e42dep[1,3]"))), 6)
    expect_equal(nrow(ggpredict(fit, c("c172code[1,3]", "e42dep[1,3]"))), 4)
  })

  test_that("ggemmeans, clean_vars", {
    expect_equal(nrow(ggemmeans(fit, c("c172code", "e42dep"))), 12)
    expect_equal(nrow(ggemmeans(fit, c("c172code [1,3]", "e42dep"))), 8)
    expect_equal(nrow(ggemmeans(fit, c("c172code", "e42dep [1,3]"))), 6)
    expect_equal(nrow(ggemmeans(fit, c("c172code [1,3]", "e42dep [1,3]"))), 4)
    expect_equal(nrow(ggemmeans(fit, c("c172code[1,3]", "e42dep"))), 8)
    expect_equal(nrow(ggemmeans(fit, c("c172code", "e42dep[1,3]"))), 6)
    expect_equal(nrow(ggemmeans(fit, c("c172code[1,3]", "e42dep[1,3]"))), 4)
  })
}
