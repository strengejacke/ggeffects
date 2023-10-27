skip_on_cran()
skip_if_not_installed("emmeans")
# lm, linear regression ----

data(efc, package = "ggeffects")
efc$c172code <- as.factor(efc$c172code)
efc$e42dep <- as.factor(efc$e42dep)
fit <- lm(barthtot ~ c12hour + c172code + e42dep, data = efc)

test_that("ggpredict, clean_vars", {
  expect_identical(nrow(ggpredict(fit, "c172code")), 3L)
  expect_identical(nrow(ggpredict(fit, "c172code [1,3]")), 2L)
  expect_identical(nrow(ggpredict(fit, "c172code[1,3]")), 2L)
})

test_that("ggpredict, clean_vars", {
  expect_identical(nrow(ggemmeans(fit, "c172code")), 3L)
  expect_identical(nrow(ggemmeans(fit, "c172code [1,3]")), 2L)
  expect_identical(nrow(ggemmeans(fit, "c172code[1,3]")), 2L)
})

test_that("ggpredict, clean_vars", {
  expect_identical(nrow(ggpredict(fit, "e42dep")), 4L)
  expect_identical(nrow(ggpredict(fit, "e42dep [1,3]")), 2L)
  expect_identical(nrow(ggpredict(fit, "e42dep[1,3]")), 2L)
})

test_that("ggpredict, clean_vars", {
  expect_identical(nrow(ggpredict(fit, c("c172code", "e42dep"))), 12L)
  expect_identical(nrow(ggpredict(fit, c("c172code [1,3]", "e42dep"))), 8L)
  expect_identical(nrow(ggpredict(fit, c("c172code", "e42dep [1,3]"))), 6L)
  expect_identical(nrow(ggpredict(fit, c("c172code [1,3]", "e42dep [1,3]"))), 4L)
  expect_identical(nrow(ggpredict(fit, c("c172code[1,3]", "e42dep"))), 8L)
  expect_identical(nrow(ggpredict(fit, c("c172code", "e42dep[1,3]"))), 6L)
  expect_identical(nrow(ggpredict(fit, c("c172code[1,3]", "e42dep[1,3]"))), 4L)
})

test_that("ggemmeans, clean_vars", {
  expect_identical(nrow(ggemmeans(fit, c("c172code", "e42dep"))), 12L)
  expect_identical(nrow(ggemmeans(fit, c("c172code [1,3]", "e42dep"))), 8L)
  expect_identical(nrow(ggemmeans(fit, c("c172code", "e42dep [1,3]"))), 6L)
  expect_identical(nrow(ggemmeans(fit, c("c172code [1,3]", "e42dep [1,3]"))), 4L)
  expect_identical(nrow(ggemmeans(fit, c("c172code[1,3]", "e42dep"))), 8L)
  expect_identical(nrow(ggemmeans(fit, c("c172code", "e42dep[1,3]"))), 6L)
  expect_identical(nrow(ggemmeans(fit, c("c172code[1,3]", "e42dep[1,3]"))), 4L)
})
