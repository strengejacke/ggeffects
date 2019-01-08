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

  test_that("ggpredict, x.as.factor-general", {
    ggpredict(fit, "c12hour", x.as.factor = TRUE)
    ggpredict(fit, "c172code", x.as.factor = TRUE)
    ggpredict(fit, "c172code", x.as.factor = FALSE)
  })

  test_that("ggemmeans, x.as.factor-general", {
    ggemmeans(fit, "c12hour", x.as.factor = TRUE)
    ggemmeans(fit, "c172code", x.as.factor = TRUE)
    ggemmeans(fit, "c172code", x.as.factor = FALSE)
  })


  test_that("ggpredict, x.as.factor-2", {
    p1 <- ggpredict(fit, "c172code", x.as.factor = TRUE)
    p2 <- ggpredict(fit, "c172code", x.as.factor = FALSE)
    expect_equal(p1$predicted, p2$predicted)
    expect_is(p1$x, "factor")
    expect_is(p2$x, "numeric")
    expect_equal(attr(p1, "x.is.factor", exact = TRUE), "1")
    expect_equal(attr(p2, "x.is.factor", exact = TRUE), "1")
  })


  test_that("ggemmeans, x.as.factor-2", {
    p1 <- ggemmeans(fit, "c172code", x.as.factor = TRUE)
    p2 <- ggemmeans(fit, "c172code", x.as.factor = FALSE)
    expect_equal(p1$predicted, p2$predicted)
    expect_is(p1$x, "factor")
    expect_is(p2$x, "numeric")
    expect_equal(attr(p1, "x.is.factor", exact = TRUE), "1")
    expect_equal(attr(p2, "x.is.factor", exact = TRUE), "1")
  })


  test_that("ggpredict, x.as.factor-3", {
    p1 <- ggpredict(fit, "e42dep", x.as.factor = TRUE)
    p2 <- ggpredict(fit, "e42dep", x.as.factor = FALSE)
    expect_equal(p1$predicted, p2$predicted)
    expect_equal(attr(p1, "x.is.factor", exact = TRUE), "1")
    expect_equal(attr(p2, "x.is.factor", exact = TRUE), "1")
  })


  test_that("ggpredict, x.as.factor-4", {
    p1 <- ggpredict(fit, c("c172code", "e42dep"), x.as.factor = TRUE)
    p2 <- ggpredict(fit, c("c172code", "e42dep"), x.as.factor = FALSE)
    expect_equal(p1$predicted, p2$predicted)
    expect_is(p1$x, "factor")
    expect_is(p2$x, "numeric")
  })


  test_that("ggpredict, x.as.factor-5", {
    p1 <- ggpredict(fit, c("e42dep", "c172code"), x.as.factor = TRUE)
    p2 <- ggpredict(fit, c("e42dep", "c172code"), x.as.factor = FALSE)
    expect_equal(p1$predicted, p2$predicted)
    expect_is(p1$x, "factor")
    expect_is(p2$x, "numeric")
  })


  test_that("ggpredict, x.as.factor-numeric", {
    p1 <- ggpredict(fit, "c12hour", x.as.factor = TRUE)
    p2 <- ggpredict(fit, "c12hour", x.as.factor = FALSE)
    expect_equal(p1$predicted, p2$predicted)
    expect_is(p1$x, "numeric")
    expect_is(p2$x, "numeric")
    expect_equal(attr(p1, "x.is.factor", exact = TRUE), "0")
    expect_equal(attr(p2, "x.is.factor", exact = TRUE), "0")
  })
}
