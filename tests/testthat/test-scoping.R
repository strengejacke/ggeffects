if (suppressWarnings(requiet("testthat") && requiet("ggeffects") && requiet("insight"))) {

  test_that("ggpredict, scoping", {
    data(efc, package = "ggeffects")

    fn1 <- function(data) {
      fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = data)
      v <- c(20, 50, 70)
      ggpredict(fit, terms = "c12hour [20, 50, 70]", verbose = FALSE)
    }

    fn2 <- function(data) {
      fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = data)
      v <- c(20, 50, 70)
      ggpredict(fit, terms = "c12hour [v]", verbose = FALSE)
    }

    out1 <- capture.output(print(fn1(efc)))
    out2 <- capture.output(print(fn2(efc)))
    expect_identical(out1, out2)
  })

  test_that("ggpredict, scoping-2", {
    data(efc, package = "ggeffects")
    fn3 <- function(data, v) {
      fit <- lm(barthtot ~ c12hour + c172code, data = data)
      ggpredict(fit, terms = "c12hour [v]", verbose = FALSE)
    }
    v <- c(5, 6, 7)
    a <- c(0.3, 0.5, 0.7)
    out <- fn3(efc, a)
    expect_equal(out$x, c(0.3, 0.5, 0.7), ignore_attr = TRUE)
  })
}
