if (suppressWarnings(requiet("testthat") && requiet("ggeffects") && requiet("insight"))) {

  test_that("ggpredict, scoping", {
    data(efc)

    fn1 <- function(data) {
      fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = data)
      v <- c(20, 50, 70)
      ggpredict(fit, terms = "c12hour [20, 50, 70]")
    }

    fn2 <- function(data) {
      fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = data)
      v <- c(20, 50, 70)
      ggpredict(fit, terms = "c12hour [v]")
    }

    out1 <- capture.output(print(fn1(efc)))
    out2 <- capture.output(print(fn2(efc)))
    expect_identical(out1, out2)
  })
}
