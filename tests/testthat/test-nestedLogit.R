if (requiet("testthat") &&
      requiet("ggeffects") &&
      requiet("nestedLogit") &&
      requiet("effect") &&
      requiet("car") &&
      requiet("carData") &&
      requiet("broom") &&
      packageVersion("insight") > "0.19.2") {

  test_that("plot, correct x-labels order for character vector", {
    data(Womenlf, package = "carData")
    Womenlf$partic <- with(
      Womenlf,
      factor(partic, levels = c("not.work", "parttime", "fulltime"))
    )

    m <- nestedLogit::nestedLogit(partic ~ hincome + children,
      dichotomies = nestedLogit::logits(
        work = nestedLogit::dichotomy("not.work", working = c("parttime", "fulltime")),
        full = nestedLogit::dichotomy("parttime", "fulltime")
      ),
      data = Womenlf
    )

    new <- expand.grid(
      hincome = c(30, 40),
      children = c("absent", "present")
    )

    out <- as.data.frame(ggpredict(m, c("hincome [30, 40]", "children")))
    expected <- as.data.frame(predict(m, newdata = new), newdata = new)
    expected <- expected[order(expected$hincome, expected$children), ]

    expect_equal(
      out$predicted,
      expected$p,
      ignore_attr = TRUE,
      tolerance = 1e-3
    )

    expect_equal(
      out$conf.low,
      c(
        0.30665, 0.09012, 0.13574, 0.6929, 0.08997, 0.0034, 0.32716, 
        0.10429, 0.02488, 0.70621, 0.0465, 0.00037),
      ignore_attr = TRUE,
      tolerance = 1e-3
    )
  })

  test_that("ggeffect works with nestedLogit", {
    data("Womenlf", package = "carData")
    m <- nestedLogit(partic ~ hincome + children,
      logits(
        work = dichotomy("not.work", c("parttime", "fulltime")),
        full = dichotomy("parttime", "fulltime")
      ),
      data = Womenlf
    )
    out1 <- ggeffect(m, "hincome [1,10,20,30,40]")
    out2 <- effects::Effect("hincome", m)

    expect_equal(out1$predicted, as.vector(out2$prob), ignore_attr = TRUE, tolerance = 1e-3)
  })
}
