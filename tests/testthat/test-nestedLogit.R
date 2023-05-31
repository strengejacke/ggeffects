if (requiet("testthat") &&
    requiet("ggeffects") &&
    requiet("nestedLogit") &&
    requiet("car") &&
    requiet("carData") &&
    requiet("broom")) {

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
    out <- out[out$response.level == "fulltime", ]

    expected <- cbind(new, as.data.frame(predict(m, newdata = new)))
    expected <- expected[expected$response == "fulltime", ]
    expected <- expected[order(expected$hincom, expected$children), ]

    expect_equal(
      out$predicted,
      expected$p,
      ignore_attr = TRUE,
      tolerance = 1e-3
    )

    expect_equal(
      out$conf.low,
      c(0.0034, 0.13574, 0.02488, 0.00037),
      ignore_attr = TRUE,
      tolerance = 1e-3
    )
  })
}
