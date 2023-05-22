if (requiet("testthat") && 
    requiet("ggeffects") &&
    requiet("nestedLogit") &&
    requiet("car") &&
    requiet("carData") &&
    requiet("datawizard") &&
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
      hincome = seq(0, 45, by = 5),
      children = c("absent", "present")
    )
    out <- ggpredict(m, c("hincome [0:45 by=5]", "children"))
    out <- datawizard::data_arrange(
      out[out$response.level == "fulltime", ],
      select = c("group", "x")
    )

    expect_equal(
      out$predicted,
      unname(predict(m, newdata = new)[, "fulltime"]),
      ignore_attr = TRUE,
      tolerance = 1e-3
    )
  })
}
