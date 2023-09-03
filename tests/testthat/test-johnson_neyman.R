if (suppressWarnings(
  requiet("testthat") &&
    requiet("ggeffects") &&
    requiet("marginaleffects")
)) {
  data(efc, package = "ggeffects")
  efc$c172code <- as.factor(efc$c172code)
  m <- lm(neg_c_7 ~ c12hour * barthtot * c172code, data = efc)

  test_that("ggpredict, johnson_neyman, 2 focal terms", {
    pr <- ggpredict(m, c("c12hour", "barthtot"))
    out <- utils::capture.output(print(johnson_neyman(pr)))
    expect_identical(
      out,
      "For values of `barthtot` larger than 47, the slope of `c12hour` is p < 0.05. "
    )
  })

  test_that("ggpredict, johnson_neyman, 3 focal terms", {
    pr <- ggpredict(m, c("c12hour", "c172code", "barthtot"))
    out <- utils::capture.output(print(johnson_neyman(pr)))
    expect_identical(
      out,
      c(
        "# Level `c172code = 1`", "For values of `barthtot` larger than 47, the slope of `c12hour` is p < 0.05. ",
        "", "# Level `c172code = 2`", "For values of `barthtot` larger than 33.50, the slope of `c12hour` is p < 0.05. ",
        "", "# Level `c172code = 3`", "There are no significant slopes of `c12hour` for any value of `barthtot`. ",
        ""
      )
    )
  })
}
