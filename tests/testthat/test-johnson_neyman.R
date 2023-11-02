skip_if_not_installed("marginaleffects")
skip_if(getRversion() < "4.3.0")

data(efc, package = "ggeffects")
efc$c172code <- as.factor(efc$c172code)
m1 <- lm(neg_c_7 ~ c12hour * barthtot * c172code, data = efc)
m2 <- lm(c12hour ~ neg_c_7 * barthtot, data = efc)
m3 <- lm(neg_c_7 ~ c12hour * barthtot, data = efc)

states <- as.data.frame(state.x77)
states$HSGrad <- states$`HS Grad`
m4 <- lm(Income ~ HSGrad + Murder * Illiteracy,
  data = states
)

test_that("ggpredict, johnson_neyman, 2 focal terms, one direction", {
  pr <- ggpredict(m1, c("c12hour", "barthtot"))
  out <- utils::capture.output(print(johnson_neyman(pr, precision = 100)))
  expect_identical(
    out,
    c(
      "The association between `c12hour` and `neg_c_7` is negative for values",
      "  of `barthtot` lower than 47. There were no clear associations for values",
      "  of `barthtot` higher than 47. "
    )
  )
})

test_that("ggpredict, johnson_neyman, 2 focal terms, inside interval", {
  pr <- ggpredict(m2, c("neg_c_7", "barthtot"))
  out <- utils::capture.output(print(johnson_neyman(pr, precision = 100)))
  expect_identical(
    out,
    c(
      "The association between `neg_c_7` and `c12hour` is positive for values",
      "  of `barthtot` that range from 46 to 49. Outside of this interval, there",
      "  were no clear associations. "
    )
  )
})

test_that("ggpredict, johnson_neyman, 2 focal terms, one direction", {
  pr <- ggpredict(m3, c("c12hour", "barthtot"))
  out <- utils::capture.output(print(johnson_neyman(pr, precision = 100)))
  expect_identical(
    out,
    c(
      "The association between `c12hour` and `neg_c_7` is positive for values",
      "  of `barthtot` higher than 48. There were no clear associations for",
      "  values of `barthtot` lower than 48. "
    )
  )
})

test_that("ggpredict, johnson_neyman, 2 focal terms, outside interval", {
  pr <- ggpredict(m4, c("Murder", "Illiteracy"))
  out <- utils::capture.output(print(johnson_neyman(pr, precision = 100)))
  expect_identical(
    out,
    c(
      "The association between `Murder` and `Income` is positive for values of",
      "  `Illiteracy` lower than 0.78 and negative for values higher than 2.66.",
      "  Inside the interval of [0.78, 2.66], there were no clear associations. "
    )
  )
})

test_that("ggpredict, johnson_neyman, 3 focal terms", {
  pr <- ggpredict(m1, c("c12hour", "c172code", "barthtot"))
  out <- utils::capture.output(print(johnson_neyman(pr, precision = 100)))
  expect_identical(
    out,
    c(
      "# Level `c172code = 1`",
      "The association between `c12hour` and `neg_c_7` is negative for values",
      "  of `barthtot` lower than 47. There were no clear associations for values",
      "  of `barthtot` higher than 47. ",
      "",
      "# Level `c172code = 2`",
      "The association between `c12hour` and `neg_c_7` is positive for values",
      "  of `barthtot` higher than 33. There were no clear associations for",
      "  values of `barthtot` lower than 33. ",
      "",
      "# Level `c172code = 3`",
      "There are no clear negative or positive associations between `c12hour`",
      "  and `neg_c_7` for any value of `barthtot`. "
    )
  )
})
