skip_on_os(c("mac", "solaris"))
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


test_that("ggpredict, johnson_neyman, p-adjustment", {
  pr <- ggpredict(m1, c("c12hour", "barthtot"))
  out1 <- johnson_neyman(pr, p_adjust = "es", precision = 100)
  out2 <- johnson_neyman(pr, precision = 100)
  out3 <- johnson_neyman(pr, p_adjust = "bh", precision = 100)
  expect_identical(attributes(out1)$intervals$pos_lower, 38)
  expect_identical(attributes(out2)$intervals$pos_lower, 47)
  expect_identical(attributes(out3)$intervals$pos_lower, 38)
  out <- utils::capture.output(print(out1))
  expect_identical(
    out,
    c(
      "The association between `c12hour` and `neg_c_7` is negative for values",
      "  of `barthtot` lower than 38. There were no clear associations for values",
      "  of `barthtot` higher than 38. ",
      "",
      "P-values were adjusted using the Esarey & Sumner (2017) method. "
    )
  )
  out <- utils::capture.output(print(out3))
  expect_identical(
    out,
    c(
      "The association between `c12hour` and `neg_c_7` is negative for values",
      "  of `barthtot` lower than 38. There were no clear associations for values",
      "  of `barthtot` higher than 38. ",
      "",
      " P-values were adjusted using the Benjamini & Hochberg (1995) method. "
    )
  )
  expect_error(johnson_neyman(pr, p_adjust = "bonferroni"), reges = "be one of")
})


test_that("ggpredict, johnson_neyman, p-adjustment, glm", {
  data(efc, package = "ggeffects")
  efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
  d <- efc
  fit <- glm(neg_c_7d ~ c12hour * barthtot, data = d, family = binomial(link = "logit"))
  pr <- ggpredict(fit, terms = c("c12hour", "barthtot"), verbose = FALSE)
  out1 <- johnson_neyman(pr, p_adjust = "es", precision = 100)
  out2 <- johnson_neyman(pr, precision = 100)
  out3 <- johnson_neyman(pr, p_adjust = "bh", precision = 100)
  expect_identical(attributes(out1)$intervals$pos_lower, NA_real_)
  expect_identical(attributes(out2)$intervals$pos_lower, 68)
  expect_identical(attributes(out3)$intervals$pos_lower, NA_real_)
  out <- utils::capture.output(print(out1))
  expect_identical(
    out,
    c(
      "There are no clear negative or positive associations between `c12hour`",
      "  and `neg_c_7d` for any value of `barthtot`. ",
      "",
      " P-values were adjusted using the Esarey & Sumner (2017) method. "
    )
  )
})
