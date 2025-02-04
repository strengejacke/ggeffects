skip_on_cran()
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
      "  of `barthtot` higher than 47."
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
      "  were no clear associations."
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
      "  values of `barthtot` lower than 48."
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
      "  Inside the interval of [0.78, 2.66], there were no clear associations."
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
      "  of `barthtot` higher than 47.",
      "",
      "# Level `c172code = 2`",
      "The association between `c12hour` and `neg_c_7` is positive for values",
      "  of `barthtot` higher than 33. There were no clear associations for",
      "  values of `barthtot` lower than 33.",
      "",
      "# Level `c172code = 3`",
      "There are no clear negative or positive associations between `c12hour`",
      "  and `neg_c_7` for any value of `barthtot`."
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
      "  of `barthtot` higher than 38.",
      "",
      "P-values were adjusted using the Esarey & Sumner (2017) method."
    )
  )
  out <- utils::capture.output(print(out3))
  expect_identical(
    out,
    c(
      "The association between `c12hour` and `neg_c_7` is negative for values",
      "  of `barthtot` lower than 38. There were no clear associations for values",
      "  of `barthtot` higher than 38.",
      "",
      "P-values were adjusted using the Benjamini & Hochberg (1995) method."
    )
  )
  expect_error(johnson_neyman(pr, p_adjust = "bonferroni"), regex = "Invalid option for argument")
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
      "  and `neg_c_7d` for any value of `barthtot`.",
      "",
      "P-values were adjusted using the Esarey & Sumner (2017) method."
    )
  )
})

test_that("ggpredict, johnson_neyman, p-adjustment, df and vcov", {
  data(efc, package = "ggeffects")
  efc$c172code <- as.factor(efc$c172code)
  m1 <- lm(neg_c_7 ~ c12hour * barthtot * c172code, data = efc)
  pr <- ggpredict(m1, c("c12hour", "barthtot"))
  out1 <- johnson_neyman(pr, precision = 100)
  out2 <- johnson_neyman(pr, p_adjust = "esarey", precision = 100)
  out3 <- johnson_neyman(pr, p_adjust = "esarey", df = 100, precision = 100)
  out4 <- johnson_neyman(pr, p_adjust = "esarey", vcov = "HC1", precision = 100)
  out5 <- johnson_neyman(pr, p_adjust = "esarey", vcov = "HC1", df = 100, precision = 100)
  expect_equal(
    head(out1$conf.low),
    c(-0.04875, -0.04811, -0.04748, -0.04685, -0.04622, -0.04559),
    tolerance = 1e-3
  )
  expect_equal(
    head(out2$conf.low),
    c(-0.05278, -0.05209, -0.0514, -0.05071, -0.05003, -0.04935),
    tolerance = 1e-3
  )
  expect_equal(
    head(out3$conf.low),
    c(-0.05329, -0.0526, -0.0519, -0.05121, -0.05052, -0.04983),
    tolerance = 1e-3
  )
  expect_equal(
    head(out4$conf.low),
    c(-0.05465, -0.05397, -0.05329, -0.05261, -0.05193, -0.05126),
    tolerance = 1e-3
  )
  expect_equal(
    head(out5$conf.low),
    c(-0.0552, -0.05452, -0.05383, -0.05314, -0.05246, -0.05178),
    tolerance = 1e-3
  )
})

test_that("ggpredict, johnson_neyman, p-adjustment, df and vcov", {
  data(efc, package = "ggeffects")
  efc$c172code <- as.factor(efc$c172code)
  m <- lm(neg_c_7 ~ c12hour * barthtot * c172code, data = efc)
  pr <- predict_response(m, c("c12hour", "c172code", "barthtot"))
  out <- johnson_neyman(pr, vcov = sandwich::vcovHC)
  expect_s3_class(out, "ggjohnson_neyman")
  expect_identical(dim(out), c(1503L, 8L))
})
