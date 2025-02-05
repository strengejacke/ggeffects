skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if(getRversion() < "4.3.0")

data(efc, package = "ggeffects")
efc$c172code <- as.factor(efc$c172code)
m1 <- lm(neg_c_7 ~ c12hour * c172code, data = efc)

test_that("ggpredict, johnson_neyman for numerical and categorical", {
  pr <- ggpredict(m1, c("c12hour", "c172code"))
  out <- utils::capture.output(print(johnson_neyman(pr, precision = 50)))
  expect_identical(
    out,
    c(
      "# Difference between levels `1-2` of c172code",
      "The difference between levels `1-2` of `c172code` is statistically",
      "  significant for values of `c12hour` higher than 100. There were no",
      "  statistically significant differencens for values of `c12hour` lower",
      "  than 100.",
      "",
      "# Difference between levels `1-3` of c172code",
      "The difference between levels `1-3` of `c172code` is statistically",
      "  significant for values of `c12hour` that range from 50 to 140. Outside",
      "  of this interval, there were no statistically significant differences.",
      "",
      "# Difference between levels `2-3` of c172code",
      "The difference between levels `2-3` of `c172code` is statistically",
      "  significant for values of `c12hour` lower than 50. There were no",
      "  statistically significant differencens for values of `c12hour` higher",
      "  than 50."
    )
  )
})
