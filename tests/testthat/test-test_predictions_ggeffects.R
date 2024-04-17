skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")

test_that("test_predictions, engine emmeans", {
  data(efc, package = "ggeffects")
  efc$c172code <- as.factor(efc$c172code)
  efc$c161sex <- as.factor(efc$c161sex)
  efc$e15relat <- as.factor(efc$e15relat)
  efc$e42dep <- as.factor(efc$e42dep)
  levels(efc$c161sex) <- c("male", "female")

  # single focal term
  m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

  # categorical
  pr <- ggemmeans(m, "c172code")
  out1 <- test_predictions(pr, engine = "ggeffects")
  out2 <- test_predictions(m, "c172code", engine = "emmeans")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_equal(attributes(out1)$standard_error, attributes(out2)$standard_error, tolerance = 1e-3)
  expect_identical(out1$c172code, out2$c172code)

  # multiple focal terms, interaction
  m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex * c172code, data = efc)

  # categorical
  pr <- ggemmeans(m, c("c172code", "c161sex"))
  out1 <- test_predictions(pr, engine = "ggeffects")
  out2 <- test_predictions(m, c("c172code", "c161sex"), engine = "emmeans")
  expect_equal(out1$Contrast[1:2], out2$Contrast[1:2], tolerance = 1e-3)
  expect_identical(out1$c172code[1:2], out2$c172code[1:2])

  # difference-in-difference
  pr <- ggemmeans(m, c("c172code", "c161sex"))
  out1 <- test_predictions(pr, engine = "ggeffects", test = "interaction")
  out2 <- test_predictions(m, c("c172code", "c161sex"), engine = "emmeans", test = "interaction")
  expect_equal(out1$Contrast, out2$Contrast, tolerance = 1e-3)
  expect_identical(out1$c172code, out2$c172code)
  expect_identical(out1$c172code, c("1-2", "1-3", "2-3"))
  expect_identical(out1$c161sex, c("male and female", "male and female", "male and female"))
  expect_identical(attributes(out1)$test, "interaction")
})
