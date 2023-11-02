skip_on_os(c("mac", "solaris"))
skip_if_not_installed("datawizard")

data(efc, package = "ggeffects")
efc$e42dep <- datawizard::to_factor(efc$e42dep)
fit <- lm(barthtot ~ c12hour + neg_c_7 + e42dep + c172code, data = efc)

test_that("ggpredict, condition", {
  expect_s3_class(ggpredict(fit, "c172code"), "data.frame")
  expect_s3_class(ggpredict(fit, "c172code", condition = c(c12hour = 40)), "data.frame")
  expect_s3_class(ggpredict(fit, "c172code", condition = c(c12hour = 40, e42dep = "severely dependent")), "data.frame")
  expect_s3_class(ggpredict(fit, "c172code", condition = c(e42dep = "severely dependent")), "data.frame")
})

test_that("ggemmeans, condition", {
  expect_s3_class(ggemmeans(fit, "c172code"), "data.frame")
  expect_s3_class(ggemmeans(fit, "c172code", condition = c(c12hour = 40)), "data.frame")
  expect_s3_class(ggemmeans(fit, "c172code", condition = c(c12hour = 40, e42dep = "severely dependent")), "data.frame")
  expect_s3_class(ggemmeans(fit, "c172code", condition = c(e42dep = "severely dependent")), "data.frame")
})


efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
d <<- efc

m1 <- glm(
  neg_c_7d ~ c12hour + e42dep + c161sex + c172code,
  data = d,
  family = binomial(link = "logit")
)

test_that("ggpredict, glm", {
  expect_s3_class(
    ggpredict(m1, "c12hour", condition = c(e42dep = "severely dependent"), verbose = FALSE),
    "data.frame"
  )
  expect_s3_class(
    ggpredict(m1, c("c12hour", "c161sex"), condition = c(e42dep = "severely dependent"), verbose = FALSE),
    "data.frame"
  )
  expect_s3_class(
    ggpredict(m1, c("c12hour", "c161sex", "c172code"), condition = c(e42dep = "severely dependent"), verbose = FALSE),
    "data.frame"
  )
})

test_that("ggpredict, glm", {
  expect_s3_class(
    ggemmeans(m1, "c12hour", condition = c(e42dep = "severely dependent"), verbose = FALSE),
    "data.frame"
  )
  expect_s3_class(
    ggemmeans(m1, c("c12hour", "c161sex"), condition = c(e42dep = "severely dependent"), verbose = FALSE),
    "data.frame"
  )
  expect_s3_class(
    ggemmeans(m1, c("c12hour", "c161sex", "c172code"), condition = c(e42dep = "severely dependent"), verbose = FALSE),
    "data.frame"
  )
})


m2 <- glm(
  neg_c_7d ~ c12hour + e42dep + c161sex + c172code,
  data = d,
  family = binomial(link = "logit")
)

test_that("ggpredict, glm", {
  expect_s3_class(ggpredict(m2, "c12hour", condition = c(c172code = 1), verbose = FALSE), "data.frame")
  expect_s3_class(ggpredict(m2, c("c12hour", "c161sex"), condition = c(c172code = 2), verbose = FALSE), "data.frame")
})

skip_if_not_installed("lme4")

data(efc, package = "ggeffects")
efc$grp <- datawizard::to_factor(efc$e15relat)
efc$e42dep <- datawizard::to_factor(efc$e42dep)
d2 <<- efc

m3 <- lme4::lmer(neg_c_7 ~ c12hour + e42dep + c161sex + c172code + (1 | grp), data = d2)

test_that("ggpredict, condition-lmer", {
  pr <- ggpredict(m3, "c12hour", type = "re")
  expect_equal(pr$predicted[1], 8.962075, tolerance = 1e-3)
  expect_equal(pr$std.error[1], 3.601748, tolerance = 1e-3)

  pr <- ggpredict(m3, "c12hour", type = "re", condition = c(c172code = 1))
  expect_equal(pr$predicted[1], 8.62045, tolerance = 1e-3)
  expect_equal(pr$std.error[1], 3.606084, tolerance = 1e-3)

  pr <- ggpredict(m3, "c12hour", type = "re", condition = c(e42dep = "severely dependent"))
  expect_equal(pr$predicted[1], 12.83257, tolerance = 1e-3)
  expect_equal(pr$std.error[1], 3.601748, tolerance = 1e-3)

  pr <- ggpredict(m3, "c12hour", type = "re", condition = c(e42dep = "severely dependent", c172code = 3))
  expect_equal(pr$predicted[1], 13.19621, tolerance = 1e-3)
  expect_equal(pr$std.error[1], 3.608459, tolerance = 1e-3)

  pr <- ggpredict(m3, "c12hour", type = "re", condition = c(e42dep = "severely dependent", c172code = 3, grp = "sibling")) # nolint
  expect_equal(pr$predicted[1], 13.13315, tolerance = 1e-3)
  expect_equal(pr$std.error[1], 3.608459, tolerance = 1e-3)

  pr <- ggpredict(m3, "c12hour", type = "re", condition = c(c172code = 3, grp = "sibling"))
  expect_equal(pr$predicted[1], 9.26265, tolerance = 1e-3)
  expect_equal(pr$std.error[1], 3.608459, tolerance = 1e-3)

  pr <- ggpredict(m3, "c12hour", type = "re", condition = c(grp = "sibling"))
  expect_equal(pr$predicted[1], 8.89902, tolerance = 1e-3)
  expect_equal(pr$std.error[1], 3.601748, tolerance = 1e-3)
})
