skip_on_os(c("mac", "solaris"))
skip_if_not_installed("datawizard")
skip_if_not_installed("lme4")
skip_if_not_installed("withr")

data(efc, package = "ggeffects")

efc$e15relat <- datawizard::to_factor(efc$e15relat)

m <- lme4::lmer(neg_c_7 ~ c160age + c12hour + (1 | e15relat), data = efc)

withr::local_options(
  list(contrasts = rep("contr.sum", 2)),
  test_that("ggpredict, contrasts-1", {
    pr <- ggpredict(m, c("c160age", "c12hour"))
    expect_false(anyNA(pr$std.error))
  })
)

withr::local_options(
  list(contrasts = rep("contr.sum", 2)),
  test_that("ggpredict, contrasts-2", {
    pr <- ggpredict(m, "c160age")
    expect_false(anyNA(pr$std.error))
  })
)

withr::local_options(
  list(contrasts = rep("contr.sum", 2)),
  test_that("ggpredict, contrasts-3", {
    pr <- ggpredict(m, "c12hour")
    expect_false(anyNA(pr$std.error))
  })
)

withr::local_options(
  list(contrasts = rep("contr.treatment", 2)),
  test_that("ggpredict, contrasts-4", {
    pr <- ggpredict(m, c("c160age", "c12hour"))
    expect_false(anyNA(pr$std.error))
  })
)

withr::local_options(
  list(contrasts = rep("contr.treatment", 2)),
  test_that("ggpredict, contrasts-5", {
    pr <- ggpredict(m, "c160age")
    expect_false(anyNA(pr$std.error))
  })
)

withr::local_options(
  list(contrasts = rep("contr.treatment", 2)),
  test_that("ggpredict, contrasts-6", {
    pr <- ggpredict(m, "c12hour")
    expect_false(anyNA(pr$std.error))
  })
)
