skip_on_os(c("mac", "solaris"))
skip_if_not_installed("datawizard")
skip_if_not_installed("lme4")
skip_if_not_installed("withr")

data(efc, package = "ggeffects")

efc$e15relat <- datawizard::to_factor(efc$e15relat)
efc$e42dep <- datawizard::to_factor(efc$e42dep)
efc$c172code <- as.factor(efc$c172code)

m <- lme4::lmer(neg_c_7 ~ e42dep + c172code + (1 | e15relat), data = efc)

withr::local_options(
  list(contrasts = rep("contr.sum", 2)),
  test_that("ggpredict, contrasts-1", {
    pr <- ggpredict(m, c("c172code", "e42dep"))
    expect_false(anyNA(pr$std.error))
  })
)

withr::local_options(
  list(contrasts = rep("contr.sum", 2)),
  test_that("ggpredict, contrasts-2", {
    pr <- ggpredict(m, "c172code")
    expect_false(anyNA(pr$std.error))
  })
)

withr::local_options(
  list(contrasts = rep("contr.sum", 2)),
  test_that("ggpredict, contrasts-3", {
    pr <- ggpredict(m, "e42dep")
    expect_false(anyNA(pr$std.error))
  })
)

withr::local_options(
  list(contrasts = rep("contr.treatment", 2)),
  test_that("ggpredict, contrasts-4", {
    pr <- ggpredict(m, c("c172code", "e42dep"))
    expect_false(anyNA(pr$std.error))
  })
)

withr::local_options(
  list(contrasts = rep("contr.treatment", 2)),
  test_that("ggpredict, contrasts-5", {
    pr <- ggpredict(m, "c172code")
    expect_false(anyNA(pr$std.error))
  })
)

withr::local_options(
  list(contrasts = rep("contr.treatment", 2)),
  test_that("ggpredict, contrasts-6", {
    pr <- ggpredict(m, "e42dep")
    expect_false(anyNA(pr$std.error))
  })
)
