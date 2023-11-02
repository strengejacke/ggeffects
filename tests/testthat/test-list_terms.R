skip_on_os(c("mac", "solaris"))
skip_if_not_installed("datawizard")

# lm, linear regression ----

data(efc, package = "ggeffects")
efc$c172code <- datawizard::to_factor(efc$c172code)
efc$c161sex <- datawizard::to_factor(efc$c161sex)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

test_that("ggpredict, lm by", {
  expect_identical(nrow(ggpredict(fit, list(c12hour = 10:20))), 11L)
  expect_s3_class(ggpredict(fit, list(c12hour = c(20, 30, 40))), "data.frame")
  out <- ggpredict(
    fit,
    list(c161sex = c("Male", "Female"),
          c172code = c("high level of education", "low level of education"))
  )
  expect_equal(
    out$x,
    structure(c(1L, 1L, 2L, 2L), levels = c("Male", "Female"), class = "factor"),
    ignore_attr = TRUE
  )
  expect_equal(
    out$group,
    structure(c(1L, 2L, 1L, 2L), levels = c("high level of education",
                                            "low level of education"), class = "factor"),
    ignore_attr = TRUE
  )
})
