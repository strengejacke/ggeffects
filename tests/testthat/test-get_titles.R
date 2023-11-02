skip_on_os(c("mac", "solaris"))
skip_if_not_installed("haven")
skip_if_not_installed("sjlabelled")
skip_if_not_installed("sjmisc")

test_that("ggpredict, get_title/labels", {
  data(efc, package = "ggeffects")
  efc$c172code <- sjmisc::to_factor(efc$c172code)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
  mydf <- ggpredict(fit, terms = c("c12hour", "c161sex", "c172code"))
  expect_identical(get_x_title(mydf), "average number of hours of care per week")
  expect_identical(get_y_title(mydf), "Total score BARTHEL INDEX")
  expect_identical(get_legend_labels(mydf), c("Male", "Female"))
  expect_identical(get_legend_title(mydf), "carer's gender")
  mydf <- ggpredict(fit, terms = "c172code")
  expect_identical(
    get_x_labels(mydf),
    c("low level of education", "intermediate level of education", "high level of education")
  )
})
