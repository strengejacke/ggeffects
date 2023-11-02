skip_if_not_installed("robustbase")
skip_if_not_installed("withr")

# test_that("ggpredict, glmrob", {
#   data(efc, package = "ggeffects")
#   efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
#   d <<- efc
#   m1 <- robustbase::glmrob(
#     neg_c_7d ~ c12hour + e42dep + c161sex + c172code,
#     data = d,
#     family = binomial
#   )
#   pr <- ggpredict(m1, "c12hour", verbose = FALSE)
#   expect_equal(pr$predicted[1], 0.4035267, tolerance = 1e-4)
#   expect_null(ggemmeans(m1, "c12hour", verbose = FALSE))
# })

withr::with_environment(
  new.env(),
  test_that("ggpredict, glmrob", {
    data(efc, package = "ggeffects")
    efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
    d <- efc
    m1 <- robustbase::glmrob(
      neg_c_7d ~ c12hour + e42dep + c161sex + c172code,
      data = d,
      family = binomial
    )
    pr <- ggpredict(m1, "c12hour", verbose = FALSE)
    expect_equal(pr$predicted[1], 0.4035267, tolerance = 1e-4)
    expect_null(ggemmeans(m1, "c12hour", verbose = FALSE))
  })
)
