skip_on_os(c("mac", "solaris"))
skip_if_not_installed("splines")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("ggpredict, custom spline function", {
    # custom function for splines (#260)
    fit <- lm(barthtot ~ c12hour + splines::bs(neg_c_7, df = 9) * c161sex + e42dep, data = efc)
    p1 <- ggpredict(fit, terms = c("neg_c_7", "c161sex", "e42dep"))
    bs9 <<- function(x) splines::bs(x, df = 9) # <---- put spline in a custom function
    fit <- lm(barthtot ~ c12hour + bs9(neg_c_7) * c161sex + e42dep, data = efc)
    p2 <- ggpredict(fit, terms = c("neg_c_7", "c161sex", "e42dep"))
    expect_equal(p1$predicted[1:8], p2$predicted[1:8], tolerance = 1e-4)
  })
)
