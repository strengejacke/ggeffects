skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip("brms-categ-cum only works interactively")

skip_if_not_installed("brms")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("ggpredict, brms-categ-cum", {
    data(mtcars)
    mtcars$cyl_ord <- as.ordered(mtcars$cyl)
    mtcars$gear_fct <- factor(mtcars$gear)
    set.seed(123)
    m3 <- brms::brm(
      gear ~ mpg,
      data = mtcars,
      family = brms::categorical(),
      refresh = 0,
      open_progress = FALSE
    )
    set.seed(123)
    m4 <- brms::brm(
      gear_fct ~ mpg,
      data = mtcars,
      family = brms::categorical(),
      refresh = 0,
      open_progress = FALSE
    )
    p3 <- ggpredict(m3, "mpg")
    p4 <- ggpredict(m4, "mpg")

    # m3/m4 are the same, except response is numeric/factor, so predictions should be the same
    p4$response.level <- as.numeric(p4$response.level)
    for (resp.level in 3:5) {
      expect_equal(
        p3[p3$response.level == resp.level, ],
        p4[p4$response.level == resp.level, ],
        ignore_attr = TRUE, tolerance = 0.05
      )
    }
  })
)
