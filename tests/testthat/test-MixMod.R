skip_on_cran()
skip_on_os(c("mac", "solaris", "linux"))

skip_if_not_installed("emmeans")
skip_if_not_installed("GLMMadaptive")
skip_if_not_installed("lme4")
skip_if_not_installed("withr")

test_that("ggpredict", {
  data(fish, package = "ggeffects")
  set.seed(123)
  m1 <- GLMMadaptive::mixed_model(
    count ~ child + camper,
    random = ~ 1 | persons,
    zi_fixed = ~ child + livebait,
    zi_random = ~ 1 | persons,
    data = fish,
    family = GLMMadaptive::zi.poisson()
  )
  m2 <- GLMMadaptive::mixed_model(
    nofish ~ xb + zg,
    random = ~ 1 | persons,
    data = fish,
    family = binomial
  )
  withr::with_options(
    list(ggeffects_warning_bias_correction = TRUE),
    {
      set.seed(123)
      expect_message(expect_message(
        {
          p <- ggpredict(m1, c("child", "camper"), type = "zero_inflated")
        },
        regex = "You are calculating adjusted"
      ), regex = "Results for MixMod-objects")
      expect_equal(p$predicted[1], 2.040251, tolerance = 1e-2)
    }
  )

  set.seed(123)
  p <- ggpredict(m1, c("child", "camper"), type = "zero_inflated_random", condition = c(count = 3.296), verbose = FALSE)
  expect_equal(p$predicted[1], 4.982773, tolerance = 1e-2)

  set.seed(123)
  p <- ggpredict(m1, c("child", "camper"), type = "zero_inflated_random", condition = c(count = 0), verbose = FALSE)
  expect_equal(p$predicted[1], 0.5115884, tolerance = 1e-2)

  set.seed(123)
  p <- ggemmeans(m1, c("child", "camper"), type = "zero_inflated", verbose = FALSE)
  expect_equal(p$predicted[1], 1.816723, tolerance = 1e-2)

  set.seed(123)
  p <- ggemmeans(m1, c("child", "camper"), type = "zero_inflated", interval = "prediction", verbose = FALSE)
  expect_equal(p$predicted[1], 1.816723, tolerance = 1e-2)

  withr::with_options(
    list(ggeffects_warning_bias_correction = TRUE),
    {
      expect_message(expect_message(expect_message(ggpredict(m1, c("child", "camper"), type = "fixed"))))
    }
  )
  withr::with_options(
    list(ggeffects_warning_bias_correction = TRUE),
    {
      expect_message(expect_message(expect_message(expect_message(ggpredict(m2, "zg", type = "zero_inflated")))))
    }
  )
})
