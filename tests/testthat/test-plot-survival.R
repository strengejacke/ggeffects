skip_on_os(c("mac", "solaris"))
skip_on_cran()
skip_if_not_installed("ggplot2")
skip_if_not_installed("datawizard")
skip_if_not_installed("vdiffr")
skip_if_not_installed("survival")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("plot, survival models", {
    library(survival)
    data(lung2)
    m <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung2)
    # predicted risk-scores
    pr <- predict_response(m, c("sex", "ph.ecog"))
    vdiffr::expect_doppelganger(
      "Vignette-plotsurvival-1",
      plot(pr)
    )
    pr <- predict_response(m, c("sex", "ph.ecog"), type = "survival")
    vdiffr::expect_doppelganger(
      "Vignette-plotsurvival-2",
      plot(pr)
    )
    pr <- predict_response(m, c("sex", "ph.ecog"), type = "cumulative_hazard")
    vdiffr::expect_doppelganger(
      "Vignette-plotsurvival-3",
      plot(pr)
    )
  })
)
