skip_on_os(c("mac", "solaris"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("datawizard")
skip_if_not_installed("vdiffr")

test_that("plot, vignette", {
  data(efc, package = "ggeffects")
  efc$c172code <- datawizard::to_factor(efc$c172code)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

  dat <- predict_response(fit, terms = c("c12hour", "c172code"))
  vdiffr::expect_doppelganger(
    "Vignette-plotintro-1",
    plot(dat, facets = TRUE)
  )
  # don't use facets, b/w figure, w/o confidence bands
  vdiffr::expect_doppelganger(
    "Vignette-plotintro-2",
    plot(dat, colors = "bw", show_ci = FALSE)
  )

  set.seed(123)
  dat <- predict_response(fit, terms = c("c12hour", "c172code"))
  vdiffr::expect_doppelganger(
    "Vignette-plotintro-3",
    plot(dat, show_data = TRUE, verbose = FALSE)
  )

  # for three variables, automatic facetting
  dat <- predict_response(fit, terms = c("c12hour", "c172code", "c161sex"))
  vdiffr::expect_doppelganger(
    "Vignette-plotintro-4",
    plot(dat)
  )

  dat <- predict_response(fit, terms = c("c172code", "c161sex"))
  vdiffr::expect_doppelganger(
    "Vignette-plotintro-5",
    plot(dat)
  )

  # point-geoms for discrete x-axis can be connected with lines
  vdiffr::expect_doppelganger(
    "Vignette-plotintro-6",
    plot(dat, connect_lines = TRUE)
  )

  # for four variables, automatic facetting and integrated panel
  dat <- predict_response(fit, terms = c("c12hour", "c172code", "c161sex", "neg_c_7"))
  # use 'one_plot = FALSE' for returning multiple single plots
  vdiffr::expect_doppelganger(
    "Vignette-plotintro-7",
    plot(dat, one_plot = TRUE)
  )

  vdiffr::expect_doppelganger(
    "Vignette-plotintro-8",
    plot(dat, one_plot = TRUE, n_rows = 2) + ggplot2::theme(legend.position = "bottom")
  )

  # dashed lines for CI
  dat <- predict_response(fit, terms = "c12hour")
  vdiffr::expect_doppelganger(
    "Vignette-plotintro-9",
    plot(dat, ci_style = "dash")
  )

  # facet by group
  dat <- predict_response(fit, terms = c("c12hour", "c172code"))
  vdiffr::expect_doppelganger(
    "Vignette-plotintro-10",
    plot(dat, facets = TRUE, ci_style = "errorbar", dot_size = 1.5)
  )

  dat <- predict_response(fit, terms = "c172code")
  vdiffr::expect_doppelganger(
    "Vignette-plotintro-11",
    plot(dat, facets = TRUE, ci_style = "errorbar", dot_size = 1.5)
  )
})
