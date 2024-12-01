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


test_that("plot, vignette introduction", {
  data(efc, package = "ggeffects")

  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "e42dep"))
  # fit model with 4-way-interaction
  fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex * c172code, data = efc)
  # adjusted predictions for all 4 interaction terms
  pr <- predict_response(fit, c("c12hour", "barthtot", "c161sex", "c172code"))
  vdiffr::expect_doppelganger(
    "Vignette-introduction-4-way",
    plot(pr) + ggplot2::theme(legend.position = "bottom")
  )
  # fit model with 5-way-interaction
  fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex * c172code * e42dep, data = efc)
  # adjusted predictions for all 5 interaction terms
  pr <- suppressWarnings(predict_response(fit, c("c12hour", "barthtot", "c161sex", "c172code", "e42dep"))) # nolint
  vdiffr::expect_doppelganger(
    "Vignette-introduction-5-way",
    plot(pr)
  )
  vdiffr::expect_doppelganger(
    "Vignette-introduction-5-way",
    plot(pr, n_rows = 2) + ggplot2::theme(legend.position = "bottom")
  )
  vdiffr::expect_doppelganger(
    "Vignette-introduction-5-way",
    plot(pr, n_rows = 4) + ggplot2::theme(legend.position = "bottom")
  )
})
