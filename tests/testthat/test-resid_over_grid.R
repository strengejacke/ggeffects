if (suppressWarnings(requiet("testthat") && requiet("ggeffects"))) {

  test_that("residualize over grid", {
    set.seed(1234)
    x <- rnorm(200)
    z <- rnorm(200)
    # quadratic relationship
    y <- 2 * x + x^2 + 4 * z + rnorm(200)

    d <- data.frame(x, y, z)
    model <- lm(y ~ x + z, data = d)

    pr <- ggpredict(model, c("x [all]", "z"))
    out <- residualize_over_grid(pr, model)
    expect_equal(
      head(out$x),
      c(-1.207, 0.277, 1.084, -2.346, 0.429, 0.506),
      tolerance = 1e-3
    )
    expect_equal(
      head(out$predicted),
      c(-1.79724, 4.88871, 3.2322, 4.13356, 1.80159, 5.65953),
      tolerance = 1e-4
    )
    expect_identical(colnames(out), c("x", "group", "predicted"))
  })

  test_that("residualize over grid, scoping", {
    data(mtcars)
    # inside a list
    models <- list(
      model1 = lm(displ ~ cty, data = mpg)
    )

    x <- ggpredict(
      model = models$model1,
      terms = "cty"
    )
    model <- ggeffects:::.get_model_object(preds)
    residual_data <- residualize_over_grid(grid = x, model = model)
    expect_equal(
      head(round(residual_data$predicted, 5)),
      c(1.8, 2.0424, 2, 2.2424, 2.8, 2.8),
      tolerance = 1e-3
    )

    x <- ggpredict(
      model = models[["model1"]],
      terms = "cty"
    )
    model <- ggeffects:::.get_model_object(preds)
    residual_data <- residualize_over_grid(grid = x, model = model)
    expect_equal(
      head(round(residual_data$predicted, 5)),
      c(1.8, 2.0424, 2, 2.2424, 2.8, 2.8),
      tolerance = 1e-3
    )

    x <- ggpredict(
      model = models[[1]],
      terms = "cty"
    )
    model <- ggeffects:::.get_model_object(preds)
    residual_data <- residualize_over_grid(grid = x, model = model)
    expect_equal(
      head(round(residual_data$predicted, 5)),
      c(1.8, 2.0424, 2, 2.2424, 2.8, 2.8),
      tolerance = 1e-3
    )
  })
}
