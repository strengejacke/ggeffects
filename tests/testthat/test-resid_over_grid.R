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

  ## TODO: works interactively only

  test_that("residualize over grid, scoping", {
    skip_if(TRUE) # works interactively only
    data(mtcars)
    # inside a list
    models <- list(
      model1 = lm(mpg ~ cyl, data = mtcars)
    )

    x <- ggpredict(
      model = models$model1,
      terms = "cyl"
    )
    model <- ggeffects:::.get_model_object(x)
    residual_data <- residualize_over_grid(grid = x, model = model)
    expect_equal(
      head(round(residual_data$predicted, 5)),
      c(21, 21, 22.8, 21.4, 18.7, 18.1),
      tolerance = 1e-3
    )

    x <- ggpredict(
      model = models[["model1"]],
      terms = "cyl"
    )
    model <- ggeffects:::.get_model_object(x)
    residual_data <- residualize_over_grid(grid = x, model = model)
    expect_equal(
      head(round(residual_data$predicted, 5)),
      c(21, 21, 22.8, 21.4, 18.7, 18.1),
      tolerance = 1e-3
    )

    x <- ggpredict(
      model = models[[1]],
      terms = "cyl"
    )
    model <- ggeffects:::.get_model_object(x)
    residual_data <- residualize_over_grid(grid = x, model = model)
    expect_equal(
      head(round(residual_data$predicted, 5)),
      c(21, 21, 22.8, 21.4, 18.7, 18.1),
      tolerance = 1e-3
    )
  })
}
