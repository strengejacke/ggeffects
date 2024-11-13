skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("gamlss")

test_that("ggpredict", {
  data(iris)
  m1 <- gamlss::gamlss(
    Sepal.Length ~ Sepal.Width + gamlss::random(Species),
    sigma.formula = ~Sepal.Width,
    data = iris
  )

  p <- ggpredict(m1, "Sepal.Width")
  expect_equal(
    p$predicted,
    c(
      6.0235, 5.98971, 5.95592, 5.92213, 5.88834, 5.85455, 5.82076,
      5.78697, 5.75318, 5.71939, 5.6856, 5.65181, 5.61802
    ),
    tolerance = 1e-2
  )
  # validate against predict()
  expect_equal(
    p$predicted,
    predict(m1, newdata = data_grid(m1, "Sepal.Width")),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_named(p, c("x", "predicted", "std.error", "conf.low", "conf.high", "group"))
  expect_equal(
    p$conf.low,
    c(
      5.66308, 5.69061, 5.71514, 5.73386, 5.74045, 5.7232, 5.67382,
      5.60019, 5.51415, 5.42217, 5.32713, 5.23036, 5.13254
    ),
    tolerance = 1e-2
  )
})

skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("ggpredict, gamlss, scale-non-focal", {
    set.seed(123)
    dat <<- data.frame(
      Y = sample(20:50, 100, replace = TRUE),
      date = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by = "day"), 10),
      cont1 = rchisq(100, df = 2),
      cont2 = runif(100),
      cat1 = sample(LETTERS[1:3], 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    m <- gamlss::gamlss(
      Y ~ date + scale(cont1) + scale(cont2) + I(scale(cont2)^2) * cat1,
      data = dat
    )
    expect_warning(expect_message(
      predict_response(m, "cont2"),
      regex = "is used on"
    ))
    out <- suppressWarnings(predict_response(m, "cont2", verbose = FALSE))
    expect_equal(
      out$conf.low,
      c(-175.41823, -176.08312, -176.38544, -176.68813, -176.89848, -177.00526),
      tolerance = 1e-4,
      ignore_attr = TRUE
    )
  })
)
