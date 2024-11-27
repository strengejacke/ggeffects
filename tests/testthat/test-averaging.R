skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("MuMIn")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("betareg")

test_that("ggpredict, glmmTMB averaging", {
  library(MuMIn)
  data(FoodExpenditure, package = "betareg")
  m <- glmmTMB::glmmTMB(
    I(food / income) ~ income + (1 | persons),
    ziformula = ~1,
    data = FoodExpenditure,
    na.action = "na.fail",
    family = glmmTMB::beta_family()
  )
  set.seed(123)
  dr <- suppressMessages(MuMIn::dredge(m))
  avg <- suppressMessages(MuMIn::model.avg(object = dr, fit = TRUE))
  out <- predict_response(avg, "income", verbose = FALSE)

  expect_equal(
    out$predicted,
    c(
      0.37697, 0.3619, 0.34709, 0.33258, 0.31837, 0.3045, 0.29097,
      0.2778, 0.26501, 0.2526, 0.24058, 0.22895, 0.21773, 0.20691
    ),
    tolerance = 1e-3
  )
  expect_equal(
    out$conf.low,
    c(
      0.31458, 0.30437, 0.29375, 0.28268, 0.27111, 0.25903, 0.24651,
      0.23365, 0.22058, 0.20748, 0.19449, 0.18176, 0.1694, 0.1575
    ),
    tolerance = 1e-3
  )
})

skip_if_not_installed("withr")
withr::with_options(
  list(na.action = "na.fail"),
  test_that("ggpredict, poly averaging", {
    library(MuMIn)
    data(mtcars)
    mtcars$am <- factor(mtcars$am)

    set.seed(123)
    m <- lm(disp ~ mpg + I(mpg^2) + am + gear, mtcars)
    dr <- suppressMessages(MuMIn::dredge(m, subset = dc(mpg, I(mpg^2))))
    dr <- subset(dr, !(has(mpg) & !has(I(mpg^2))))
    mod.avg.i <- suppressMessages(MuMIn::model.avg(dr, fit = TRUE))
    out <- suppressWarnings(ggpredict(mod.avg.i, terms = c("mpg [all]", "am"), verbose = FALSE))
    expect_equal(
      out$predicted[1:5],
      c(470.40705, 460.38525, 376.43462, 366.41282, 347.1256),
      tolerance = 1e-5
    )
    expect_equal(
      out$conf.low[1:5],
      c(407.89779, 386.98966, 339.47644, 314.52591, 315.85964),
      tolerance = 1e-5
    )

    set.seed(123)
    m <- lm(disp ~ poly(mpg, 2, raw = TRUE) + am + gear, mtcars)
    dr <- suppressMessages(MuMIn::dredge(m))
    mod.avg.poly <- suppressMessages(MuMIn::model.avg(dr, fit = TRUE))
    out <- suppressWarnings(ggpredict(mod.avg.poly, terms = c("mpg [all]", "am")))
    expect_equal(
      out$predicted[1:5],
      c(470.40705, 460.38525, 376.43462, 366.41282, 347.1256),
      tolerance = 1e-5
    )
    expect_equal(
      out$conf.low[1:5],
      c(407.89779, 386.98966, 339.47644, 314.52591, 315.85964),
      tolerance = 1e-5
    )
  })
)

try(unloadNamespace("MuMIn"), silent = TRUE)
