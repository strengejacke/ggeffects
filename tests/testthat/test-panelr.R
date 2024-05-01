skip_on_os(c("mac", "solaris"))
skip_if_not_installed("panelr")
skip_if_not_installed("lme4")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("ggpredict", {
    set.seed(123)
    dt <- as.data.frame(datasets::ChickWeight)
    dt$x <- runif(nrow(dt), 0, 10)
    dt$Diet <- as.factor(dt$Diet)
    dt <<- dt[complete.cases(dt), ]
    pss <<- panelr::panel_data(dt, id = Chick, wave = Time)
    m1 <- panelr::wbm(weight ~ x | Diet, data = pss)
    out <- suppressWarnings(ggpredict(m1, "x"))
    expect_equal(
      out$predicted,
      c(
        88.39701, 89.57662, 90.75622, 91.93583, 93.11544, 94.29505,
        95.47465
      )
    )
  })
)
