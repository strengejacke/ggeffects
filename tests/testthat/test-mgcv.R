skip_on_cran()
skip_on_os(c("mac", "solaris"))
unloadNamespace("gam")
skip_if_not_installed("mgcv")

test_that("ggpredict", {
  set.seed(0)
  dat <- mgcv::gamSim(6, n = 200, scale = 0.2, dist = "poisson")
  m1 <- mgcv::gamm(
    y ~ s(x0) + s(x1) + s(x2),
    family = poisson,
    data = dat,
    random = list(fac = ~1),
    verbosePQL = FALSE
  )
  p <- ggpredict(m1, "x1")
  expect_equal(p$predicted[1], 15.5450060160087, tolerance = 1e-3)
  expect_s3_class(ggpredict(m1, c("x1", "x2")), "data.frame")

  m2 <- mgcv::gam(
    y ~ s(x0) + s(x1) + s(x2),
    family = poisson,
    data = dat,
    verbosePQL = FALSE
  )
  p <- ggpredict(m2, "x1")
  expect_equal(p$predicted[1], 22.57359, tolerance = 1e-3)
  expect_equal(p$conf.low[1], 17.89382, tolerance = 1e-3)
})

skip_if_not_installed("lme4")

test_that("ggpredict, population level", {
  # validate mgcv against lme4
  data("sleepstudy", package = "lme4")
  m_lmer <- lme4::lmer(Reaction ~ poly(Days, 2) + (1 | Subject),
    data = sleepstudy
  )
  # equivalent model, random effects are defined via s(..., bs = "re")
  m_gam <- mgcv::gam(Reaction ~ poly(Days, 2) + s(Subject, bs = "re"),
    family = gaussian(), data = sleepstudy, method = "ML"
  )

  # predictions are identical
  p0_gam <- ggpredict(
    m_gam,
    terms = "Days [all]",
    exclude = "s(Subject)",
    newdata.guaranteed = TRUE
  )
  p0_lmer <- ggpredict(m_lmer, terms = "Days [all]")

  expect_equal(
    p0_gam$predicted,
    p0_lmer$predicted,
    tolerance = 0.02,
    ignore_attr = TRUE
  )
})
