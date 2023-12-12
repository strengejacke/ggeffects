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
})

skip_if_not_installed("lme4")
skip_if_not_installed("mlmRev")

test_that("ggpredict, population level", {
  # validate mgcv against lme4
  data("Contraception", package = "mlmRev")
  d_contra <<- transform(Contraception,
    use_n = as.numeric(use) - 1,
    age_sc = drop(scale(age))
  )

  m_gam <- mgcv::gam(use_n ~ poly(age_sc, 2) + urban + s(district, bs = "re"),
    family = binomial, data = d_contra, method = "ML"
  )
  m_glmer <- lme4::glmer(use_n ~ poly(age_sc, 2) + urban + (1 | district),
    family = binomial, data = d_contra
  )

  p0_gam <- ggpredict(m_gam,
    terms = c("age_sc [all]", "urban"), exclude = "s(district)",
    newdata.guaranteed = TRUE
  )
  p0_glmer <- ggpredict(m_glmer, terms = c("age_sc [all]", "urban"))

  expect_equal(
    p0_gam$predicted,
    p0_glmer$predicted,
    tolerance = 0.02,
    ignore_attr = TRUE
  )
})
