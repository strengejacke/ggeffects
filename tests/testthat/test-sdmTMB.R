skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("sdmTMB")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("mgcv")

test_that("ggpredict for sdmTMB without random fields validates against glm()", {
  data(pcod_2011, package = "sdmTMB")
  pcod_2011$fyear <- as.factor(pcod_2011$year)
  fit <- sdmTMB::sdmTMB(
    present ~ poly(depth_scaled, 2) + fyear,
    data = pcod_2011,
    spatial = "off",
    family = binomial()
  )
  p <- ggpredict(fit, "depth_scaled [all]")

  fit_glm <- glm(
    present ~ poly(depth_scaled, 2) + fyear,
    data = pcod_2011,
    family = binomial()
  )
  p_glm <- ggpredict(fit_glm, "depth_scaled [all]")

  expect_equal(p$predicted, p_glm$predicted, tolerance = 1e-4)
  expect_equal(p$std.error, p_glm$std.error, tolerance = 1e-4)
  expect_equal(p$conf.low, p_glm$conf.low, tolerance = 1e-4)
  expect_equal(p$conf.high, p_glm$conf.high, tolerance = 1e-4)

  p <- ggpredict(fit, "depth_scaled [all]", back_transform = FALSE)
  p_glm <- ggpredict(fit_glm, "depth_scaled [all]", back_transform = FALSE)
  expect_equal(p$predicted, p_glm$predicted, tolerance = 1e-4)
  expect_equal(p$std.error, p_glm$std.error, tolerance = 1e-4)
  expect_equal(p$conf.low, p_glm$conf.low, tolerance = 1e-4)
  expect_equal(p$conf.high, p_glm$conf.high, tolerance = 1e-4)
})

test_that("ggpredict for sdmTMB *with* random fields returns sensible values", {
  data(pcod_2011, package = "sdmTMB")
  mesh <- sdmTMB::make_mesh(pcod_2011, c("X", "Y"), cutoff = 15)
  fit <- sdmTMB::sdmTMB(
    present ~ poly(depth_scaled, 2),
    data = pcod_2011,
    spatial = "on",
    mesh = mesh,
    family = binomial()
  )
  p <- ggpredict(fit, "depth_scaled [all]")
  expect_identical(sum(is.na(p$std.error)), 0L)
  expect_error(ggpredict(fit, "depth_scaled [all]", type = "random"), regexp = "supported")
})

test_that("ggpredict for sdmTMB delta models returns an error for now", {
  data(pcod_2011, package = "sdmTMB")
  fit <- sdmTMB::sdmTMB(
    density ~ depth_scaled,
    data = pcod_2011,
    spatial = "off",
    family = sdmTMB::delta_gamma()
  )
  expect_error(ggpredict(fit, "depth_scaled [all]"), regexp = "delta")
})

test_that("ggpredict for sdmTMB with IID random intercepts matches glmmTMB", {
  data(pcod_2011, package = "sdmTMB")
  pcod_2011$fyear <- as.factor(pcod_2011$year)
  fit <- sdmTMB::sdmTMB(
    present ~ depth_scaled + (1 | fyear),
    data = pcod_2011,
    spatial = "off",
    family = binomial()
  )
  p <- ggpredict(fit, "depth_scaled [all]")

  fit_glm <- glmmTMB::glmmTMB(
    present ~ depth_scaled + (1 | fyear),
    data = pcod_2011,
    family = binomial()
  )
  p_glm <- ggpredict(fit_glm, "depth_scaled [all]")

  expect_equal(p$predicted, p_glm$predicted, tolerance = 1e-4)
  expect_equal(p$std.error, p_glm$std.error, tolerance = 1e-4)
  expect_equal(p$conf.low, p_glm$conf.low, tolerance = 1e-4)
  expect_equal(p$conf.high, p_glm$conf.high, tolerance = 1e-4)
})

test_that("ggpredict for sdmTMB works with smoothers and matches mgcv", {
  set.seed(1)
  invisible(capture.output({
    dat <- mgcv::gamSim(3, n = 800)
  }))
  fit_gam <- mgcv::gam(y ~ s(x2, by = x1), data = dat)
  fit <- sdmTMB::sdmTMB(y ~ s(x2, by = x1), data = dat, spatial = "off")
  p <- ggpredict(fit, "x2 [all]")
  p_gam <- ggpredict(fit_gam, "x2 [all]")
  expect_equal(p$predicted, as.numeric(p_gam$predicted), tolerance = 1e-2)
})
