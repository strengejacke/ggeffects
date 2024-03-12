skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("GLMMadaptive")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("pscl")
skip_if_not_installed("emmeans")

data(fish, package = "ggeffects")

set.seed(123)
m1 <- GLMMadaptive::mixed_model(
  count ~ child + camper,
  random = ~ 1 | persons,
  zi_fixed = ~ child + livebait,
  zi_random = ~ 1 | persons,
  data = fish,
  family = GLMMadaptive::zi.poisson()
)

m2 <- glmmTMB::glmmTMB(
  count ~ child + camper + (1 | persons),
  ziformula = ~ child + livebait + (1 | persons),
  data = fish,
  family = poisson()
)

data(Salamanders, package = "glmmTMB")
m3 <- pscl::zeroinfl(count ~ mined | mined, dist = "poisson", data = Salamanders)

test_that("ggpredict", {
  set.seed(123)
  nd <- new_data(m1, "livebait")
  p1 <- predict(m1, newdata = nd, type_pred = "response", type = "zero_part")
  p2 <- suppressWarnings(ggpredict(m1, "livebait", type = "zi_prob"))
  expect_equal(unname(p1), p2$predicted, tolerance = 1e-3)
})

test_that("ggpredict", {
  set.seed(123)
  nd <- new_data(m2, "livebait")
  p1 <- predict(m2, newdata = nd, type = "zprob")
  p2 <- suppressWarnings(ggpredict(m2, "livebait", type = "zi_prob"))
  expect_equal(unname(p1), p2$predicted, tolerance = 1e-3)
})

test_that("ggpredict", {
  set.seed(123)
  nd <- new_data(m3, "mined")
  p1 <- predict(m3, newdata = nd, type = "zero")
  p2 <- suppressWarnings(ggpredict(m3, "mined", type = "zi_prob"))
  expect_equal(unname(p1), p2$predicted, tolerance = 1e-3)
})

test_that("ggpredict", {
  set.seed(123)
  p3 <- suppressWarnings(ggemmeans(m3, "mined", type = "zi_prob"))
  expect_equal(p3$predicted, c(0.8409091, 0.3809524), tolerance = 1e-3)
})

test_that("ggpredict pscl, sandwich", {
  skip_if_not_installed("sandwich")
  data(Salamanders, package = "glmmTMB")
  m1 <- pscl::zeroinfl(count ~ mined | mined, dist = "poisson", data = Salamanders)
  out <- ggpredict(
    m1,
    "mined",
    type = "count",
    vcov_fun = "vcovCL",
    vcov_type = "HC0",
    vcov_args = list(cluster = Salamanders$site)
  )
  expect_named(out, c("x", "predicted", "std.error", "conf.low", "conf.high", "group"))
  expect_equal(out$conf.low, c(1.08279, 3.06608), tolerance = 1e-3)
  expect_message(expect_message({
    out <- ggpredict(
      m1,
      "mined",
      type = "count",
      vcov_fun = "vcovCR",
      vcov_type = "CR0",
      vcov_args = list(cluster = Salamanders$site)
    )
  }, regex = "robust"), regex = "variance-covariance")
  expect_named(out, c("x", "predicted", "group"))
  expect_null(out$conf.low)
})
