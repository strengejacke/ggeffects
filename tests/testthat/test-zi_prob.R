.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest && getRversion() >= "4.0.0" && require("testthat") && require("ggeffects") && require("GLMMadaptive") && require("glmmTMB") && require("pscl")) {

  data(fish)

  set.seed(123)
  m1 <- GLMMadaptive::mixed_model(
    count ~ child + camper,
    random = ~ 1 | persons,
    zi_fixed = ~ child + livebait,
    zi_random = ~ 1 | persons,
    data = fish,
    family = GLMMadaptive::zi.poisson()
  )

  m2 <- glmmTMB(
    count ~ child + camper + (1 | persons),
    ziformula = ~ child + livebait + (1 | persons),
    data = fish,
    family = poisson()
  )

  data(Salamanders)
  m3 <- zeroinfl(count ~ mined | mined, dist = "poisson", data = Salamanders)

  set.seed(123)
  nd <- new_data(m1, "livebait")
  p1 <- predict(m1, newdata = nd, type_pred = "response", type = "zero_part")
  p2 <- suppressWarnings(ggpredict(m1, "livebait", type = "zi_prob"))

  test_that("ggpredict", {
    expect_equal(unname(p1), p2$predicted, tolerance = 1e-3)
  })

  set.seed(123)
  nd <- new_data(m2, "livebait")
  p1 <- predict(m2, newdata = nd, type = "zprob")
  p2 <- suppressWarnings(ggpredict(m2, "livebait", type = "zi_prob"))

  test_that("ggpredict", {
    expect_equal(unname(p1), p2$predicted, tolerance = 1e-3)
  })

  set.seed(123)
  nd <- new_data(m3, "mined")
  p1 <- predict(m3, newdata = nd, type = "zero")
  p2 <- suppressWarnings(ggpredict(m3, "mined", type = "zi_prob"))

  test_that("ggpredict", {
    expect_equal(unname(p1), p2$predicted, tolerance = 1e-3)
  })

  set.seed(123)
  p3 <- suppressWarnings(ggemmeans(m3, "mined", type = "zi_prob"))

  test_that("ggpredict", {
    expect_equal(p3$predicted, c(0.8409091, 0.3809524), tolerance = 1e-3)
  })
}
