if (require("testthat") && require("ggeffects") && require("GLMMadaptive")) {

  fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
  fish$nofish <- as.factor(fish$nofish)
  fish$livebait <- as.factor(fish$livebait)
  fish$camper <- as.factor(fish$camper)

  set.seed(123)
  m1 <- GLMMadaptive::mixed_model(
    count ~ child + camper,
    random = ~ 1 | persons,
    zi_fixed = ~ child + livebait,
    zi_random = ~ 1 | persons,
    data = fish,
    family = GLMMadaptive::zi.poisson()
  )

  m2 <- GLMMadaptive::mixed_model(
    nofish ~ xb + zg,
    random = ~ 1 | persons,
    data = fish,
    family = binomial
  )

  test_that("ggpredict", {
    # this test fails on osx, but not on windows
    skip_on_cran()
    skip_on_travis()

    p <- ggpredict(m1, c("child", "camper"), type = "fe.zi")
    expect_equal(p$predicted[1], 1.849963, tolerance = 1e-3)

    p <- ggpredict(m1, c("child", "camper"), type = "re.zi")
    expect_equal(p$predicted[1], 4.525552, tolerance = 1e-3)
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, c("child", "camper"), type = "fe.zi")
    expect_equal(p$predicted[1], 1.906611, tolerance = 1e-3)
  })

  test_that("ggpredict", {
    expect_message(ggpredict(m1, c("child", "camper"), type = "fe"))
    expect_message(ggpredict(m2, "zg", type = "fe.zi"))
  })

}
