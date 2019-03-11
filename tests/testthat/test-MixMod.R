if (require("testthat") && require("ggeffects") && require("GLMMadaptive")) {

  fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
  fish$nofish <- as.factor(fish$nofish)
  fish$livebait <- as.factor(fish$livebait)
  fish$camper <- as.factor(fish$camper)

  m1 <- GLMMadaptive::mixed_model(
    count ~ child + camper,
    random = ~ 1 | persons,
    zi_fixed = ~ child + livebait,
    zi_random = ~ 1 | persons,
    data = fish,
    family = GLMMadaptive::zi.poisson()
  )

  test_that("ggpredict", {
    p <- ggpredict(m1, c("child", "camper"), type = "fe.zi")
    expect_equal(p$predicted[1], 1.849848, tolerance = 1e-5)

    p <- ggpredict(m1, c("child", "camper"), type = "re.zi")
    expect_equal(p$predicted[1], 4.525552, tolerance = 1e-5)
  })

  test_that("ggemmeans", {
    p <- ggemmeans(m1, c("child", "camper"), type = "fe.zi")
    expect_equal(p$predicted[1], 1.906611, tolerance = 1e-5)
  })
}
