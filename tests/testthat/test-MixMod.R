.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest) {

  if (require("testthat") && require("ggeffects") && require("GLMMadaptive") && require("lme4")) {

    # fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
    # fish$nofish <- as.factor(fish$nofish)
    # fish$livebait <- as.factor(fish$livebait)
    # fish$camper <- as.factor(fish$camper)

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

    m2 <- GLMMadaptive::mixed_model(
      nofish ~ xb + zg,
      random = ~ 1 | persons,
      data = fish,
      family = binomial
    )

    test_that("ggpredict", {
      # this test fails on osx, but not on windows
      skip_on_cran()

      set.seed(123)
      expect_warning(p <- ggpredict(m1, c("child", "camper"), type = "fe.zi"))
      expect_equal(p$predicted[1], 2.045537, tolerance = 1e-2)

      set.seed(123)
      p <- ggpredict(m1, c("child", "camper"), type = "re.zi", condition = c(count = 3.296))
      expect_equal(p$predicted[1], 4.982773, tolerance = 1e-2)

      set.seed(123)
      p <- ggpredict(m1, c("child", "camper"), type = "re.zi", condition = c(count = 0))
      expect_equal(p$predicted[1], 0.5115884, tolerance = 1e-2)
    })

    test_that("ggemmeans", {
      set.seed(123)
      p <- ggemmeans(m1, c("child", "camper"), type = "fe.zi")
      expect_equal(p$predicted[1], 1.816723, tolerance = 1e-2)

      set.seed(123)
      p <- ggemmeans(m1, c("child", "camper"), type = "re.zi")
      expect_equal(p$predicted[1], 3.457011, tolerance = 1e-2)
    })

    test_that("ggpredict", {
      expect_warning(expect_message(ggpredict(m1, c("child", "camper"), type = "fe")))
      expect_warning(expect_message(ggpredict(m2, "zg", type = "fe.zi")))
    })

  }

}
