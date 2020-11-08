.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest) {

  if (suppressWarnings(
    require("testthat") &&
    require("lme4") &&
    require("sjmisc") &&
    require("rstanarm") &&
    require("ggeffects")
  )) {
    # fit linear model
    data(sleepstudy)
    data(efc)
    sleepstudy$age <- round(runif(nrow(sleepstudy), min = 20, max = 60))
    sleepstudy$Rdicho <- dicho(sleepstudy$Reaction)
    efc <- to_label(efc, e42dep, c161sex, c172code)

    m <- rstanarm::stan_glmer(
      Reaction ~ Days + age + (1 | Subject),
      data = sleepstudy, QR = TRUE,
      # this next line is only to keep the example small in size!
      chains = 2, cores = 1, seed = 12345, iter = 500, refresh = 0
    )

    m2 <- rstanarm::stan_glmer(
      Rdicho ~ Days + age + (1 | Subject),
      data = sleepstudy, QR = TRUE,
      family = binomial,
      chains = 2, iter = 500, refresh = 0
    )

    m3 <- rstanarm::stan_glm(
      tot_sc_e ~ neg_c_7 + e42dep + barthtot + c172code + c161sex,
      data = efc,
      family = poisson("log"),
      chains = 2, iter = 500
    )

    test_that("ggpredict, rstan", {
      expect_s3_class(ggpredict(m, "Days"), "data.frame")
      expect_s3_class(ggpredict(m, c("Days", "age")), "data.frame")
      expect_s3_class(ggpredict(m, "Days", type = "re"), "data.frame")
      expect_s3_class(ggpredict(m, c("Days", "age"), type = "re"), "data.frame")
      expect_s3_class(ggpredict(m, "Days", ppd = TRUE), "data.frame")
      expect_s3_class(ggpredict(m, c("Days", "age"), ppd = TRUE), "data.frame")
      expect_s3_class(ggpredict(m, "Days", type = "re", ppd = TRUE), "data.frame")
      expect_s3_class(ggpredict(m, c("Days", "age"), type = "re", ppd = TRUE), "data.frame")
    })

    test_that("ggpredict, rstan", {
      expect_s3_class(ggpredict(m2, "Days"), "data.frame")
      expect_s3_class(ggpredict(m2, c("Days", "age")), "data.frame")
      expect_s3_class(ggpredict(m2, "Days", type = "re"), "data.frame")
      expect_s3_class(ggpredict(m2, c("Days", "age"), type = "re"), "data.frame")
      expect_s3_class(ggpredict(m2, "Days", ppd = TRUE), "data.frame")
      expect_s3_class(ggpredict(m2, c("Days", "age"), ppd = TRUE), "data.frame")
      expect_s3_class(ggpredict(m2, "Days", type = "re", ppd = TRUE), "data.frame")
      expect_s3_class(ggpredict(m2, c("Days", "age"), type = "re", ppd = TRUE), "data.frame")
    })

    test_that("ggpredict, rstan", {
      expect_s3_class(ggpredict(m3, "neg_c_7"), "data.frame")
      expect_s3_class(ggpredict(m3, c("neg_c_7", "e42dep")), "data.frame")
      expect_s3_class(ggpredict(m3, "neg_c_7", ppd = TRUE), "data.frame")
      expect_s3_class(ggpredict(m3, c("neg_c_7", "e42dep"), ppd = TRUE), "data.frame")
    })
  }
}
