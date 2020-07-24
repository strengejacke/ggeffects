.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest) {

  if (suppressWarnings(
    require("testthat") &&
    require("lme4") &&
    require("sjmisc") &&
    requireNamespace("rstanarm") &&
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
      chains = 2, cores = 1, seed = 12345, iter = 500
    )

    m2 <- rstanarm::stan_glmer(
      Rdicho ~ Days + age + (1 | Subject),
      data = sleepstudy, QR = TRUE,
      family = binomial,
      chains = 2, iter = 500
    )

    m3 <- rstanarm::stan_glm(
      tot_sc_e ~ neg_c_7 + e42dep + barthtot + c172code + c161sex,
      data = efc,
      family = poisson("log"),
      chains = 2, iter = 500
    )

    test_that("ggpredict, rstan", {
      ggpredict(m, "Days")
      ggpredict(m, c("Days", "age"))
      ggpredict(m, "Days", type = "re")
      ggpredict(m, c("Days", "age"), type = "re")
      ggpredict(m, "Days", ppd = TRUE)
      ggpredict(m, c("Days", "age"), ppd = TRUE)
      ggpredict(m, "Days", type = "re", ppd = TRUE)
      ggpredict(m, c("Days", "age"), type = "re", ppd = TRUE)
    })

    test_that("ggpredict, rstan", {
      ggpredict(m2, "Days")
      ggpredict(m2, c("Days", "age"))
      ggpredict(m2, "Days", type = "re")
      ggpredict(m2, c("Days", "age"), type = "re")
      ggpredict(m2, "Days", ppd = TRUE)
      ggpredict(m2, c("Days", "age"), ppd = TRUE)
      ggpredict(m2, "Days", type = "re", ppd = TRUE)
      ggpredict(m2, c("Days", "age"), type = "re", ppd = TRUE)
    })

    test_that("ggpredict, rstan", {
      ggpredict(m3, "neg_c_7")
      ggpredict(m3, c("neg_c_7", "e42dep"))
      ggpredict(m3, "neg_c_7", ppd = TRUE)
      ggpredict(m3, c("neg_c_7", "e42dep"), ppd = TRUE)
    })
  }
}
