skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("rstanarm")
skip_if_not_installed("lme4")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("ggpredict, rstan", {
    # fit linear model
    data(sleepstudy, package = "lme4")
    data(efc, package = "ggeffects")
    sleepstudy$age <- round(runif(nrow(sleepstudy), min = 20, max = 60))
    sleepstudy$Rdicho <- as.factor(as.numeric(sleepstudy$Reaction > median(sleepstudy$Reaction)))
    efc <- datawizard::to_factor(efc, c("e42dep", "c161sex", "c172code"))
    d <<- sleepstudy
    d2 <<- efc

    m <- suppressWarnings(rstanarm::stan_glmer(
      Reaction ~ Days + age + (1 | Subject),
      data = d, QR = TRUE,
      # this next line is only to keep the example small in size!
      chains = 2, cores = 1, seed = 12345, iter = 500, refresh = 0
    ))

    m2 <- suppressWarnings(rstanarm::stan_glmer(
      Rdicho ~ Days + age + (1 | Subject),
      data = d, QR = TRUE,
      family = binomial,
      chains = 2, iter = 500, refresh = 0
    ))

    m3 <- suppressWarnings(rstanarm::stan_glm(
      tot_sc_e ~ neg_c_7 + e42dep + barthtot + c172code + c161sex,
      data = d2,
      family = poisson("log"),
      chains = 2, iter = 500, refresh = 0
    ))

    expect_s3_class(ggpredict(m, "Days", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m, c("Days", "age"), verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m, "Days", type = "re", interval = "confidence", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m, c("Days", "age"), type = "re", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m, "Days", interval = "prediction", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m, c("Days", "age"), interval = "prediction", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m, "Days", type = "re", interval = "prediction", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m, c("Days", "age"), type = "re", interval = "prediction", verbose = FALSE), "data.frame")

    expect_s3_class(ggpredict(m2, "Days", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m2, c("Days", "age"), verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m2, "Days", type = "re", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m2, c("Days", "age"), type = "re", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m2, "Days", interval = "prediction", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m2, c("Days", "age"), interval = "prediction", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m2, "Days", type = "re", interval = "prediction", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m2, c("Days", "age"), type = "re", interval = "prediction", verbose = FALSE), "data.frame") # nolint

    expect_s3_class(ggpredict(m3, "neg_c_7", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m3, c("neg_c_7", "e42dep"), verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m3, "neg_c_7", interval = "prediction", verbose = FALSE), "data.frame")
    expect_s3_class(ggpredict(m3, c("neg_c_7", "e42dep"), interval = "prediction", verbose = FALSE), "data.frame")
  })
)


withr::with_environment(
  new.env(),
  test_that("ggpredict, rstanarm, ci-level", {
    data(efc, package = "ggeffects")
    fit <- rstanarm::stan_glm(barthtot ~ c12hour + neg_c_7, data = efc, seed = 1, refresh = 0)

    # 90% CI
    set.seed(1)
    out1 <- ggpredict(fit,
      terms = "c12hour[20]",
      condition = c(neg_c_7 = 11),
      interval = "prediction",
      ci_level = 0.9
    )
    set.seed(1)
    est_ppd <- rstanarm::posterior_predict(fit, newdata = data.frame(c12hour = 20, neg_c_7 = 11))
    out2 <- rstanarm::posterior_interval(est_ppd) # 90% CI
    expect_equal(
      out1$conf.low,
      as.vector(out2[1, 1]),
      tolerance = 1e-4
    )

    # 95% CI
    set.seed(1)
    out1 <- ggpredict(fit,
      terms = "c12hour[20]",
      condition = c(neg_c_7 = 11),
      interval = "prediction",
      ci_level = 0.95
    )
    set.seed(1)
    est_ppd <- rstanarm::posterior_predict(fit, newdata = data.frame(c12hour = 20, neg_c_7 = 11))
    out2 <- rstanarm::posterior_interval(est_ppd, prob = 0.95) # 90% CI
    expect_equal(
      out1$conf.low,
      as.vector(out2[1, 1]),
      tolerance = 1e-4
    )
  })
)
