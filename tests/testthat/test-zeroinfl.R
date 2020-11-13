if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("glmmTMB") &&
  require("pscl")
)) {
  data(Salamanders)

  m1 <- zeroinfl(count ~ mined | mined, dist = "poisson", data = Salamanders)
  m2 <- hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "poisson", data = Salamanders)
  m3 <- hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "binomial", data = Salamanders)
  m4 <- hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "binomial", link = "log", data = Salamanders)
  m5 <- suppressWarnings(zeroinfl(count ~ mined | mined, dist = "negbin", link = "log", data = Salamanders))

  test_that("ggpredict, pscl", {
    expect_s3_class(ggpredict(m1, "mined", type = "fe"), "data.frame")
    expect_s3_class(ggpredict(m1, "mined", type = "fe.zi"), "data.frame")
    expect_s3_class(ggpredict(m2, "mined", type = "fe"), "data.frame")
    expect_s3_class(ggpredict(m2, "mined", type = "fe.zi"), "data.frame")
    expect_s3_class(ggpredict(m3, "mined", type = "fe"), "data.frame")
    expect_s3_class(ggpredict(m3, "mined", type = "fe.zi"), "data.frame")
    expect_s3_class(ggpredict(m4, "mined", type = "fe"), "data.frame")
    expect_s3_class(ggpredict(m4, "mined", type = "fe.zi"), "data.frame")
    expect_s3_class(ggpredict(m5, "mined", type = "fe"), "data.frame")
    expect_s3_class(ggpredict(m5, "mined", type = "fe.zi"), "data.frame")
  })

  test_that("ggpredict, pscl", {
    skip_on_cran()
    set.seed(123)
    pr <- ggpredict(m1, "mined", type = "fe.zi")
    expect_equal(pr$conf.low, c(0.1731, 2.0172), tolerance = 1e-3)

    model <- zeroinfl(count ~ mined * spp | mined * spp, dist = "poisson", data = Salamanders)
    set.seed(123)
    pr <- ggpredict(model, c("mined", "spp"), type = "fe.zi")
    expect_equal(
      pr$conf.low,
      c(0, 0, 0.03704, 1e-05, 1e-05, 0.14815, 0.13418, 1.61886,
        0.04808, 1.81329, 0.48571, 3.07055, 3.1093, 1.33136),
      tolerance = 1e-2
    )
  })

  test_that("ggemmeans, pscl", {
    expect_s3_class(ggemmeans(m1, "mined", type = "fe"), "data.frame")
    expect_s3_class(ggemmeans(m1, "mined", type = "fe.zi"), "data.frame")
    expect_s3_class(ggemmeans(m2, "mined", type = "fe"), "data.frame")
    expect_s3_class(ggemmeans(m2, "mined", type = "fe.zi"), "data.frame")
    expect_s3_class(ggemmeans(m3, "mined", type = "fe"), "data.frame")
    expect_s3_class(ggemmeans(m3, "mined", type = "fe.zi"), "data.frame")
    expect_s3_class(ggemmeans(m4, "mined", type = "fe"), "data.frame")
    expect_s3_class(ggemmeans(m4, "mined", type = "fe.zi"), "data.frame")
    expect_s3_class(ggemmeans(m5, "mined", type = "fe"), "data.frame")
    expect_s3_class(ggemmeans(m5, "mined", type = "fe.zi"), "data.frame")
  })

  test_that("compare, pscl", {
    p1 <- ggemmeans(m1, "mined", type = "fe")
    p2 <- ggpredict(m1, "mined", type = "fe")
    expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)

    p1 <- ggemmeans(m1, "mined", type = "fe.zi")
    p2 <- ggpredict(m1, "mined", type = "fe.zi")
    expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)

    p1 <- ggemmeans(m2, "mined", type = "fe")
    p2 <- ggpredict(m2, "mined", type = "fe")
    expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)

    p1 <- ggemmeans(m2, "mined", type = "fe.zi")
    p2 <- ggpredict(m2, "mined", type = "fe.zi")
    expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)

    p1 <- ggemmeans(m5, "mined", type = "fe")
    p2 <- ggpredict(m5, "mined", type = "fe")
    expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)

    p1 <- ggemmeans(m5, "mined", type = "fe.zi")
    p2 <- ggpredict(m5, "mined", type = "fe.zi")
    expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-3)
  })

  #Generate some data

  set.seed(123)
  N <- 100 #Samples
  x <- runif(N, 0, 5) #Predictor 1
  z <- runif(N, 0, 5) #Predictor 2
  off <- rgamma(N, 3, 2) #Offset variable
  yhat <- -1 + x * 0.2 + z * -0.2 + z * x * 0.2 + log(off) #Prediction on log scale
  dat <- data.frame(y = NA, x,z, logOff = log(off)) #Storage dataframe

  dat$y <- rpois(N, exp(yhat)) #Poisson process
  dat$y <- ifelse(rbinom(N, 1, 0.3), 0, dat$y) #Zero-inflation process

  #Fit zeroinfl and glm model

  #Interaction b/w x and z
  model <- zeroinfl(y ~ offset(logOff) + x * z | 1, data = dat, dist = 'poisson')

  test_that("pscl, offset, interaction and CI", {
    pr <- ggpredict(model, c("x", "z"))
    expect_equal(
      pr$conf.low,
      c(0.09963, 0.11356, 0.10043, 0.15167, 0.18376, 0.17829, 0.2295,
        0.29636, 0.31618, 0.34394, 0.47536, 0.55973, 0.50699, 0.75547,
        0.98802, 0.72668, 1.18187, 1.73484, 0.99808, 1.80482, 3.016,
        1.30413, 2.6781, 5.15324, 1.63447, 3.87834, 8.6113, 1.99384,
        5.52767, 14.12902, 2.39388, 7.80484, 22.93352),
      tolerance = 1e-3
    )
  })

}
