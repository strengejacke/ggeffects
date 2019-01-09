.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest) {

  if (suppressWarnings(
    require("testthat") &&
    require("brms") &&
    require("ggeffects")
  )) {
    context("ggeffects, brms-trial")

    data("epilepsy")
    data(efc)

    bprior1 <- prior(student_t(5,0,10), class = b) + prior(cauchy(0,2), class = sd)

    m1 <- brm(
      count ~ log_Age_c + log_Base4_c * Trt + (1|patient),
      data = epilepsy,
      family = poisson(),
      prior = bprior1,
      chains = 1,
      iter = 500
    )

    f1 <- bf(neg_c_7 ~ e42dep + c12hour + c172code)
    f2 <- bf(c12hour ~ c172code)
    m2 <- brm(f1 + f2 + set_rescor(FALSE), data = efc, chains = 1, iter = 500)

    dat <- read.table(header = TRUE, text = "
      n r r/n group treat c2 c1 w
      62 3 0.048387097 1 0 0.1438 1.941115288 1.941115288
      96 1 0.010416667 1 0 0.237 1.186583128 1.186583128
      17 0 0 0 0 0.2774 1.159882668 3.159882668
      41 2 0.048780488 1 0 0.2774 1.159882668 3.159882668
      212 170 0.801886792 0 0 0.2093 1.133397521 1.133397521
      143 21 0.146853147 1 1 0.1206 1.128993008 1.128993008
      143 0 0 1 1 0.1707 1.128993008 2.128993008
      143 33 0.230769231 0 1 0.0699 1.128993008 1.128993008
      73 62 1.260273973 0 1 0.1351 1.121927228 1.121927228
      73 17 0.232876712 0 1 0.1206 1.121927228 1.121927228"
    )
    dat$treat <- as.factor(dat$treat)

    m3 <- brm(r | trials(n) ~ treat * c2, data = dat, family = binomial(link = logit))

    test_that("ggpredict, brms-ppd", {
      ggpredict(m1, c("log_Base4_c", "Trt"))
      ggpredict(m2, "c172code")
      ggpredict(m3, c("treat", "c2"))
    })

    test_that("ggpredict, brms-ppd", {
      p1 <- ggpredict(m1, c("log_Base4_c", "Trt"))
      p2 <- ggemmeans(m1, c("log_Base4_c", "Trt"))
      expect_equal(p1$predicted[1], p2$predicted[1], tolerance = 1e-5)
    })

  }
}
