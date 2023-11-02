skip_on_cran()
skip_on_os(c("mac", "solaris"))

skip_if_not_installed("lme4")
skip_if_not_installed("datawizard")

set.seed(123)

dat <- data.frame(
  outcome = rbinom(n = 100, size = 1, prob = 0.35),
  var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
  var_cont = rnorm(n = 100, mean = 10, sd = 7),
  group = sample(letters[1:4], size = 100, replace = TRUE)
)

dat$var_cont <- datawizard::standardise(dat$var_cont)

m1 <- lme4::glmer(
  outcome ~ var_binom + var_cont + (1 | group),
  data = dat,
  family = binomial(link = "logit")
)

test_that("se-sorting", {
  pr <- ggpredict(m1, "var_cont")
  expect_equal(pr$predicted, c(0.336719595864838, 0.343075324628438, 0.349487808877511, 0.355955205473809,
                               0.362475595153102, 0.369046984214646, 0.375667306420833, 0.38233442510547,
                               0.389046135488166, 0.395800167191347, 0.402594186955394, 0.409425801546448,
                               0.41629256085041), tolerance = 1e-4)
  expect_equal(pr$std.error, c(0.618699912753018, 0.526519784780116, 0.441130838598037, 0.367300396177996,
                               0.313309075157131, 0.290440016857388, 0.305758174839891, 0.354345242445446,
                               0.424938667902817, 0.508453560698829, 0.599513975290497, 0.695161003669588,
                               0.793738286055424), tolerance = 1e-4)
})


m2 <- lme4::glmer(
  outcome ~ var_binom * poly(var_cont, degree = 2, raw = TRUE) + (1 | group),
  data = dat,
  family = binomial(link = "logit")
)

test_that("se-sorting", {
  pr <- ggpredict(m2, c("var_cont", "var_binom"))
  expect_equal(
    pr$predicted[1:10],
    c(
      0.166784864613204, 0.0615489873135249, 0.224095606762232, 0.141279183688389,
      0.281091987683061, 0.250512693421789, 0.331840641853432, 0.354819954638596,
      0.371819795779451, 0.422913790544266
    ),
    tolerance = 1e-4
  )
  expect_equal(
    pr$std.error[1:10],
    c(
      1.34423391467447, 3.65581221675649, 0.920590886385926, 2.34007695224355,
      0.595294475516507, 1.35709636952096, 0.384285954721907, 0.760109860798146,
      0.302556537107688, 0.594810096113016
    ),
    tolerance = 1e-4
  )
})
