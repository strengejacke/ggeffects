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
  pr <- ggpredict(m1, "var_cont", verbose = FALSE)
  expect_equal(pr$predicted, c(0.336719595864838, 0.343075324628438, 0.349487808877511, 0.355955205473809,
                               0.362475595153102, 0.369046984214646, 0.375667306420833, 0.38233442510547,
                               0.389046135488166, 0.395800167191347, 0.402594186955394, 0.409425801546448,
                               0.41629256085041), tolerance = 1e-4)
  expect_equal(
    pr$std.error,
    c(
      0.61298, 0.52179, 0.43737, 0.36447, 0.31129, 0.28892, 0.30424,
      0.35236, 0.42223, 0.50488, 0.59501, 0.68971, 0.78731
    ),
    tolerance = 1e-4
  )
  nd <- data_grid(m1, "var_cont")
  pr2 <- suppressWarnings(predict(
    m1,
    newdata = nd,
    se.fit = TRUE,
    re.form = NA,
    allow.new.levels = TRUE,
    type = "link"
  ))
  expect_equal(pr$std.error, pr2$se.fit, tolerance = 1e-4, ignore_attr = TRUE)
})


m2 <- lme4::glmer(
  outcome ~ var_binom * poly(var_cont, degree = 2, raw = TRUE) + (1 | group),
  data = dat,
  family = binomial(link = "logit")
)

test_that("se-sorting", {
  pr <- ggpredict(m2, c("var_cont", "var_binom"), verbose = FALSE)
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
      1.32945, 3.5651, 0.91359, 2.27853, 0.59304, 1.32133, 0.38298,
      0.74691, 0.29904, 0.59257
    ),
    tolerance = 1e-4
  )
  nd <- data_grid(m2, c("var_cont", "var_binom"))
  pr2 <- suppressWarnings(predict(
    m2,
    newdata = nd,
    se.fit = TRUE,
    re.form = NA,
    allow.new.levels = TRUE,
    type = "link"
  ))
  expect_equal(
    pr$std.error[order(pr$group, pr$x)],
    pr2$se.fit,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
