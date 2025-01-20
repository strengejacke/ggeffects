skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("datawizard")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("lme4")
skip_if_not_installed("withr")

test_that("ggpredict, condition", {
  data(efc, package = "ggeffects")
  efc$grp <- datawizard::to_factor(efc$e15relat)
  efc$e42dep <- datawizard::to_factor(efc$e42dep)
  efc$c172code <- datawizard::to_factor(efc$c172code)
  focal <- c("c12hour [20,30,40]", "c172code")
  at_list <- list(
    c12hour = c(20, 30, 40),
    c172code = levels(efc$c172code)
  )

  model <- lme4::lmer(neg_c_7 ~ c12hour + e42dep + c161sex + c172code + (1 | grp), data = efc)
  out1 <- ggaverage(model, focal)
  out2 <- suppressWarnings(marginaleffects::avg_predictions(model, variables = at_list, re.form = NULL))

  expect_equal(out1$predicted, out2$estimate, tolerance = 1e-4)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4)
  expect_snapshot(print(out1))

  # valid type arguments
  expect_error(ggaverage(model, focal, type = "random"), regex = "`type = \"random\"` is not supported")
  expect_error(ggaverage(model, focal, type = "link"), regex = "`type = \"link\"` is not supported")
  expect_error(predict_response(model, focal, margin = "average", type = "random"), regex = "`type = \"random\"` is not supported")
  expect_error(predict_response(model, focal, margin = "average", type = "link"), regex = "`type = \"link\"` is not supported")

  model <- lm(neg_c_7 ~ c12hour + e42dep + c161sex + c172code, data = efc)
  out1 <- ggaverage(model, focal)
  out2 <- marginaleffects::avg_predictions(model, variables = at_list)

  expect_equal(out1$predicted, out2$estimate, tolerance = 1e-4)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4)

  out1 <- ggaverage(model, focal, ci_level = NA)
  expect_named(out1, c("x", "predicted", "group"))

  # vcov
  skip_if_not_installed("sandwich")
  out3 <- ggaverage(model, focal, vcov = "HC0")
  expect_equal(
    out3$conf.low,
    c(10.9112, 11.2802, 11.5731, 11.0061, 11.3741, 11.6526, 11.0946, 11.4556, 11.7258),
    tolerance = 1e-4
  )
})


withr::with_environment(
  new.env(),
  test_that("ggaverage, glm", {
    data(efc, package = "ggeffects")
    efc$neg_c_7d <- as.numeric(efc$neg_c_7 > median(efc$neg_c_7, na.rm = TRUE))
    efc$c172code <- datawizard::to_factor(efc$c172code)
    efc$e42dep <- datawizard::to_factor(efc$e42dep)
    d <- efc
    model <- glm(
      neg_c_7d ~ c12hour + e42dep + c161sex + c172code,
      data = d,
      family = binomial(link = "logit")
    )

    focal <- c("c12hour [20,30,40]", "c172code")
    at_list <- list(
      c12hour = c(20, 30, 40),
      c172code = levels(efc$c172code)
    )

    out1 <- ggaverage(model, focal)
    out2 <- marginaleffects::avg_predictions(model, variables = at_list, type = "invlink(link)")

    expect_equal(out1$predicted, out2$estimate, tolerance = 1e-4)
    expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4)

    expect_snapshot(print(out1))
    expect_silent(ggaverage(model, focal, type = "link"))
    expect_silent(predict_response(model, focal, margin = "average", type = "link"))
    expect_silent(predict_response(model, focal, margin = "average", type = "fixed"))
    expect_error(predict_response(model, focal, margin = "average", type = "probs"))
  })
)
