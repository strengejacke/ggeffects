skip_on_os(c("mac", "solaris"))
skip_if_not_installed("MASS")
skip_if_not_installed("effects")
skip_if_not_installed("emmeans")
skip_if_not_installed("withr")

withr::with_options(
  list(contrasts = c("contr.treatment", "contr.poly")),
  {
    data(housing, package = "MASS")
    fit <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

    test_that("ggpredict, polr", {
      pr <- ggpredict(fit, "Infl")
      expect_equal(
        pr$predicted[c(1, 4, 7, 2, 5, 8, 3, 6, 9)],
        as.vector(do.call(rbind, as.list(predict(fit, newdata = data_grid(fit, "Infl"), type = "probs")))),
        ignore_attr = TRUE,
        tolerance = 1e-3
      )
      expect_snapshot(print(pr))
      pr <- ggpredict(fit, c("Infl", "Type"))
      pr$response.level <- factor(pr$response.level, levels = c("Low", "Medium", "High"))
      expect_equal(
        as.vector(do.call(rbind, as.list(predict(fit, newdata = data_grid(fit, c("Infl", "Type")), type = "probs")))),
        pr$predicted[order(pr$response.level, pr$group)],
        ignore_attr = TRUE,
        tolerance = 1e-3
      )
      pr <- ggpredict(fit, c("Infl", "Type", "Cont"))
      pr$response.level <- factor(pr$response.level, levels = c("Low", "Medium", "High"))
      expect_equal(
        as.vector(do.call(
          rbind,
          as.list(predict(fit, newdata = data_grid(fit, c("Infl", "Type", "Cont")), type = "probs"))
        )),
        pr$predicted[order(pr$response.level, pr$facet, pr$group)],
        ignore_attr = TRUE,
        tolerance = 1e-3
      )
    })

    test_that("ggaverage, polr, weights", {
      skip_if_not_installed("marginaleffects")
      pr <- ggaverage(fit, "Infl")
      pr2 <- marginaleffects::avg_predictions(fit, variables = "Infl")
      expect_equal(
        pr$predicted,
        c(0.45941, 0.26685, 0.27374, 0.33072, 0.27537, 0.39392, 0.19755, 0.23777, 0.56469),
        ignore_attr = TRUE,
        tolerance = 1e-3
      )
      expect_equal(
        pr$predicted[c(1, 4, 7, 2, 5, 8, 3, 6, 9)],
        pr2$estimate,
        ignore_attr = TRUE,
        tolerance = 1e-3
      )
      # test proper print output
      expect_snapshot(print(pr))
      expect_snapshot(format(pr))
      # with weights
      pr <- ggaverage(fit, "Infl", weights = "Freq")
      pr2 <- marginaleffects::avg_predictions(fit, variables = "Infl", wts = "Freq")
      expect_equal(
        pr$predicted,
        c(0.4489, 0.27129, 0.27981, 0.31999, 0.27757, 0.40244, 0.18882, 0.23581, 0.57537),
        ignore_attr = TRUE,
        tolerance = 1e-3
      )
      expect_equal(
        pr$predicted[c(1, 4, 7, 2, 5, 8, 3, 6, 9)],
        pr2$estimate,
        ignore_attr = TRUE,
        tolerance = 1e-3
      )
    })

    test_that("ggemmeans, polr", {
      pr <- ggemmeans(fit, "Infl")
      expect_identical(nrow(pr), 9L)
      pr <- ggemmeans(fit, c("Infl", "Type"))
      expect_identical(nrow(pr), 36L)
      pr <- ggemmeans(fit, c("Infl", "Type", "Cont"))
      expect_identical(nrow(pr), 72L)
    })

    test_that("ggpredict, polr", {
      pr1 <- ggpredict(fit, "Infl [Low,High]")
      expect_identical(nrow(pr1), 6L)
      pr2 <- ggpredict(fit, c("Infl [Low,High]", "Type [Tower]"))
      expect_identical(nrow(pr2), 6L)
      pr3 <- ggpredict(fit, c("Infl [Medium,Low]", "Type [Terrace]", "Cont [Low]"))
      expect_identical(nrow(pr3), 6L)
      expect_equal(pr1$predicted, pr2$predicted, tolerance = 1e-3)
    })

    test_that("ggemmeans, polr", {
      pr1 <- ggemmeans(fit, "Infl [Low,High]")
      expect_equal(pr1$predicted, c(0.45941, 0.26685, 0.27374, 0.19755, 0.23777, 0.56469), tolerance = 1e-3)
      expect_identical(nrow(pr1), 6L)
      pr2 <- ggemmeans(fit, c("Infl [Low,High]", "Type [Tower]"))
      expect_equal(pr2$predicted, c(0.33827, 0.28572, 0.37601, 0.12423, 0.19175, 0.68401), tolerance = 1e-3)
      expect_identical(nrow(pr2), 6L)
      pr3 <- ggemmeans(fit, c("Infl [Medium,Low]", "Type [Terrace]", "Cont [Low]"))
      expect_identical(nrow(pr3), 6L)
    })

    test_that("ggpredict, polr", {
      pr <- ggpredict(fit, "Infl [Low,High]", condition = c(Type = "Tower"))
      expect_identical(nrow(pr), 6L)
      pr <- ggpredict(fit, c("Infl [Low,High]", "Type [Tower]"), condition = c(Cont = "Low"))
      expect_identical(nrow(pr), 6L)
    })

    test_that("ggemmeans, polr", {
      pr <- ggemmeans(fit, "Infl [Low,High]", condition = c(Type = "Tower"))
      expect_identical(nrow(pr), 6L)
      expect_equal(pr$predicted, c(0.33827, 0.28572, 0.37601, 0.12423, 0.19175, 0.68401), tolerance = 1e-3)
      pr <- ggemmeans(fit, c("Infl [Low,High]", "Type [Tower]"), condition = c(Cont = "Low"))
      expect_identical(nrow(pr), 6L)
    })

    test_that("ggemmeans, polr", {
      p1 <- ggemmeans(fit, "Infl", condition = c(Type = "Tower", Cont = "Low"))
      p2 <- ggpredict(fit, "Infl")
      expect_equal(
        p1$predicted[p1$x == 1 & p1$response.level == "Low"],
        p2$predicted[p2$x == 1 & p2$response.level == "Low"],
        tolerance = 1e-3
      )
    })

    test_that("ggeffect, polr", {
      ggeffect(fit, "Infl")
      ggeffect(fit, c("Infl", "Type"))
      ggeffect(fit, c("Infl", "Type", "Cont"))
    })

    test_that("ggeffect, polr", {
      ggeffect(fit, "Infl [Low,High]")
      ggeffect(fit, c("Infl [Low,High]", "Type [Tower]"))
      ggeffect(fit, c("Infl [Medium,Low]", "Type [Terrace]", "Cont [Low]"))
    })
  }
)
