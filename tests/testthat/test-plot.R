if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("sjlabelled") &&
  require("sjmisc")
)) {
  context("ggeffects, plot")

  # lm, linear regression ----

  data(efc)
  efc$c172code <- to_label(efc$c172code)
  fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

  test_that("ggpredict, lm", {
    pr <- ggpredict(fit, "c12hour [20,30,40]")
    p <- plot(pr)
    p <- plot(pr, ci = FALSE)
    p <- plot(pr, ci = TRUE, ci.style = "dot")
    p <- plot(pr, rawdata = TRUE)
    p <- plot(pr, rawdata = TRUE, jitter = FALSE)
    p <- plot(pr, colors = "bw")
    p <- plot(pr, colors = "gs")

    pr <- ggpredict(fit, c("c12hour", "c172code"))
    p <- plot(pr)
    p <- plot(pr, ci = FALSE)
    p <- plot(pr, ci = TRUE, ci.style = "dot")
    p <- plot(pr, rawdata = TRUE)
    p <- plot(pr, rawdata = TRUE, jitter = 0)
    p <- plot(pr, facets = TRUE)
    p <- plot(pr, facets = FALSE)
    p <- plot(pr, use.theme = FALSE)
    p <- plot(pr, colors = "bw")
    p <- plot(pr, colors = "gs")
  })

}
