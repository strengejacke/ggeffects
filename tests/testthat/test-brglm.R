skip_on_os(c("mac", "linux", "solaris"))
skip_if_not_installed("brglm")

test_that(" ggpredict brglm", {
  data(lizards, package = "brglm")
  m <- brglm::brglm(
    cbind(grahami, opalinus) ~ height + diameter + light + time,
    family = binomial(logit),
    data = lizards,
    method = "brglm.fit"
  )
  out <- predict_response(m, "height")
  expect_equal(
    out$predicted,
    predict(m, newdata = data_grid(m, "height"), type = "response"),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(out$predicted, c(0.8701, 0.95295), tolerance = 1e-4)
  expect_equal(out$conf.low, c(0.77567, 0.90524), tolerance = 1e-4)
})
