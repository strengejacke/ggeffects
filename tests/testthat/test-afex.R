skip_on_os(c("mac", "solaris"))
skip_if_not_installed("afex")
skip_if_not_installed("emmeans")

test_that("validate ggpredict lm against predict", {
  data(md_12.1, package = "afex")
  m <- afex::aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

  w <- data.frame(
    "None vs Some" = c(-2, 1, 1) / 2,
    "High vs Med" = c(0, 1, -1)
  )

  ems <- emmeans::emmeans(m, ~ angle + noise)
  out <- emmeans::contrast(ems, interaction = list(angle = w, noise = "pairwise"))

  ems2 <- suppressWarnings(predict_response(m, ~ angle + noise, margin = "marginalmeans"))
  out2 <- suppressWarnings(test_predictions(ems2, test = "interaction", test_args = list(angle = w, noise = "pairwise")))

  ems <- as.data.frame(ems)
  ems <- ems[order(ems$angle), ]
  expect_equal(ems$emmean, ems2$predicted, tolerance = 1e-4, ignore_attr = TRUE)

  out <- as.data.frame(out)
  expect_equal(out$estimate, rev(out2$Contrast), tolerance = 1e-4, ignore_attr = TRUE)
})
