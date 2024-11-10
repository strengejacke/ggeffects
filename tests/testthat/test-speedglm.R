skip_on_os(c("mac", "solaris"))
skip_if_not_installed("speedglm")

test_that("ggpredict, speedglm", {
  n <- 10000
  k <- 5
  y <- rgamma(n, 1.5, 1)
  x <- round(matrix(rnorm(n * k), n, k), digits = 3)
  colnames(x) <- paste0("s", 1:k)
  da <- data.frame(y, x)
  fo <- as.formula(paste("y~", paste(paste0("s", 1:k), collapse = "+")))

  m <- speedglm::speedglm(fo, data = da, family = Gamma(log))
  m2 <- glm(fo, data = da, family = Gamma(log))
  out <- predict_response(m, "s1")

  expect_equal(
    out$predicted,
    c(1.46128, 1.47384, 1.48652, 1.4993, 1.5122, 1.5252, 1.53832),
    tolerance = 1e-3
  )
  expect_equal(
    out$predicted,
    predict(m2, newdata = data_grid(m, "s1"), type = "response"),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    out$predicted,
    predict(m, newdata = data_grid(m, "s1"), type = "response"),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    out$conf.low,
    c(1.32374, 1.37825, 1.43335, 1.4753, 1.45869, 1.4269, 1.39415),
    tolerance = 1e-3
  )
})
