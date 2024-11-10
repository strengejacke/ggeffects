skip_on_os(c("mac", "solaris"))
skip_if_not_installed("speedglm")

test_that("ggpredict, speeglm", {
  set.seed(123)
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
    c(1.57405, 1.52709, 1.48154, 1.43735, 1.39447, 1.35288),
    tolerance = 1e-3
  )
  expect_equal(
    out$predicted,
    predict(m2, newdata = data_grid(m2, "s1"), type = "response"),
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
    c(1.47518, 1.47446, 1.45823, 1.387, 1.30606, 1.22845),
    tolerance = 1e-3
  )
})
