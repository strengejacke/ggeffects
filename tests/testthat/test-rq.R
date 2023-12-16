skip_on_os(c("mac", "solaris"))
skip_if_not_installed("quantreg")
skip_if_not_installed("effects")
skip_if_not_installed("emmeans")

data(stackloss)
m1 <- quantreg::rq(stack.loss ~ Air.Flow + Water.Temp, data = stackloss, tau = 0.25)

test_that("ggpredict, rq", {
  expect_warning({
    pr <- ggpredict(m1, "Air.Flow")
  })
  expect_equal(pr$predicted[1], 10.09524, tolerance = 1e-4)
})

test_that("ggeffect, rq", {
  expect_null(ggeffect(m1, "Air.Flow", verbose = FALSE))
})

test_that("ggemmeans, rq", {
  out <- ggemmeans(m1, "Air.Flow", verbose = FALSE)
  expect_equal(out$predicted[1], 10.09524, tolerance = 1e-4)
  expect_identical(dim(out), c(7L, 6L))
})

test_that("ggemmeans, rq, multiple taus", {
  data(stackloss)
  m2 <- quantreg::rq(
    stack.loss ~ Air.Flow + Water.Temp,
    data = stackloss,
    tau = c(0.25, 0.5, 0.75)
  )
  out <- ggemmeans(m2, "Air.Flow")
})
