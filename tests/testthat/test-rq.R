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
  expect_null(ggemmeans(m1, "Air.Flow", verbose = FALSE))
})
