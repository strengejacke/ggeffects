skip_if_not_installed("MASS")
newdata <- data.frame(
  y = c(602, 38, 616, 256, 21, 723, 245, 176, 89, 1614, 31, 27, 313, 251, 345),
  x = c(31, 35, 21, 30, 37, 26, 45, 21, 74, 27, 37, 37, 31, 37, 25),
  offset_1 = c(72, 50, 31, 30, 16, 25, 75, 16, 78, 40, 68, 25, 71, 52, 17)
)
moff <- MASS::glm.nb(y ~ x + offset(log(offset_1)), data = newdata)

test_that("handle transformed offset terms", {
  expect_message(
    {
      out1 <- ggpredict(moff, "x")
    },
    regex = "transformed offset"
  )
  out2 <- ggpredict(moff, "x", condition = c(offset_1 = 1))
  out3 <- ggpredict(moff, "x", condition = c(offset_1 = 2))

  expect_equal(
    head(out1$predicted),
    c(792.28838, 613.37035, 575.35071, 539.68771, 445.4225, 417.81307),
    tolerance = 1e-4
  )
  expect_equal(
    head(out2$predicted),
    c(17.84433, 13.81465, 12.95835, 12.15513, 10.03204, 9.4102),
    tolerance = 1e-4
  )
  expect_equal(
    head(out3$predicted),
    c(35.68867, 27.6293, 25.9167, 24.31026, 20.06408, 18.82041),
    tolerance = 1e-4
  )
  expect_identical(attributes(out1)$offset, "offset_1")
  expect_identical(attributes(out2)$offset, "offset_1")
  expect_identical(attributes(out3)$offset, "offset_1")
})

test_that("handle transformed offset terms in ggemmeans", {
  skip_if_not_installed("emmeans")
  expect_message(
    {
      out1 <- ggemmeans(moff, "x")
    },
    regex = "transformed offset"
  )
  out2 <- ggemmeans(moff, "x", condition = c(offset_1 = 1))
  out3 <- ggemmeans(moff, "x", condition = c(offset_1 = 2))

  expect_equal(
    head(out1$predicted),
    c(792.28838, 613.37035, 575.35071, 539.68771, 445.4225, 417.81307),
    tolerance = 1e-4
  )
  expect_equal(
    head(out2$predicted),
    c(17.84433, 13.81465, 12.95835, 12.15513, 10.03204, 9.4102),
    tolerance = 1e-4
  )
  expect_equal(
    head(out3$predicted),
    c(35.68867, 27.6293, 25.9167, 24.31026, 20.06408, 18.82041),
    tolerance = 1e-4
  )
  expect_identical(attributes(out1)$offset, "offset_1")
  expect_identical(attributes(out2)$offset, "offset_1")
  expect_identical(attributes(out3)$offset, "offset_1")
})
