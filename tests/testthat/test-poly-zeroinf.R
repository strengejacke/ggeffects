skip_on_os(c("mac", "solaris"))
skip_if_not_installed("glmmTMB")
skip_if_not_installed("pscl")

test_that("ggpredict, glmmTMB", {
  data(Salamanders, package = "glmmTMB")

  m1 <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ spp + poly(cover, 3) + mined + (1 | site),
    ziformula = ~DOY,
    dispformula = ~spp,
    data = Salamanders,
    family = glmmTMB::nbinom2
  ))

  m2 <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ spp + poly(cover, 3) + mined + (1 | site),
    ziformula = ~ poly(DOY, 3),
    dispformula = ~spp,
    data = Salamanders,
    family = glmmTMB::nbinom2
  ))

  m3 <- pscl::zeroinfl(count ~ spp + poly(cover, 3) + mined | DOY, data = Salamanders)
  m4 <- pscl::zeroinfl(count ~ spp + poly(cover, 3) + mined | poly(DOY, 3), data = Salamanders)

  pr <- ggpredict(m1, c("cover", "mined", "spp"), type = "fe.zi", verbose = FALSE)
  expect_identical(ncol(pr), 7L)
  expect_named(pr, c("x", "predicted", "std.error", "conf.low", "conf.high", "group", "facet"))

  pr <- ggpredict(m1, c("mined", "spp"), type = "fe.zi", verbose = FALSE)
  expect_identical(ncol(pr), 6L)

  pr <- suppressMessages(ggpredict(m2, c("cover", "mined", "spp"), type = "fe.zi", verbose = FALSE))
  expect_identical(ncol(pr), 7L)

  pr <- suppressMessages(ggpredict(m2, c("mined", "spp"), type = "fe.zi", verbose = FALSE))
  expect_identical(ncol(pr), 6L)

  pr <- ggpredict(m3, c("mined", "spp"), type = "fe.zi", verbose = FALSE)
  expect_identical(ncol(pr), 6L)

  pr <- ggpredict(m3, c("cover", "mined", "spp"), type = "fe.zi", verbose = FALSE)
  expect_identical(ncol(pr), 7L)

  pr <- ggpredict(m4, c("mined", "spp"), type = "fe.zi", verbose = FALSE)
  expect_identical(ncol(pr), 6L)

  pr <- ggpredict(m4, c("cover", "mined", "spp"), type = "fe.zi", verbose = FALSE)
  expect_identical(ncol(pr), 7L)
})
