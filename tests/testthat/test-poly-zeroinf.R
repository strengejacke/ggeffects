if (suppressWarnings(
  require("testthat") &&
  require("ggeffects") &&
  require("glmmTMB") &&
  require("pscl") &&
  getRversion() >= "4.0.0"
)) {

  data(Salamanders)

  m1 <- glmmTMB(
    count ~ spp + poly(cover, 3) + mined + (1 | site),
    ziformula = ~DOY,
    dispformula = ~spp,
    data = Salamanders,
    family = nbinom2
  )

  m2 <- suppressWarnings(glmmTMB(
    count ~ spp + poly(cover, 3) + mined + (1 | site),
    ziformula = ~poly(DOY, 3),
    dispformula = ~spp,
    data = Salamanders,
    family = nbinom2
  ))

  m3 <- zeroinfl(count ~ spp + poly(cover, 3) + mined | DOY, data = Salamanders)
  m4 <- zeroinfl(count ~ spp + poly(cover, 3) + mined | poly(DOY, 3), data = Salamanders)

  test_that("ggpredict, glmmTMB", {
    pr <- ggpredict(m1, c("cover", "mined", "spp"), type = "fe.zi")
    expect_equal(ncol(pr), 7)
    expect_equal(
      colnames(pr),
      c("x", "predicted", "std.error", "conf.low", "conf.high", "group", "facet")
    )

    pr <- ggpredict(m1, c("mined", "spp"), type = "fe.zi")
    expect_equal(ncol(pr), 6)

    pr <- ggpredict(m2, c("cover", "mined", "spp"), type = "fe.zi")
    expect_equal(ncol(pr), 7)

    pr <- ggpredict(m2, c("mined", "spp"), type = "fe.zi")
    expect_equal(ncol(pr), 6)

    pr <- ggpredict(m3, c("mined", "spp"), type = "fe.zi")
    expect_equal(ncol(pr), 6)

    pr <- ggpredict(m3, c("cover", "mined", "spp"), type = "fe.zi")
    expect_equal(ncol(pr), 7)

    pr <- ggpredict(m4, c("mined", "spp"), type = "fe.zi")
    expect_equal(ncol(pr), 6)

    pr <- ggpredict(m4, c("cover", "mined", "spp"), type = "fe.zi")
    expect_equal(ncol(pr), 7)
  })
}
