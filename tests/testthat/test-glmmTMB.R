skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("lme4")

data(Owls, package = "glmmTMB")
data(Salamanders, package = "glmmTMB")

m1 <- suppressWarnings(glmmTMB::glmmTMB(
  SiblingNegotiation ~ SexParent + ArrivalTime + (1 | Nest),
  data = Owls,
  family = glmmTMB::nbinom1()
))
m2 <- glmmTMB::glmmTMB(
  SiblingNegotiation ~ SexParent + ArrivalTime + (1 | Nest),
  data = Owls,
  family = glmmTMB::nbinom2()
)
m4 <- glmmTMB::glmmTMB(
  SiblingNegotiation ~ FoodTreatment + ArrivalTime + SexParent + (1 | Nest),
  data = Owls,
  ziformula =  ~ 1,
  family = glmmTMB::truncated_poisson(link = "log")
)


test_that("validate ggpredict against predict, nbinom", {
  nd <- data_grid(m1, "SexParent")
  pr <- predict(m1, newdata = nd, type = "link", se.fit = TRUE)
  linv <- insight::link_inverse(m1)
  dof <- insight::get_df(m1, type = "wald", verbose = FALSE)
  tcrit <- stats::qt(0.975, df = dof)
  out1 <- data.frame(
    predicted = linv(pr$fit),
    conf.low = linv(pr$fit - tcrit * pr$se.fit),
    conf.high = linv(pr$fit + tcrit * pr$se.fit)
  )
  out2 <- ggpredict(m1, "SexParent")

  expect_equal(out1$predicted, out2$predicted, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$conf.high, out2$conf.high, tolerance = 1e-4, ignore_attr = TRUE)
})


test_that("validate ggpredict lmer against marginaleffects", {
  out1 <- marginaleffects::predictions(
    m1,
    variables = "SexParent",
    newdata = marginaleffects::datagrid(m1),
    vcov = FALSE
  )
  out1 <- out1[order(out1$SexParent), ]
  out2 <- ggpredict(
    m1,
    "SexParent",
    condition = c(Nest = "Oleyes"),
    type = "random"
  )
  expect_equal(
    out1$estimate,
    out2$predicted,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("ggpredict, glmmTMB", {
  expect_s3_class(ggpredict(m1, c("ArrivalTime", "SexParent")), "data.frame")
  expect_s3_class(ggpredict(m2, c("ArrivalTime", "SexParent")), "data.frame")
  expect_s3_class(ggpredict(m4, c("FoodTreatment", "ArrivalTime [21,24,30]", "SexParent")), "data.frame")
  expect_s3_class(ggpredict(m1, c("ArrivalTime", "SexParent"), type = "re"), "data.frame")
  expect_s3_class(ggpredict(m2, c("ArrivalTime", "SexParent"), type = "re"), "data.frame")
  expect_s3_class(ggpredict(m4, c("FoodTreatment", "ArrivalTime [21,24,30]", "SexParent"), type = "re"), "data.frame")

})


test_that("ggpredict, glmmTMB", {
  expect_message(ggpredict(m1, c("ArrivalTime", "SexParent"), type = "fe.zi"))
})


test_that("ggpredict, glmmTMB", {
  p1 <- ggpredict(m1, c("ArrivalTime", "SexParent"))
  p2 <- ggpredict(m2, c("ArrivalTime", "SexParent"))
  p3 <- ggemmeans(m1, c("ArrivalTime", "SexParent"))
  p4 <- ggemmeans(m2, c("ArrivalTime", "SexParent"))
  expect_equal(p1$predicted[1], p3$predicted[1], tolerance = 1e-3)
  expect_equal(p2$predicted[1], p4$predicted[1], tolerance = 1e-3)
})


m3 <- glmmTMB::glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula = ~ spp + mined,
  family = glmmTMB::truncated_poisson(),
  data = Salamanders
)
m4 <- glmmTMB::glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula = ~ spp + mined + (1 | site),
  family = glmmTMB::truncated_poisson(),
  data = Salamanders
)
m5 <- glmmTMB::glmmTMB(
  count ~ spp + mined + cover + (1 | site),
  ziformula = ~ spp + mined,
  family = glmmTMB::truncated_poisson(),
  data = Salamanders
)

test_that("ggpredict, glmmTMB", {
  p1 <- ggpredict(m3, "mined", type = "fe")
  p2 <- ggpredict(m3, "mined", type = "fe.zi")
  p3 <- ggpredict(m3, "mined", type = "re")
  p4 <- ggpredict(m3, "mined", type = "re.zi")
  expect_gt(p3$conf.high[1], p1$conf.high[1])
  expect_gt(p4$conf.high[1], p2$conf.high[1])
  expect_s3_class(ggpredict(m3, "mined", type = "fe.zi"), "data.frame")
})


test_that("ggpredict, glmmTMB", {
  p1 <- ggpredict(m5, c("mined", "spp", "cover"), type = "fe")
  p3 <- ggemmeans(m5, c("mined", "spp", "cover"), type = "fe")
  expect_equal(p1$predicted[1], p3$predicted[1], tolerance = 1e-3)

  # p2 <- ggpredict(m5, c("mined", "spp", "cover"), type = "fe.zi")
  # p4 <- ggemmeans(m5, c("mined", "spp", "cover"), type = "fe.zi")
  # expect_equal(p2$predicted[1], p4$predicted[1], tolerance = 1e-3)
})


test_that("ggpredict, glmmTMB", {
  p1 <- ggpredict(m3, "mined", type = "fe")
  p2 <- ggpredict(m3, c("mined", "spp"), type = "fe.zi")
  p3 <- ggemmeans(m3, "mined", type = "fe", condition = c(spp = "GP"))
  p4 <- ggemmeans(m3, c("mined", "spp"), type = "fe.zi")
  p5 <- ggpredict(m3, c("mined", "spp"), type = "fe")
  p6 <- ggemmeans(m3, c("mined", "spp"), type = "fe")
  expect_equal(p1$predicted[1], p3$predicted[1], tolerance = 1e-3)
  # expect_equal(p2$predicted[1], p4$predicted[1], tolerance = 1e-3)
  expect_equal(p5$predicted[1], p6$predicted[1], tolerance = 1e-3)
})


test_that("ggpredict, glmmTMB", {
  p1 <- ggpredict(m4, "mined", type = "fe")
  p2 <- ggpredict(m4, "mined", type = "fe.zi")
  p3 <- ggpredict(m4, "mined", type = "re")
  p4 <- ggpredict(m4, "mined", type = "re.zi")
  expect_gt(p3$conf.high[1], p1$conf.high[1])
  expect_gt(p4$conf.high[1], p2$conf.high[1])

  p1 <- ggpredict(m4, c("spp", "mined"), type = "fe")
  p2 <- ggpredict(m4, c("spp", "mined"), type = "fe.zi")
  p3 <- ggpredict(m4, c("spp", "mined"), type = "re")
  p4 <- ggpredict(m4, c("spp", "mined"), type = "re.zi")
  expect_gt(p3$conf.high[1], p1$conf.high[1])
  expect_gt(p4$conf.high[1], p2$conf.high[1])
})


test_that("ggpredict, glmmTMB", {
  p <- ggpredict(m3, "spp", type = "fe.zi")
  expect_true(all(p$conf.low > 0))
  set.seed(100)
  p <- ggpredict(m3, "spp", type = "fe.zi")
  expect_true(all(p$conf.low > 0))
})


test_that("ggpredict, glmmTMB-simulate", {
  expect_s3_class(ggpredict(m3, "mined", type = "sim"), "data.frame")
  expect_s3_class(ggpredict(m3, c("spp", "mined"), type = "sim"), "data.frame")
  expect_s3_class(ggpredict(m4, "mined", type = "sim"), "data.frame")
  expect_s3_class(ggpredict(m4, c("spp", "mined"), type = "sim"), "data.frame")
})


md <- glmmTMB::glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula = ~ spp + mined,
  dispformula = ~ DOY,
  family = glmmTMB::truncated_poisson(),
  data = Salamanders
)

test_that("ggpredict, glmmTMB", {
  p1 <- ggpredict(md, c("spp", "mined"), type = "fe")
  p2 <- ggpredict(md, c("spp", "mined"), type = "fe.zi")
  p3 <- suppressWarnings(ggpredict(md, c("spp", "mined"), type = "re"))
  p4 <- suppressWarnings(ggpredict(md, c("spp", "mined"), type = "re.zi"))
  expect_gt(p3$conf.high[1], p1$conf.high[1])
  expect_gt(p4$conf.high[1], p2$conf.high[1])
})

data(efc_test)

m5 <- glmmTMB::glmmTMB(
  negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
  data = efc_test, ziformula = ~ c172code,
  family = binomial(link = "logit")
)

test_that("ggpredict, glmmTMB", {
  expect_s3_class(ggpredict(m5, "c161sex", type = "fe"), "data.frame")
  expect_s3_class(ggpredict(m5, "c161sex", type = "fe.zi"), "data.frame")
  expect_s3_class(ggpredict(m5, "c161sex", type = "re"), "data.frame")
  expect_s3_class(ggpredict(m5, "c161sex", type = "re.zi"), "data.frame")
})


data(efc_test)

m6 <- glmmTMB::glmmTMB(
  negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
  data = efc_test,
  family = binomial(link = "logit")
)

test_that("ggpredict, glmmTMB", {
  expect_s3_class(ggpredict(m6, "c161sex", type = "fe"), "data.frame")
  expect_s3_class(ggpredict(m6, "c161sex", type = "re"), "data.frame")
})

test_that("validate ggpredict against predict, binomial", {
  nd <- data_grid(m6, "e42dep")
  pr <- predict(m6, newdata = nd, type = "link", se.fit = TRUE)
  linv <- insight::link_inverse(m6)
  dof <- insight::get_df(m6, type = "wald", verbose = FALSE)
  tcrit <- stats::qt(0.975, df = dof)
  out1 <- data.frame(
    predicted = linv(pr$fit),
    conf.low = linv(pr$fit - tcrit * pr$se.fit),
    conf.high = linv(pr$fit + tcrit * pr$se.fit)
  )
  out2 <- ggpredict(m6, "e42dep")

  expect_equal(out1$predicted, out2$predicted, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$conf.high, out2$conf.high, tolerance = 1e-4, ignore_attr = TRUE)
})


data(efc_test)

efc_test$tot_sc_e <- as.numeric(efc_test$tot_sc_e)
efc_test$c172code <- as.factor(efc_test$c172code)

m7 <- glmmTMB::glmmTMB(
  tot_sc_e ~ neg_c_7 * c172code + c161sex + (1 | grp),
  data = efc_test, ziformula = ~ c172code,
  family = glmmTMB::nbinom1()
)

test_that("ggpredict, glmmTMB", {
  expect_s3_class(ggpredict(m7, "neg_c_7"), "data.frame")
  expect_s3_class(ggpredict(m7, "neg_c_7 [all]"), "data.frame")
  expect_s3_class(ggpredict(m7, "neg_c_7", type = "fe.zi"), "data.frame")
  expect_s3_class(ggpredict(m7, "neg_c_7 [all]", type = "fe.zi"), "data.frame")

  expect_s3_class(ggpredict(m7, c("neg_c_7", "c172code")), "data.frame")
  expect_s3_class(ggpredict(m7, c("neg_c_7 [all]", "c172code")), "data.frame")
  expect_s3_class(ggpredict(m7, c("neg_c_7", "c172code"), type = "fe.zi"), "data.frame")
  expect_s3_class(ggpredict(m7, c("neg_c_7 [all]", "c172code"), type = "fe.zi"), "data.frame")
})


m8 <- glmmTMB::glmmTMB(
  tot_sc_e ~ neg_c_7 * c172code + (1 | grp),
  data = efc_test, ziformula = ~ c172code,
  family = glmmTMB::nbinom1()
)

test_that("ggpredict, glmmTMB", {
  expect_s3_class(ggpredict(m8, "neg_c_7"), "data.frame")
  expect_s3_class(ggpredict(m8, "neg_c_7 [all]"), "data.frame")
  expect_s3_class(ggpredict(m8, "neg_c_7", type = "fe.zi"), "data.frame")
  expect_s3_class(ggpredict(m8, "neg_c_7 [all]", type = "fe.zi"), "data.frame")

  expect_s3_class(ggpredict(m8, c("neg_c_7", "c172code")), "data.frame")
  expect_s3_class(ggpredict(m8, c("neg_c_7 [all]", "c172code")), "data.frame")
  expect_s3_class(ggpredict(m8, c("neg_c_7", "c172code"), type = "fe.zi"), "data.frame")
  expect_s3_class(ggpredict(m8, c("neg_c_7 [all]", "c172code"), type = "fe.zi"), "data.frame")
})


data(Salamanders)
m9 <- glmmTMB::glmmTMB(
  count ~ spp + cover + mined + (1 | site),
  ziformula =  ~ DOY,
  dispformula = ~ spp,
  data = Salamanders,
  family = glmmTMB::nbinom2()
)

test_that("ggpredict, glmmTMB", {
  expect_s3_class(ggpredict(m9, c("cover", "mined", "spp"), type = "fe"), "data.frame")
  expect_s3_class(ggpredict(m9, c("cover", "mined", "spp"), type = "fe.zi"), "data.frame")
  expect_s3_class(suppressWarnings(ggpredict(m9, c("cover", "mined", "spp"), type = "re")), "data.frame")
  expect_s3_class(suppressWarnings(ggpredict(m9, c("cover", "mined", "spp"), type = "re.zi")), "data.frame")
})

test_that("validate ggpredict against predict, linear, REML-fit", {
  data(sleepstudy, package = "lme4")
  # REML-fit
  m10 <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 + Days | Subject),
    data = sleepstudy,
    REML = TRUE
  )
  nd <- data_grid(m10, "Days")
  pr <- predict(m10, newdata = nd, type = "link", se.fit = TRUE)
  dof <- insight::get_df(m10, type = "wald", verbose = FALSE)
  tcrit <- stats::qt(0.975, df = dof)
  out1 <- data.frame(
    predicted = pr$fit,
    conf.low = pr$fit - tcrit * pr$se.fit,
    conf.high = pr$fit + tcrit * pr$se.fit
  )
  out2 <- ggpredict(m10, "Days", type = "random", interval = "confidence")

  expect_equal(out1$predicted, out2$predicted, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$conf.high, out2$conf.high, tolerance = 1e-4, ignore_attr = TRUE)

  # ML-fit
  m11 <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 + Days | Subject),
    data = sleepstudy,
    REML = FALSE
  )
  nd <- data_grid(m11, "Days")
  pr <- predict(m11, newdata = nd, type = "link", se.fit = TRUE)
  dof <- insight::get_df(m11, type = "wald", verbose = FALSE)
  tcrit <- stats::qt(0.975, df = dof)
  out1 <- data.frame(
    predicted = pr$fit,
    conf.low = pr$fit - tcrit * pr$se.fit,
    conf.high = pr$fit + tcrit * pr$se.fit
  )
  out2 <- ggpredict(m11, "Days")
  out3 <- ggpredict(m11, "Days", type = "random", interval = "confidence")

  expect_equal(out1$predicted, out2$predicted, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$predicted, out3$predicted, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$conf.low, out2$conf.low, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$conf.low, out3$conf.low, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$conf.high, out2$conf.high, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$conf.high, out3$conf.high, tolerance = 1e-4, ignore_attr = TRUE)
})
