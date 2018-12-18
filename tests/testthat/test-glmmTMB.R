context("ggeffects, glmmTMB")

library(ggeffects)

# glmmTMB ----

library(glmmTMB)
data(Owls)

m1 <- glmmTMB(SiblingNegotiation ~ SexParent + ArrivalTime + (1 | Nest), data = Owls, family = nbinom1)
m2 <- glmmTMB(SiblingNegotiation ~ SexParent + ArrivalTime + (1 | Nest), data = Owls, family = nbinom2)
m4 <- glmmTMB(SiblingNegotiation ~ FoodTreatment + ArrivalTime + SexParent + (1 | Nest), data = Owls, ziformula =  ~ 1, family = truncated_poisson(link = "log"))

test_that("ggpredict, glmmTMB", {
  ggpredict(m1, c("ArrivalTime", "SexParent"))
  ggpredict(m2, c("ArrivalTime", "SexParent"))
  ggpredict(m4, c("FoodTreatment", "ArrivalTime [21,24,30]", "SexParent"))
  ggpredict(m1, c("ArrivalTime", "SexParent"), type = "re")
  ggpredict(m2, c("ArrivalTime", "SexParent"), type = "re")
  ggpredict(m4, c("FoodTreatment", "ArrivalTime [21,24,30]", "SexParent"), type = "re")
})

m3 <- glmmTMB(count ~ spp + mined + (1 | site), ziformula = ~ spp + mined, family = truncated_poisson, data = Salamanders)
m4 <- glmmTMB(count ~ spp + mined + (1 | site), ziformula = ~ spp + mined + (1 | site), family = truncated_poisson, data = Salamanders)

test_that("ggpredict, glmmTMB", {
  p1 <- ggpredict(m3, "mined", type = "fe")
  p2 <- ggpredict(m3, "mined", type = "fe.zi")
  p3 <- ggpredict(m3, "mined", type = "re")
  p4 <- ggpredict(m3, "mined", type = "re.zi")
  expect_gt(p3$std.error[1], p1$std.error[1])
  expect_gt(p4$std.error[1], p2$std.error[1])
})

test_that("ggpredict, glmmTMB", {
  p1 <- ggpredict(m4, "mined", type = "fe")
  p2 <- ggpredict(m4, "mined", type = "fe.zi")
  p3 <- ggpredict(m4, "mined", type = "re")
  p4 <- ggpredict(m4, "mined", type = "re.zi")
  expect_gt(p3$std.error[1], p1$std.error[1])
  expect_gt(p4$std.error[1], p2$std.error[1])

  p1 <- ggpredict(m4, c("spp", "mined"), type = "fe")
  p2 <- ggpredict(m4, c("spp", "mined"), type = "fe.zi")
  p3 <- ggpredict(m4, c("spp", "mined"), type = "re")
  p4 <- ggpredict(m4, c("spp", "mined"), type = "re.zi")
  expect_gt(p3$std.error[1], p1$std.error[1])
  expect_gt(p4$std.error[1], p2$std.error[1])
})

data(efc_test)

m5 <- glmmTMB(
  negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
  data = efc_test, ziformula = ~ c172code,
  family = binomial(link = "logit")
)

test_that("ggpredict, glmmTMB", {
  ggpredict(m5, "c161sex", type = "fe")
  ggpredict(m5, "c161sex", type = "fe.zi")
  ggpredict(m5, "c161sex", type = "re")
  ggpredict(m5, "c161sex", type = "re.zi")
})


data(efc_test)

m6 <- glmmTMB(
  negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
  data = efc_test,
  family = binomial(link = "logit")
)

test_that("ggpredict, glmmTMB", {
  ggpredict(m6, "c161sex", type = "fe")
  ggpredict(m6, "c161sex", type = "re")
})


data(efc_test)

efc_test$tot_sc_e <- as.numeric(efc_test$tot_sc_e)
efc_test$c172code <- as.factor(efc_test$c172code)

m7 <- glmmTMB(
  tot_sc_e ~ neg_c_7 * c172code + c161sex + (1 | grp),
  data = efc_test, ziformula = ~ c172code,
  family = nbinom1
)

test_that("ggpredict, glmmTMB", {
  ggpredict(m7, "neg_c_7")
  ggpredict(m7, "neg_c_7 [all]")
  ggpredict(m7, "neg_c_7", type = "fe.zi")
  ggpredict(m7, "neg_c_7 [all]", type = "fe.zi")

  ggpredict(m7, c("neg_c_7", "c172code"))
  ggpredict(m7, c("neg_c_7 [all]", "c172code"))
  ggpredict(m7, c("neg_c_7", "c172code"), type = "fe.zi")
  ggpredict(m7, c("neg_c_7 [all]", "c172code"), type = "fe.zi")
})


m8 <- glmmTMB(
  tot_sc_e ~ neg_c_7 * c172code + (1 | grp),
  data = efc_test, ziformula = ~ c172code,
  family = nbinom1
)

test_that("ggpredict, glmmTMB", {
  ggpredict(m8, "neg_c_7")
  ggpredict(m8, "neg_c_7 [all]")
  ggpredict(m8, "neg_c_7", type = "fe.zi")
  ggpredict(m8, "neg_c_7 [all]", type = "fe.zi")

  ggpredict(m8, c("neg_c_7", "c172code"))
  ggpredict(m8, c("neg_c_7 [all]", "c172code"))
  ggpredict(m8, c("neg_c_7", "c172code"), type = "fe.zi")
  ggpredict(m8, c("neg_c_7 [all]", "c172code"), type = "fe.zi")
})
