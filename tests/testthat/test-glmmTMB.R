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

test_that("ggpredict, glmmTMB", {
  ggpredict(m3, "mined", type = "fe")
  ggpredict(m3, "mined", type = "fe.zi")
  ggpredict(m3, "mined", type = "re")
  ggpredict(m3, "mined", type = "re.zi")
})

data(efc_test)

m4 <- glmmTMB(
  negc7d ~ c12hour + e42dep + c161sex + c172code + (1 | grp),
  data = efc_test, ziformula = ~ c172code,
  family = binomial(link = "logit")
)

test_that("ggpredict, glmmTMB", {
  ggpredict(m4, "c161sex", type = "fe")
  ggpredict(m4, "c161sex", type = "fe.zi")
  ggpredict(m4, "c161sex", type = "re")
  ggpredict(m4, "c161sex", type = "re.zi")
})
