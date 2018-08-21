context("ggeffects, pscl")

library(ggeffects)

# glmmTMB ----

library(glmmTMB)
library(pscl)
data(Owls)

m1 <- zeroinfl(count ~ mined | mined, dist = "poisson", data = Salamanders)
m2 <- hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "poisson", data = Salamanders)
m3 <- hurdle(count ~ mined | mined, dist = "poisson", zero.dist = "binomial", data = Salamanders)
m4 <- hurdle(count ~ mined | mined, dist ="poisson", zero.dist = "binomial", link = "log", data = Salamanders)
m5 <- zeroinfl(count ~ mined | mined, dist = "negbin", link = "log", data = Salamanders)

test_that("ggpredict, glmmTMB", {
  ggpredict(m1, "mined", type = "fe")
  ggpredict(m1, "mined", type = "fe.zi")
  ggpredict(m2, "mined", type = "fe")
  ggpredict(m2, "mined", type = "fe.zi")
  ggpredict(m3, "mined", type = "fe")
  ggpredict(m3, "mined", type = "fe.zi")
  ggpredict(m4, "mined", type = "fe")
  ggpredict(m4, "mined", type = "fe.zi")
  ggpredict(m5, "mined", type = "fe")
  ggpredict(m5, "mined", type = "fe.zi")
})
