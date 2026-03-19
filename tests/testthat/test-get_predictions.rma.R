# test_get_predictions_rma.R

library(metafor)
library(ggeffects)
library(ggplot2)

source("R/get_predictions_rma.R")

data <- dat.bcg
data$alloc <- factor(data$alloc)

data_effets <- escalc(
  measure = "RR",
  ai = tpos, bi = tneg,
  ci = cpos, di = cneg,
  data = data
)

modele_rma <- rma(yi, vi, mods = ~ alloc, data = data_effets)

# 1) Test simple : ça doit marcher sans erreur
pred_alloc <- ggpredict(modele_rma, terms = "alloc")
print(pred_alloc)

# on doit avoir une ligne par modalité de alloc
stopifnot(nrow(pred_alloc) == nlevels(data_effets$alloc))

# 2) Vérification manuelle des valeurs avec predict.rma()

grille <- data.frame(
  alloc = levels(data_effets$alloc)
)

X <- model.matrix(~ alloc, data = grille)
newmods <- X[, colnames(X) != "(Intercept)", drop = FALSE]

pred_manuel <- predict(modele_rma, newmods = newmods)

verif <- data.frame(
  alloc = grille$alloc,
  pred_ggpredict = pred_alloc$predicted,
  pred_predict_rma = pred_manuel$pred,
  low_ggpredict = pred_alloc$conf.low,
  low_predict_rma = pred_manuel$ci.lb,
  high_ggpredict = pred_alloc$conf.high,
  high_predict_rma = pred_manuel$ci.ub
)

print(verif)

stopifnot(all.equal(pred_alloc$predicted, pred_manuel$pred, tolerance = 1e-6))
stopifnot(all.equal(pred_alloc$conf.low, pred_manuel$ci.lb, tolerance = 1e-6))
stopifnot(all.equal(pred_alloc$conf.high, pred_manuel$ci.ub, tolerance = 1e-6))

# 3) Test avec variable continue

data_effets$year_centered <- scale(data_effets$year, scale = FALSE)

modele_rma_cont <- rma(yi, vi, mods = ~ year_centered, data = data_effets)
pred_cont <- ggpredict(modele_rma_cont, terms = "year_centered [all]")

print(head(pred_cont))
stopifnot(nrow(pred_cont) > 1)

# 4) Test avec deux modérateurs

modele_rma_mixte <- rma(yi, vi, mods = ~ alloc + year_centered, data = data_effets)
pred_mixte <- ggpredict(modele_rma_mixte, terms = c("year_centered [all]", "alloc"))

print(head(pred_mixte))
stopifnot(nrow(pred_mixte) > 1)

# 5) Vérification visuelle

plot(pred_alloc)

ggplot(pred_alloc, aes(x = x, y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  labs(x = "alloc", y = "prediction")

