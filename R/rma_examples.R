library(metafor)
library(ggeffects)


#### maintenant, on a plus d'erreur sur le script qu'on a utilisé plus haut pour un facteur : (voir ci-dessous)

data <- dat.bcg
data$alloc <- factor(data$alloc)

data_effets <- escalc(
  measure = "RR",
  ai = tpos, bi = tneg,
  ci = cpos, di = cneg,
  data = data
)

modele_rma <- rma(yi, vi, mods = ~ alloc, data = data_effets)

ggpredict(modele_rma, terms = "alloc")


#### on peut essayer avec une variable quantitative également :

# Calcul des tailles d'effet yi et vi
data_effets_quanti <- escalc(
  measure = "RR",
  ai = tpos, bi = tneg,
  ci = cpos, di = cneg,
  data = data
)

# modèle avec variable quantitative
modele_rma_q <- rma(yi, vi, mods = ~ ablat, data = data_effets_quanti)

# ce qu'on aimerait voir fonctionner
ggpredict(modele_rma_q, terms = "ablat")
