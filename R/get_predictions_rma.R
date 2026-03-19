#petit script du code qu'on s'attend à voir fonctionner 

library(metafor)
library(ggeffects)

data <- dat.bcg
summary(data)

# on transforme la variable alloc en facteur
data$alloc <- factor(data$alloc)


# Calcul des tailles d'effet yi et des variances vi
data_effets <- escalc(
  measure = "RR",   # par exemple log risk ratio
  ai = tpos, bi = tneg,
  ci = cpos, di = cneg,
  data = data
)

# modele de meta regression avec fonction rma 
modele_rma <- rma(yi, vi, mods = ~ alloc, data = data_effets)

# ce qui devrait marcher
ggeffects::ggpredict(modele_rma , terms = "alloc")

# sauf qu'on a l'erreur : 

#Error in `$<-.data.frame`(`*tmp*`, "predicted", value = c(-0.965774034173421,  : 
#replacement has 13 rows, data has 3



