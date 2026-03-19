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
#ggeffects::ggpredict(modele_rma , terms = "alloc")

# sauf qu'on a l'erreur : 

#Error in `$<-.data.frame`(`*tmp*`, "predicted", value = c(-0.965774034173421,  : 
#replacement has 13 rows, data has 3


reg1 <- lm(Sepal.Length ~ Sepal.Width+Species,data=iris)
pre <- ggpredict(reg1)




library(metafor)
library(ggeffects)

get_predictions.rma <- function(model,
                                data_grid,
                                terms = NULL,
                                ci_level = 0.95,
                                model_info = NULL,
                                type = NULL,
                                typical = NULL,
                                vcov = NULL,
                                vcov_args = NULL,
                                condition = NULL,
                                interval = NULL,
                                link_inverse = NULL,
                                bias_correction = FALSE,
                                verbose = TRUE,
                                ...) {
  
  # On récupère la formule des modérateurs du modèle rma
  mods_formula <- eval(model$call$mods)
  
  # On construit la matrice du modèle SEULEMENT avec ces modérateurs
  X_new <- model.matrix(mods_formula, data = data_grid)
  
  # predict.rma() attend newmods sans intercept
  if ("(Intercept)" %in% colnames(X_new)) {
    newmods <- X_new[, colnames(X_new) != "(Intercept)", drop = FALSE]
  } else {
    newmods <- X_new
  }
  
  if (ncol(newmods) == 0) {
    newmods <- NULL
  }
  
  pr <- predict(
    model,
    newmods = newmods,
    level = ci_level * 100,
    ...
  )
  
  data_grid$predicted <- as.numeric(pr$pred)
  data_grid$conf.low  <- as.numeric(pr$ci.lb)
  data_grid$conf.high <- as.numeric(pr$ci.ub)
  
  data_grid
}

registerS3method("get_predictions", "rma", get_predictions.rma,
                 envir = asNamespace("ggeffects"))
registerS3method("get_predictions", "rma.uni", get_predictions.rma,
                 envir = asNamespace("ggeffects"))

# --- ton exemple ---
data <- dat.bcg
data$alloc <- factor(data$alloc)

data_effets <- escalc(
  measure = "RR",
  ai = tpos, bi = tneg,
  ci = cpos, di = cneg,
  data = data
)

modele_rma <- rma(yi, vi, mods = ~ alloc, data = data_effets)

ggeffects::ggpredict(modele_rma, terms = "alloc")

