# Nettoyer l'environnement
rm(list = ls())

# Charger les packages nécessaires
library(readxl)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)
library(FactoMineR)  # Pour l'ACM

# Charger les données
file_path <- "~/data/ENSAI_24_25_DONNEES_GA_JEUNESSE_LIV.xlsx"
data <- read_excel(file_path)

# Filtrer les données pour Bayard Jeunesse
bayard_data <- data %>% filter(shop == "bayard-jeunesse.com")

# Fonction pour calculer l'âge à partir de la date de naissance
convert_to_age <- function(birth_date) {
  birth_date <- as.Date(birth_date, format = "%Y-%m-%d")

  if (is.na(birth_date)) {
    return(NA)
  }

  age <- as.integer(difftime(Sys.Date(), birth_date, units = "days") / 365.25)
  return(age)
}

# Convertir birth_date_younger en format Date et filtrer les dates valides
bayard_data <- bayard_data %>%
  mutate(birth_date_younger = as.Date(birth_date_younger, format = "%Y-%m-%d")) %>%
  filter(birth_date_younger >= as.Date("1950-01-01") & birth_date_younger <= as.Date("2025-12-31"))

# Appliquer la fonction pour calculer l'âge
bayard_data$age_younger <- sapply(bayard_data$birth_date_younger, convert_to_age)

assign_region <- function(zip_code) {
  zip_code <- as.character(zip_code)  # S'assurer que c'est du texte
  zip_code <- substr(zip_code, 1, 2)  # Prendre les 2 premiers caractères

  # Vérifier que zip_code est bien numérique
  if (!grepl("^[0-9]{2}$", zip_code)) {
    return("Autres")  # Catégorie par défaut si le code n'est pas valide
  }

  zip_code <- as.numeric(zip_code)  # Convertir en numérique pour la correspondance

  if (zip_code %in% c(75, 77, 78, 91, 92, 93, 94, 95)) {
    return("Île-de-France")
  } else if (zip_code %in% c(1, 3, 7, 15, 26, 38, 42, 43, 63, 69, 73, 74)) {
    return("Auvergne-Rhône-Alpes")
  } else if (zip_code %in% c(44, 49, 53, 72, 85)) {
    return("Pays de la Loire")
  } else if (zip_code %in% c(16, 17, 19, 23, 24, 33, 40, 47, 64, 79, 86, 87)) {
    return("Nouvelle-Aquitaine")
  } else if (zip_code %in% c(8, 10, 51, 52, 54, 55, 57, 67, 68, 88)) {
    return("Grand Est")
  } else {
    return("Autres")  # Catégorie par défaut
  }
}



# Création du dataset avec les variables pertinentes
profil_client <- bayard_data %>%
  select(transaction_revenue, age_younger, gender, zip_code) %>%
  na.omit()

# Catégorisation de transaction_revenue en 5 groupes selon les quantiles
quantiles_tr <- quantile(profil_client$transaction_revenue, probs = seq(0, 1, 0.2), na.rm = TRUE)

profil_client <- profil_client %>%
  mutate(transaction_revenue_cat = cut(transaction_revenue,
                                       breaks = quantiles_tr,
                                       include.lowest = TRUE,
                                       labels = c("Très bas", "Bas", "Moyen", "Élevé", "Très élevé")))

# Catégorisation de age_younger en 5 groupes selon les quantiles
quantiles_age <- quantile(profil_client$age_younger, probs = seq(0, 1, 0.2), na.rm = TRUE)

profil_client <- profil_client %>%
  mutate(age_younger_cat = cut(age_younger,
                               breaks = quantiles_age,
                               include.lowest = TRUE,
                               labels = c("Très jeune", "Jeune", "Adulte", "Mûr", "Senior")))

# Vérifier que zip_code est bien en format caractère
profil_client <- profil_client %>%
  mutate(zip_code = as.character(zip_code))  # S'assurer que c'est bien du texte

# Filtrer les valeurs non nulles avant d'appliquer la fonction
profil_client <- profil_client %>%
  filter(!is.na(zip_code) & nchar(zip_code) >= 2)  # Supprime les valeurs vides ou incorrectes

# Appliquer assign_region uniquement sur les zip_code valides
profil_client$region <- sapply(profil_client$zip_code, assign_region)


# Sélection des variables catégorielles pour l'ACM
profil_client_cat <- profil_client %>%
  select(transaction_revenue_cat, age_younger_cat, gender, region) %>%
  mutate(across(everything(), as.factor))  # Conversion en facteur



# Transformer en facteur
profil_client_cat$region <- as.factor(profil_client_cat$region)

# Vérifier la répartition
table(profil_client_cat$region)

# Vérification de la structure des données
str(profil_client_cat)

# Réalisation de l'ACM
acm_result <- MCA(profil_client_cat, graph = FALSE)

# Visualisation des résultats
fviz_mca_biplot(acm_result, repel = TRUE) +
  labs(title = "Analyse des Correspondances Multiples (ACM)")

fviz_mca_biplot(acm_result, repel = TRUE, label = "var") +
  labs(title = "ACM - Visualisation des Variables")


# Contribution des variables aux axes principaux
fviz_mca_var(acm_result, repel = TRUE) +
  labs(title = "Contribution des Variables - ACM")


# Calcul des quantiles de l'âge
quantiles_age <- quantile(profil_client$age_younger, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Affichage des quantiles
quantiles_age

# Créer les catégories d'âge
age_categories <- cut(profil_client$age_younger,
                      breaks = quantiles_age,
                      include.lowest = TRUE,
                      labels = c("Jeunes enfants (0-5 ans)", "Jeunes enfants (6-8 ans)", "Pré-adolescents (9-11 ans)", "Adolescents (12-17 ans)", "Adultes (18+ ans)"))

# Afficher la répartition des catégories
table(age_categories)
