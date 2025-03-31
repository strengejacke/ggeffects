# Charger la bibliothèque ggplot2 pour la visualisation
library(ggplot2)

# Fonction pour ajouter la pente sur un tracé ggplot2
ajouter_pente <- function(data, var_x, var_y) {
  # Ajustement d'un modèle de régression linéaire simple
  model <- lm(y ~ x, data = data)  # Régression linéaire de y en fonction de x

  # Extraction des coefficients de la régression (intercept et pente)
  coeffs <- coef(model)
  slope <- round(coeffs[2], 2)  # Pente arrondie à 2 décimales
  intercept <- round(coeffs[1], 2)  # Ordonnée à l'origine arrondie à 2 décimales

  # Calcul de l'angle de la pente en degrés pour incliner l'étiquette
  angle_deg <- atan(slope) * 180 / pi

  # Calcul du point central pour placer le texte de l'équation
  x_mid <- mean(range(data$x))  # Milieu de l'axe X
  y_mid <- intercept + slope * x_mid + 0.5  # Position sur la droite avec léger décalage

  # Création du graphique avec ggplot2
  ggplot(data, aes(x, y)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +  # Ajout de la droite de régression
    labs(title = "Régression Linéaire", x = "X", y = "Y") +  # Labels des axes
    annotate("text",
             x = x_mid,  # Position X du texte
             y = y_mid,  # Position Y légèrement au-dessus de la droite
             label = paste("y =", slope, "* x"),
             angle = angle_deg,  # Inclinaison du texte selon la pente
             hjust = 0.5, vjust = 0,
             size = 5)  # Taille du texte
}

# ==== TEST DE LA FONCTION ====
# Génération de données fictives
set.seed(123)
n <- 100
x <- rnorm(n, mean = 50, sd = 10)
y <- 3 + 0.5 * x + rnorm(n, mean = 0, sd = 5)

# Création du data frame
data <- data.frame(x, y)

# Utilisation de la fonction pour tracer le graphique
ajouter_pente(data, "x", "y")

