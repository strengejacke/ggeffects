# Charger la bibliothèque ggplot2 pour la visualisation
library(ggplot2)

# Génération de données fictives
set.seed(123)  # Fixer le générateur aléatoire pour la reproductibilité
n <- 100  # Nombre d'observations
x <- rnorm(n, mean = 50, sd = 10)  # Génération de la variable indépendante (X) selon une distribution normale
y <- 3 + 0.5 * x + rnorm(n, mean = 0, sd = 5)  # Génération de la variable dépendante (Y) avec bruit aléatoire


  # Création d'un data frame contenant les données
data <- data.frame(x, y)
ajouter_pente <- function(data, var_x, var_y) {
# Ajustement d'un modèle de régression linéaire simple
model <- lm(y ~ x, data = data)  # Régression linéaire de y en fonction de x

# Extraction des coefficients de la régression (intercept et pente)
coeffs <- coef(model)  # Récupération des coefficients
slope <- round(coeffs[2], 2)  # Pente de la droite, arrondie à 2 décimales
intercept <- round(coeffs[1], 2)  # Ordonnée à l'origine, arrondie à 2 décimales

# Calcul de l'angle de la pente en degrés pour incliner l'étiquette
angle_deg <- atan(slope) * 180 / pi

# Calcul du point central pour placer le texte de l'équation
x_mid <- mean(range(data$x))  # Milieu de l'axe X
y_mid <- intercept + slope * x_mid + 0.5  # Correspondance sur la droite avec un léger décalage vertical

# Création du graphique avec ggplot2
ggplot(data, aes(x, y)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Ajout de la droite de régression sans intervalle de confiance
  labs(title = "Régression Linéaire", x = "X", y = "Y") +  # Ajout des labels
  annotate("text",  # Ajout du texte de l'équation de la droite
           x = x_mid,  # Position X du texte (milieu de la plage de X)
           y = y_mid,  # Position Y légèrement au-dessus de la droite
           label = paste("y =", slope, "* x"),  # Contenu du texte affiché
           angle = angle_deg,  # Inclinaison du texte suivant la pente de la droite
           hjust = 0.5, vjust = 0,  # Alignement du texte
           size = 5)  # Taille du texte
}

# Utilisation de la fonction pour tracer le graphique
ajouter_pente(data, "x", "y")
