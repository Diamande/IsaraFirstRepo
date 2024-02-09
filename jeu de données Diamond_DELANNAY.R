##essai graphismes avec données Diamond

library(ggplot2)
library(dplyr)
data("diamonds")
names(diamonds)

## 1. Nuage de points de la Distribution des prix des diamants par carats
ggplot(diamonds, aes(x = carat, y = price, color = price)) +
  geom_point(size = 3) +
  labs(title = "Distribution des prix des diamants par carats",
       x = "Carats", y = "Prix") +
  scale_color_continuous(name = "Prix")+
  theme_minimal()
# -> Plus les carats augmentent et plus le prix augmente

## 2. Boxplot de la Distribution des prix des diamants par groupes de carats
# Création d'une nouvelle variable "carat_group" en regroupant les carats
diamonds$carat_group <- cut(diamonds$carat, breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, Inf), labels = c("0-0.5", "0.5-1", "1-1.5", "1.5-2", "2-2.5", "2.5-3", "3+"))
ggplot(diamonds, aes(x = carat_group, y = price, fill = carat_group)) +
  geom_boxplot() +
  labs(title = "Distribution des prix des diamants par groupes de carats",
       x = "Groupe de carats", y = "Prix") +
  theme_minimal()
# -> Plus les carats augmentent et plus le prix augmente

## 3. Boxplot de la Distribution des dimensions (x, y) en fonction du prix
# Création d'une nouvelle variable "prix_group" en regroupant les prix
diamonds$prix_group <- cut(diamonds$price, breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000, Inf), labels = c("0-2500", "2500-5000", "5000-7500", "7500-10000", "10000-12500", "12500-15000", "15000+"))
ggplot(diamonds, aes(x = x, y = y, fill = prix_group)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 12))+  # Limiter l'axe y de 0 à 12
  labs(title = "Distribution des dimensions (x, y) en fonction du prix",
       x = "Taille x", y = "Taille y") +
  theme_minimal()
# -> Plus la taille augmente et plus le prix augmente
  
## 4. Boxplot de la Distribution des prix des diamants par couleur
ggplot(diamonds, aes(x = color, y = price, fill = color)) +
  geom_boxplot() +
  labs(title = "Distribution des prix des diamants par couleur",
       x = "Couleur du diamant", y = "Prix") +
  theme_minimal()
# -> Les couleurs H, I, J se vendent le plus cher

## 5. Nuage de points de la Distribution de la taille des diamants par couleur
ggplot(diamonds, aes(x = x, y = y, z = z, color = color)) +
  geom_point() +
  scale_color_manual(values = rainbow(length(unique(diamonds$color))), name = "Color")+
  coord_cartesian(ylim = c(0, 12))+
  labs(title = "Distribution de la taille des diamants par couleur",
       x = "Longueur du diamant", y = "Largeur du diamant") +
  theme_dark()
#je ne sais pas comment représenter le z
# -> Les couleurs H, I, J sont les plus gros en taille généralement

## 6. Nuage de points de la Distribution des prix et des carats des diamants par couleur
ggplot(diamonds, aes(x = price, y = carat, color = color)) +
  geom_point() +
  scale_color_manual(values = rainbow(length(unique(diamonds$color))), name = "Color") +
  coord_cartesian(ylim = c(0, 5)) +  # Limite des carats de 0 à 5
  labs(title = "Distribution des prix et des carats des diamants par couleur",
       x = "Prix du diamant", y = "Carat du diamant") +
  theme_dark()
# -> Les carats augmentent en fonction des couleurs (ordre croissant : D, E, F, G, H, I, J)

## 7. Histogramme de la Distribution du nombre vendu et du prix des diamants en fonction de sa qualité
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 500) +
  facet_wrap(~cut, ncol = 2)+
  scale_x_continuous(limits = c(0, 10000))+
  labs(title = "Distribution du nombre vendu et du prix des diamants en fonction de sa qualité",
       x = "Prix", y = "Nombre vendu") +
  theme_minimal()
# -> Plus la qualité du diamant augmente et plus le prix et le nombre de vendu augmentent

