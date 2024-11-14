# PROJECT DE DATA MINING AVEC R

#@ AUTORS: BATIOMOKO BARRO, PAFADNAM IBRAHIM

# Problématique
## Nous souhaitons analyser la cybersécurité des différents pays en fonction de leurs indices, en utilisant une analyse factorielle pour réduire la dimensionnalité et en regroupant les pays en clusters pour identifier des patterns ou groupes similaires en termes de cybersécurité.

## Chargement des bibliothèques
library(tidyverse)
library(readr)
library(corrplot)
library(ggplot2)
library(reshape2)
library(FactoMineR)
library(factoextra)


##  Lecture et présentation du jeu de données

## Chargement du dataset
cyber_data <- read.csv("/home/barro/Téléchargements/Cyber_security.csv")
df <- cyber_data

#Affichage de la data
View(df)

## Visualisation des premières colones
head(df)

## Explication du jeu de données

### Country: Le nom du pays.
### Region: La région géographique à laquelle le pays appartient.
### CEI (Cybersecurity Expenditure Index): Un indice mesurant les dépenses en cybersécurité dans chaque pays.
### GCI (Global Cybersecurity Index): Un indice global évaluant la préparation et la capacité des pays à faire face aux cybermenaces.
### NCSI (National Cyber Security Index): Un indice national évaluant la cybersécurité d’un pays en fonction de divers facteurs.
### DDL (Digital Development Level): Un indicateur du niveau de développement numérique du pays.

## Obtenir des informations générales sur le dataset
glimpse(df)

# ANALYSE STATISTIQUE

## Statistiques descriptives de base
summary(df)

## Vérifier les valeurs manquantes
sum(is.na(df))

## Vérifier les doublons
sum(duplicated(df))

## Afficher les colonnes avec des valeurs manqunates
colSums(is.na(df))

## Statistiques descriptive de base pour les  indicateurs CEI, GCI, NCSI et DDL
summary(df[, c("CEI", "GCI", "NCSI", "DDL")])

## Remplacer les valeurs manquantes par la médiane
df$CEI[is.na(df$CEI)] <- 0.4830
df$GCI[is.na(df$GCI)] <- 53.15
df$NCSI[is.na(df$NCSI)] <- 40.26
df$DDL[is.na(df$DDL)] <- 51.79

## Visualisations

## Histogramme pour chaque indicateur
ggplot(df, aes(x = CEI)) + 
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") + 
  ggtitle("Histogramme de CEI") + 
  theme_minimal()

ggplot(df, aes(x = GCI)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") + 
  ggtitle("Histogramme de GCI") + 
  theme_minimal()

ggplot(df, aes(x = NCSI)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") + 
  ggtitle("Histogramme de NCSI") + 
  theme_minimal()

ggplot(df, aes(x = DDL)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") + 
  ggtitle("Histogramme de DDL") + 
  theme_minimal()

## Boites à moustaches pour les indicateurs
ggplot(melt(df[, c("CEI", "GCI", "NCSI", "DDL")]), aes(x = variable, y = value)) + 
  geom_boxplot(fill = "lightgreen") + 
  ggtitle("Boites à moustaches des Indicateurs") + 
  theme_minimal() + 
  xlab("Indicateurs") + 
  ylab("Valeurs")

## Etude de corrélations
cor_matrix <- cor(df[, c("CEI", "GCI", "NCSI", "DDL")])
print(cor_matrix) # Affiche la matrice de corrélation

## Visualiser la matrice de corrélations
corrplot(cor_matrix, method = "color", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45, 
         title = "Matrice de Corrélations")


# ANALYSE FACTORIELLE (ACP)

## Normalisation des données pour exclure les colonnes non numérique comme les noms de pays
df_numeric <- df %>%
  select_if(is.numeric) %>%
  scale()

## Vérifier les données normalisées
head(df_numeric)

## Réalisation de l'ACP
acp_result <- PCA(df_numeric, graph = FALSE)

## Visualisation des résultats
fviz_pca_var(acp_result, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

## Analyse des résultats de l'ACP

## Contributions  des variables aux deux premières dimensions
fviz_contrib(acp_result, choice = "var", axes = 1:2, top = 10)

## Visualisation  des individus (pays) sur les deux premier axes
fviz_pca_ind(acp_result, geom.ind = "point", pointsize = 3,
             col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)



# CLUSTERING : K-MEANS

## Déterminer le nombre optimal de clusters
fviz_nbclust(df_numeric, kmeans, method = "wss")

## Appliquer le clustering k-means
set.seed(123)
kmeans_result <- kmeans(df_numeric, centers = 3, nstart = 25)
df$Cluster <- as.factor(kmeans_result$cluster)

## Visualiser les clusters
fviz_cluster(kmeans_result, data = df_numeric,
             geom = "point", ellipse.type = "convex",
             main = "Clustering des profils de cybersécurité")

## Analyse des clusters
cluster_summary <- df %>%
  group_by(Cluster) %>%
  summarise(across(c(CEI, GCI, NCSI, DDL), mean))

print(cluster_summary)

## Interprétation des clusters
for (i in 1:3) {
  cat("Cluster", i, ":\n")
  cat("Pays inclus :", paste(df$Country[df$Cluster == i], collapse = ", "), "\n")
  cat("Moyenne CEI :", mean(df$CEI[df$Cluster == i]), "\n")
  cat("Moyenne GCI :", mean(df$GCI[df$Cluster == i]), "\n")
  cat("Moyenne NCSI :", mean(df$NCSI[df$Cluster == i]), "\n")
  cat("Moyenne DDL :", mean(df$DDL[df$Cluster == i]), "\n\n")
}


## Les regions et indicateurs
region <- df %>%
  group_by(Region) %>%
  summarise(across(c(CEI, GCI, DDL), mean))

print(region)
