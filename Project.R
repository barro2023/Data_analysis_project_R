#Chargement des librairies nécessaires
library(tidyverse)
library(readr)

#Chargerment du datadet
cyber_data <- read.csv("/home/barro/Téléchargements/Cyber_security.csv")
df <- cyber_data

#ANALYSE DESCRIPTIVE
#Visualiser les premières colones
head(df)

#Obtenir des informations générales sur le dataset
glimpse(df)

#Résumer les statistiques descriptives de base
summary(df)

#Vérifier les valeurs manquantes
sum(is.na(df))

#Vérifier les doublons
sum(duplicated(df))

#Afficher les colonnes avec des valeurs manqunates
colSums(is.na(df))

#Statistiques descriptive de base pour les  indicateurs CEI, GCI, NCSI et DDL
summary(df[, c("CEI", "GCI", "NCSI", "DDL")])

#Remplacer les valeurs manquantes par la médiane
df$CEI[is.na(df$CEI)] <- 0.4830
df$GCI[is.na(df$GCI)] <- 53.15
df$NCSI[is.na(df$NCSI)] <- 40.26
df$DDL[is.na(df$DDL)] <- 51.79

#Visualisations 
library(ggplot2) #Charger les librairies nécessaires

#Histogramme pour chaque indicateur
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

#Installation du package reshape2
install.packages("reshape2")

#Chargement du package
library(reshape2)


#Boites à moustaches pour les indicateurs
ggplot(melt(df[, c("CEI", "GCI", "NCSI", "DDL")]), aes(x = variable, y = value)) + 
  geom_boxplot(fill = "lightgreen") + 
  ggtitle("Boites à moustaches des Indicateurs") + 
  theme_minimal() + 
  xlab("Indicateurs") + 
  ylab("Valeurs")


#ANALYSE FACTORIELLE(ACP)
#Normalisation des données pour exclure les colonnes non numérique comme les noms de pays
df_numeric <- df %>%
  select_if(is.numeric) %>%
  scale()

#Vérifier les données normalisées
head(df_numeric)

#Chargement des librairies pour l'ACP
library(FactoMineR)
library(factoextra)

#Réalisation de l'ACP
acp_result <- PCA(df_numeric, graph = FALSE)

#Visualisation des résultats
fviz_pca_var(acp_result, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#Analyse des résultats de l'ACP

#Contributions  des variables aux deux premières dimensions
fviz_contrib(acp_result, choice = "var", axes = 1:2, top = 10)

#Visualisation  des individus (pays) sur les deux premier axes
fviz_pca_ind(acp_result, geom.ind = "point", pointsize = 3,
             col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


#CLUSTERING : K-MEANS

#Déterminer le nombre optimal de clusters
fviz_nbclust(df_numeric, kmeans, method = "wss")

#Appliquer le k-means

#Pour la reproductibilité
set.seed(123)

#Ajuster centers selon l'analyse précedente
kmeans_result <- kmeans(df_numeric, centers = 3, nstart = 25)

#Ajouter les clusters  au dataset  original
df$cluster <- kmeans_result$cluster

#Visualisation des clusters sur les deux premiers axes de l'ACP
fviz_cluster(kmeans_result, data = df_numeric, geom = "point", stand = FALSE,
             ellipse.type = "norm", ggtheme = theme_minimal())