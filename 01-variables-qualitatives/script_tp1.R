# ==============================================================================
# TP 1 : Analyse Descriptive des Variables Qualitatives & Manipulation de Données
# Description : Manipulation de vecteurs, sous-ensembles de DataFrames et 
#               analyses croisées sur le jeu de données du Titanic.
# ==============================================================================

# ==============================================================================
# EXERCICE 4 : Manipulation de vecteurs et requêtes logiques
# ==============================================================================

# 1 & 2. Création du vecteur 'jourTC' (nombre de jours d'utilisation des transports en commun)
jourTC <- c(5, 0, 2, 5, 3, 4, 5, 4, 5, 0, 0, 1, 4, 3, 5)

# 3. Création du vecteur 'locomotion' (mode de transport principal : TC, V, M)
locomotion <- c("TC", "V", "V", "TC", "TC", "TC", "TC",
                "TC", "TC", "V", "M", "V", "TC", "M", "TC")

# 5. Vérification de la taille des échantillons
length(jourTC)
length(locomotion)

# 6 & 7. Effectifs et fréquences de la variable locomotion
table(locomotion)
prop.table(table(locomotion))

# 8. Représentations graphiques de la variable locomotion
barplot(table(locomotion),
        col = "steelblue",
        xlab = "Moyen de transport",
        ylab = "Effectif",
        main = "Répartition des modes de transport")

# 10 & 11. Filtrage et tests logiques
table(jourTC >= 4)                       # Au moins 4 jours par semaine
table(jourTC == 5)                       # Strictement 5 jours par semaine
table(jourTC <= 2)                       # Au plus 2 jours par semaine
table(jourTC != 3)                       # Nombre de jours différent de 3
table(jourTC >= 2 & jourTC <= 4)         # Entre 2 et 4 jours (inclus)


# ==============================================================================
# EXERCICE 5 : Analyse des données du Titanic
# ==============================================================================

# 1. Chargement du fichier de données
TI <- read.table(file = "data/titanic.csv", header = TRUE, sep = ";")

# 2. Inspection des variables
names(TI)
str(TI)

# 3, 4 & 5. Indexation et extraction de sous-ensembles
TI[2, 3]                                 # Valeur ligne 2, colonne 3
TI[5, 4]                                 # Valeur ligne 5, colonne 4
head(TI[10, ])                           # Ligne 10 complète
TI[2:12, ]                               # Lignes 2 à 12
TI[c(100, 200), c(2, 4)]                 # Lignes 100 et 200 pour les colonnes 2 et 4

# 6 & 7. Extraction des variables dans le DataFrame
P <- TI$pclass    # Classe (1, 2, 3)
S <- TI$survived  # Survie (0 = Décédé, 1 = Survivant)
G <- TI$gender    # Genre (F, M)
A <- TI$age       # Âge

# 8 à 11. Requêtes conditionnelles sur le jeu de données
TI[P == 1, ]                             # Passagers de 1ère classe
TI[P == 3, ]                             # Passagers de 3ème classe
TI[G == "F", ]                           # Passagères femmes
TI[P == 1 & G == "F", ]                  # Femmes de 1ère classe
TI[P == 2 & G == "M", ]                  # Hommes de 2ème classe
TI[A < 1 & !is.na(A), ]                  # Nourrissons (< 1 an)
TI[A < 18 & !is.na(A), ]                 # Mineurs (< 18 ans)
TI[A >= 12 & A <= 18 & !is.na(A), ]      # Adolescents (12-18 ans)

# 12 à 14. Analyse univariée de la classe des passagers
table_P <- table(P)
prop.table(table_P)

barplot(table_P, 
        col = "darkseagreen3",
        xlab = "Classe de voyage", 
        ylab = "Effectif",
        main = "Distribution des passagers par classe")

# 15. Analyse univariée de la survie
table_S <- table(S)
prop.table(table_S)

barplot(table_S, 
        names.arg = c("Décédé", "Survivant"),
        col = c("firebrick", "dodgerblue"),
        ylab = "Effectif",
        main = "Répartition de la survie")

# 16 à 18. Analyse bivariée : Classe vs Survie
table_PS <- table(P, S)
colnames(table_PS) <- c("Décédé", "Survivant")

table_PS                                 # Effectifs conjoints
prop.table(table_PS)                     # Proportions globales
prop.table(table_PS, margin = 1)         # Profils lignes (survie par classe)
prop.table(table_PS, margin = 2)         # Profils colonnes (classe par statut de survie)

# 19 & 20. Représentations graphiques croisées
# Diagramme en barres groupées (Survie selon la classe)
barplot(table(S, P), 
        beside = TRUE,
        legend.text = c("Décédé", "Survivant"),
        col = c("firebrick", "dodgerblue"),
        xlab = "Classe de voyage",
        ylab = "Effectif",
        main = "Survie selon la classe de voyage")

# Diagramme des proportions relatives (Profils lignes)
barplot(prop.table(table(S, P), margin = 2), 
        beside = TRUE,
        legend.text = c("Décédé", "Survivant"),
        col = c("firebrick", "dodgerblue"),
        xlab = "Classe de voyage",
        ylab = "Proportion",
        main = "Taux de survie au sein de chaque classe")

# 21. Analyse croisée : Classe vs Genre
table_PG <- table(P, G)
table_PG                                 # Effectifs croisés
prop.table(table_PG, margin = 1)         # Répartition du genre dans chaque classe
prop.table(table_PG, margin = 2)         # Répartition de la classe pour chaque genre

barplot(prop.table(table_PG, margin = 2), 
        beside = TRUE,
        legend.text = c("1ère", "2ème", "3ème"),
        col = c("gold", "green", "gray"),
        xlab = "Genre",
        ylab = "Proportion",
        main = "Répartition des classes selon le genre")

