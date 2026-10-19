# ==============================================================================
# TP 2 : Analyse Descriptive des Variables Quantitatives
# Description : Calcul d'indicateurs de dispersion, fonctions de répartition, 
#               et étude des relations entre variables quantitatives/qualitatives.
# ==============================================================================

# ==============================================================================
# EXERCICE 1 : Base de données TITANIC (Âge et Survie)
# ==============================================================================

# 1. Chargement du jeu de données 
TI <- read.table(file = "data/titanic.csv", header = TRUE, sep = ";")

# 2. Affectation des variables
P <- TI$pclass
S <- TI$survived
G <- TI$gender
A <- TI$age

# Note : On considère la variable Âge (A) comme continue car elle présente 
# un grand nombre de modalités différentes.

# 3. Calculer les indicateurs de position et dispersion pour l'âge
summary(A)

# 4. Variance empirique corrigée et écart-type (en excluant les valeurs manquantes NA)
var(A, na.rm = TRUE)
sd(A, na.rm = TRUE) # L'écart-type est dans la même unité que la variable (années)

# 5. Filtrage : Passagers de plus de 40 ans et moins de 20 ans
# On crée un vecteur sans NA pour ne pas fausser les proportions
A_clean <- A[!is.na(A)] 

table(A_clean > 40)
table(A_clean < 20)

# Proportions
prop.table(table(A_clean > 40))
prop.table(table(A_clean < 20))

# 6. Quantiles : 5% les plus jeunes et 5% les plus âgés
quantile(A, 0.05, na.rm = TRUE)
quantile(A, 0.95, na.rm = TRUE)

# 7. Histogramme de l'âge avec repères statistiques
hist(A, 
     main = "Distribution de l'âge des passagers du Titanic",
     xlab = "Âge (années)", ylab = "Fréquence",
     col = "lightgray", border = "white")
abline(v = mean(A, na.rm = TRUE), col = "red", lwd = 2)             # Moyenne
abline(v = median(A, na.rm = TRUE), col = "blue", lwd = 2, lty = 2) # Médiane
abline(v = quantile(A, 0.25, na.rm = TRUE), col = "green", lwd = 2) # Q1
abline(v = quantile(A, 0.75, na.rm = TRUE), col = "green", lwd = 2) # Q3
legend("topright", legend = c("Moyenne", "Médiane", "Q1 & Q3"), 
       col = c("red", "blue", "green"), lwd = 2, lty = c(1, 2, 1))

# 8. Fonction de répartition empirique (ECDF)
plot(ecdf(A_clean), 
     main = "Fonction de répartition empirique de l'âge",
     xlab = "Âge", ylab = "Probabilité cumulée", col = "darkgray")
abline(h = 0.5, col = "red", lty = 2)
abline(v = median(A, na.rm = TRUE), col = "red", lty = 2)
abline(h = 0.25, col = "blue", lty = 3)
abline(v = quantile(A, 0.25, na.rm = TRUE), col = "blue", lty = 3)
abline(h = 0.75, col = "green", lty = 3)
abline(v = quantile(A, 0.75, na.rm = TRUE), col = "green", lty = 3)

# 9. Boîte à moustaches de l'échantillon global
boxplot(A, 
        main = "Boîte à moustaches de l'âge",
        ylab = "Âge", col = "bisque")
abline(h = mean(A, na.rm = TRUE), col = "red", lwd = 2, lty = 2) # Ajout moyenne

# 10. Indicateurs de l'âge selon la classe de survie
by(A, S, summary)

# 11. Boîtes à moustaches de l'âge selon la survie
boxplot(A ~ S, 
        main = "Distribution de l'âge selon le statut de survie",
        names = c("Décédé", "Survivant"),
        col = c("firebrick", "dodgerblue"),
        xlab = "Statut", ylab = "Âge")

# 12. Étude approfondie de la répartition par survie
Ano <- A[S == 0 & !is.na(A)] # S == 0 (ou "no" selon le fichier)
Ayes <- A[S == 1 & !is.na(A)] # S == 1 (ou "yes" selon le fichier)

# Superposition des fonctions de répartition (Survie vs Décès)
plot(ecdf(Ano), col = "red", main = "Fonction de répartition : Décès vs Survie", xlab = "Âge", ylab = "Fn(x)")
lines(ecdf(Ayes), col = "green")
legend("bottomright", legend = c("Décédés", "Survivants"), col = c("red", "green"), lwd = 2)
# Interprétation : La courbe des survivants est plus haute pour les petites valeurs, 
# indiquant une sur-représentation des enfants parmi les survivants.

# 13. Analyse de l'âge selon la classe de voyage
table(P)
barplot(table(P), main = "Effectif par classe", col = "darkseagreen", xlab = "Classe", ylab = "Effectif")

by(A, P, summary)

boxplot(A ~ P, 
        main = "Distribution de l'âge selon la classe du passager",
        xlab = "Classe de voyage", ylab = "Âge",
        col = c("gold", "red", "violet"))

# 14. Fonctions de répartition empiriques par classe
A1 <- A[P == 1 & !is.na(A)]
A2 <- A[P == 2 & !is.na(A)]
A3 <- A[P == 3 & !is.na(A)]

plot(ecdf(A1), col = "red", main = "Fonction de répartition par classe", xlab = "Âge", ylab = "Fn(x)")
lines(ecdf(A2), col = "blue")
lines(ecdf(A3), col = "green")
legend("bottomright", legend = c("1ère Classe", "2ème Classe", "3ème Classe"), col = c("red", "blue", "green"), lwd = 2)


# ==============================================================================
# EXERCICE 2 : Base de données OZONE (Variables environnementales)
# ==============================================================================

# 1. Charger la base de données
oz <- read.table("data/ozone.csv", header = TRUE, sep = ";")

# 2. Dimensions du jeu de données (lignes, colonnes)
dim(oz)

# 3. Extraction de la variable Maximum d'Ozone
MO3 <- oz$maxO3
summary(MO3)

# 4. Représentations graphiques de la variable maxO3
par(mfrow = c(1, 2)) # Sépare la fenêtre graphique en 2 colonnes
boxplot(MO3, main = "Boîte à moustaches (maxO3)", ylab = "Concentration maxO3", col = "lightblue")
hist(MO3, main = "Histogramme (maxO3)", xlab = "Concentration maxO3", col = "lightblue")
par(mfrow = c(1, 1)) # Réinitialise l'affichage graphique

# 5. Ozone en fonction de la pluie
P_oz <- oz$pluie
by(MO3, P_oz, summary)

boxplot(MO3 ~ P_oz, 
        main = "Concentration maximale d'ozone selon la pluie",
        xlab = "Pluie", ylab = "Ozone max",
        names = c("Sec", "Pluvieux"), # Adapter selon les modalités réelles du CSV
        col = c("orange", "cadetblue"))
# Interprétation : Fort lien de dépendance. L'ozone est nettement plus faible en cas de pluie.

# 6. Ozone en fonction du vent
V <- oz$vent
by(MO3, V, summary)

boxplot(MO3 ~ V, 
        main = "Concentration maximale d'ozone selon le secteur du vent",
        xlab = "Direction du vent", ylab = "Ozone max",
        col = c("pink", "lightgreen", "lightblue", "khaki"))
# Interprétation : Les médianes et dispersions varient selon le vent (dépendance des variables).

# 7. Relation entre les deux variables qualitatives (Vent et Pluie)
prop.table(table(P_oz, V))             # Proportions globales
prop.table(table(P_oz, V), margin = 1) # Distribution conditionnelle du vent sachant la pluie

barplot(prop.table(table(V, P_oz), margin = 2), 
        beside = TRUE, 
        main = "Répartition du vent selon les conditions de pluie",
        xlab = "Pluie", ylab = "Proportion",
        legend.text = TRUE, col = topo.colors(4))

# 8. Relation entre deux quantitatives continues (Ozone jour J vs jour J-1)
MO3v <- oz$maxO3v

plot(MO3v, MO3, 
     main = "Relation entre l'Ozone de la veille et du jour",
     xlab = "Max Ozone Veille (maxO3v)", 
     ylab = "Max Ozone Jour (maxO3)",
     pch = 16, col = "darkblue")

# Bonus biostatistique : Ajout d'une droite de tendance linéaire
abline(lm(MO3 ~ MO3v), col = "red", lwd = 2)

