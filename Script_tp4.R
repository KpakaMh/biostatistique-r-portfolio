# ==============================================================================
# TP 4 : Propriétés des Lois de Probabilités, Approximations & Diagnostic Q-Q Plot
# Description : Modélisation gaussienne, approximation normale de la loi Binomiale 
#               (Théorème Central Limite) et validation graphique des hypothèses 
#               de normalité via Q-Q Plots.
# ==============================================================================


# ==============================================================================
# EXERCICE 1 : Modélisation de Données Biométriques (Taille de la population)
# ==============================================================================

# Contexte : Modèle théorique de la taille des hommes français X ~ N(mu = 172 cm, sigma = 14 cm)
mf  <- 172
sdf <- sqrt(196) # sd = 14 cm

# 1. Proportion d'hommes mesurant moins de 160 cm : P(X < 160)
p_160 <- pnorm(160, mean = mf, sd = sdf)
cat("Proportion < 160 cm :", round(p_160, 4), "\n")

# 2. Visualisation graphique de la densité et de la fonction de répartition
par(mfrow = c(1, 2))

# Densité de la loi normale
curve(dnorm(x, mean = mf, sd = sdf), from = mf - 3 * sdf, to = mf + 3 * sdf,
      main = "Densité N(172, 14²)", xlab = "Taille (cm)", ylab = "f(x)", col = "darkblue", lwd = 2)
abline(v = mf, col = "red", lty = 2)
abline(v = 160, col = "blue", lwd = 2)

# Fonction de répartition (CDF)
curve(pnorm(x, mean = mf, sd = sdf), from = mf - 3 * sdf, to = mf + 3 * sdf,
      main = "Fonction de Répartition", xlab = "Taille (cm)", ylab = "F(x)", col = "darkblue", lwd = 2)
abline(v = 160, col = "blue", lwd = 2)
abline(h = p_160, col = "blue", lty = 2)

par(mfrow = c(1, 1)) # Réinitialisation de l'affichage

# 3. Proportion d'hommes mesurant plus de 200 cm : P(X > 200)
p_200 <- 1 - pnorm(200, mean = mf, sd = sdf)
cat("Proportion > 200 cm :", round(p_200, 4), "\n")

# 4. Proportion d'hommes mesurant entre 165 et 185 cm : P(165 < X < 185)
p_inter <- pnorm(185, mean = mf, sd = sdf) - pnorm(165, mean = mf, sd = sdf)
cat("Proportion entre 165 et 185 cm :", round(p_inter, 4), "\n")

# 5. Taille estimée du 9000-ième homme sur 10 000 (Quantile à 90%)
q_90 <- qnorm(0.90, mean = mf, sd = sdf)
cat("Taille du 90ème percentile :", round(q_90, 2), "cm\n")

# 6. Taille telle que 75% de la population est au-dessus (Quantile à 25%)
q_25 <- qnorm(0.25, mean = mf, sd = sdf)
cat("Taille minimale des 75% les plus grands :", round(q_25, 2), "cm\n")


# ==============================================================================
# EXERCICE 2 : Dimensionnement Sanitaire & Approximation Normale
# ==============================================================================

# Contexte : Campagne de vaccination sur N = 150 000 personnes avec p = 0.4
# Vraie loi : X ~ B(n = 150000, p = 0.4)
n_vac <- 150000
p_vac <- 0.4

# Paramètres de l'approximation normale N(np, np(1-p)) - Moivre-Laplace
ms  <- n_vac * p_vac                  # Espérance = 60 000
sds <- sqrt(n_vac * p_vac * (1 - p_vac)) # Écart-type = 189.74

# Risk de pénurie si 60 500 doses sont préparées : P(X > 60500)
p_penurie_binom <- 1 - pbinom(60500, size = n_vac, prob = p_vac)
p_penurie_norm  <- 1 - pnorm(60500, mean = ms, sd = sds)

cat("Risque de pénurie (Loi Binomiale) :", round(p_penurie_binom, 5), "\n")
cat("Risque de pénurie (Approx. Normale):", round(p_penurie_norm, 5), "\n")
# Interprétation : L'approximation est extrêmement précise car n*p = 60000 >> 5

# Nombre de doses pour garantir un approvisionnement suffisant à 90%
doses_binom <- qbinom(0.90, size = n_vac, prob = p_vac)
doses_norm  <- qnorm(0.90, mean = ms, sd = sds)

cat("Doses requises (Binomiale) :", doses_binom, "\n")
cat("Doses requises (Normale)   :", round(doses_norm), "\n")


# ==============================================================================
# EXERCICE 3 : Modélisation de Données Climatiques
# ==============================================================================

# 1. Janvier : Température ~ N(mu = 3.5°C, sigma = 2°C)
mj <- 3.5; sdj <- 2

pnorm(1, mean = mj, sd = sdj)     # P(T < 1°C)
1 - pnorm(5, mean = mj, sd = sdj) # P(T > 5°C)

# 2. Avril : Température ~ N(mu = 13.7°C, sigma = 3°C)
ma <- 13.7; sda <- 3

pnorm(7, mean = ma, sd = sda)     # P(T < 7°C)
1 - pnorm(15, mean = ma, sd = sda)# P(T > 15°C)

# 3. Températures extrêmes (1% inférieur et 1% supérieur)
# Janvier
qnorm(0.01, mean = mj, sd = sdj) # Seuil des 1% les plus froids
qnorm(0.99, mean = mj, sd = sdj) # Seuil des 1% les plus chauds

# Avril
qnorm(0.01, mean = ma, sd = sda)
qnorm(0.99, mean = ma, sd = sda)


# ==============================================================================
# EXERCICE 4 : Diagnostics Graphiques de Normalité (Q-Q Plots)
# ==============================================================================

set.seed(42) # Reproductibilité

# 1. Diagnostic de normalité sur données gaussiennes
X_norm100 <- rnorm(100, mean = 2, sd = 1)

qqnorm(X_norm100, main = "Q-Q Plot d'un échantillon Gaussien (n = 100)")
abline(a = 2, b = 1, col = "red", lwd = 2, lty = 2)               # Droite théorique
abline(a = mean(X_norm100), b = sd(X_norm100), col = "green", lwd = 2) # Droite empirique

# 2. Effet de la taille de l'échantillon sur l'alignement des quantiles
par(mfrow = c(1, 2))
X_200 <- rnorm(200, mean = 2, sd = 1)
qqnorm(X_200, main = "Gaussien (n = 200)")
abline(a = mean(X_200), b = sd(X_200), col = "green", lwd = 2)

X_500 <- rnorm(500, mean = 2, sd = 1)
qqnorm(X_500, main = "Gaussien (n = 500)")
abline(a = mean(X_500), b = sd(X_500), col = "green", lwd = 2)
par(mfrow = c(1, 1))

# 3. Diagnostic sur une loi non gaussienne et asymétrique (Chi-deux)
Y_chisq <- rchisq(n = 200, df = 5)

qqnorm(Y_chisq, main = "Q-Q Plot d'une loi Asymétrique (Chi-deux df=5)")
abline(a = mean(Y_chisq), b = sd(Y_chisq), col = "red", lwd = 2)
# Interprétation : La courbure caractéristique indique un écart fort à la normalité.

# 4. Comparaison directe de deux distributions via qqplot()
X_a <- rnorm(100, mean = 20, sd = sqrt(5))
Y_a <- rnorm(100, mean = 20, sd = sqrt(5))

qqplot(X_a, Y_a, main = "Comparaison de 2 échantillons Gaussiens identiques")
abline(0, 1, col = "red", lwd = 2)

# 5. Comparaison Normale vs Student (Tolérance aux queues lourdes)
# Student df = 1 (Loi de Cauchy : queues très lourdes / valeurs extrêmes)
X_norm <- rnorm(1000, mean = 0, sd = 1)
Y_t1   <- rt(1000, df = 1)

qqplot(X_norm, Y_t1, main = "Normale vs Student(df = 1) : Queues lourdes",
       xlab = "Quantiles de N(0,1)", ylab = "Quantiles de t(df=1)")
abline(0, 1, col = "red", lwd = 2)

# Student df = 100 (Convergence vers la loi Normale)
Y_t100 <- rt(1000, df = 100)

qqplot(X_norm, Y_t100, main = "Normale vs Student(df = 100) : Convergence",
       xlab = "Quantiles de N(0,1)", ylab = "Quantiles de t(df=100)")
abline(0, 1, col = "red", lwd = 2)

