# ==============================================================================
# TP 3 : Variables Aléatoires, Lois de Probabilité & Simulations Monte-Carlo
# Description : Simulation de tirages aléatoires, convergence de la moyenne 
#               empirique (Loi des Grands Nombres) et applications biomédicales 
#               des lois Normale et Binomiale.
# ==============================================================================


# ==============================================================================
# EXERCICE 2 : Loi de Bernoulli & Loi des Grands Nombres
# ==============================================================================

# 1. Définition du support de la variable de Bernoulli (0 = Échec/Infection, 1 = Succès/Sain)
x <- c(0, 1)

# 2. Tirage aléatoire unique (expérience de Bernoulli de paramètre p = 0.5)
sample(x, size = 1, replace = TRUE)

# 3 & 4. Simulation d'un échantillon de taille n = 100 et calcul des proportions
n_small <- 100
E_small <- sample(x, size = n_small, replace = TRUE)

table(E_small)
prop.table(table(E_small))
mean(E_small) # La moyenne empirique équivaut à la proportion de 1

# 5. Convergence vers l'espérance théorique (n = 50 000)
# Illustration du théorème de la Loi des Grands Nombres
n_large <- 50000
E_large <- sample(x, size = n_large, replace = TRUE)
mean(E_large) # Doit tendre vers E(X) = 0.5

# 6. Bernoulli déséquilibrée / biaisée (ex. p_succès = 2/3)
E_biased <- sample(x, size = n_large, replace = TRUE, prob = c(1/3, 2/3))
mean(E_biased) # Doit tendre vers E(X) = 2/3 ≈ 0.667


# ==============================================================================
# EXERCICE 3 : Loi Normale / Gaussienne (Variables Biomédicales Continues)
# ==============================================================================

# Modèle : X ~ N(mu = 1, sigma = 2)
n_norm <- 50
mu <- 1
sig <- 2

# 1 & 2. Génération d'un échantillon de taille n = 50
set.seed(123) # Pour la reproductibilité
ech_norm <- rnorm(n_norm, mean = mu, sd = sig)
mean(ech_norm) # Moyenne empirique (estimation de mu)

# 3. Représentation graphique : Histogramme et repères théoriques vs empiriques
hist(ech_norm, 
     main = "Distribution empirique d'une variable Normale X ~ N(1, 2²)",
     xlab = "Valeurs de X", ylab = "Densité",
     probability = TRUE, col = "lightblue", border = "white")

# Superposition de la densité théorique
curve(dnorm(x, mean = mu, sd = sig), add = TRUE, col = "darkblue", lwd = 2)

# Repères visuels
abline(v = mu, col = "red", lwd = 2, lty = 1)               # Moyenne théorique
abline(v = mean(ech_norm), col = "darkgreen", lwd = 2, lty = 2) # Moyenne empirique
legend("topright", legend = c("Densité théorique", "Mu théorique", "Moyenne empirique"),
       col = c("darkblue", "red", "darkgreen"), lty = c(1, 1, 2), lwd = 2)

# 4 & 5. Comparaison des probabilités théoriques et empiriques : P(X < 2)
p_theo_less_2 <- pnorm(2, mean = mu, sd = sig)           # Théorique : ~0.6915
p_emp_less_2  <- mean(ech_norm < 2)                      # Empirique (prop.table)

cat("P(X < 2) - Théorique :", round(p_theo_less_2, 4), "| Empirique :", round(p_emp_less_2, 4), "\n")

# 6 à 9. Autres seuils d'intérêt biomédical

# P(X < 0)
pnorm(0, mean = mu, sd = sig)
mean(ech_norm < 0)

# P(X > 2) = 1 - P(X <= 2)
1 - pnorm(2, mean = mu, sd = sig)
mean(ech_norm > 2)

# P(X < 1)
pnorm(1, mean = mu, sd = sig)
mean(ech_norm < 1)

# P(X > 3)
1 - pnorm(3, mean = mu, sd = sig)
mean(ech_norm > 3)


# ==============================================================================
# EXERCICE 4 : Loi Binomiale (Modélisation d'Essais / Actes Médicaux)
# ==============================================================================

# Contexte : Succès d'un traitement sur n = 4 patients avec une probabilité p = 0.6
# Variable aléatoire : X ~ B(n = 4, p = 0.6)

n_trials <- 4
p_success <- 0.6

# 1. Calcul de la probabilité d'échec total P(X = 0)
dbinom(0, size = n_trials, prob = p_success)

# 2. Simulation de N = 100 réplications de l'essai clinique
N_small <- 100
E_binom_small <- rbinom(N_small, size = n_trials, prob = p_success)

# Distribution des fréquences observées
table(E_binom_small)
prop.table(table(E_binom_small))

# Visualisation graphique de l'échantillon
barplot(prop.table(table(E_binom_small)),
        main = "Répartition empirique du nombre de succès (N = 100)",
        xlab = "Nombre de succès (X)", ylab = "Proportion",
        col = "cadetblue", border = "white")

# 3. Grande simulation Monte-Carlo (N = 10 000) pour convergence
N_large <- 10000
E_binom_large <- rbinom(N_large, size = n_trials, prob = p_success)
prop.table(table(E_binom_large))

# 4. Probabilité d'avoir exactement 2 succès : P(X = 2)
p_exact_2_theo <- dbinom(2, size = n_trials, prob = p_success)
p_exact_2_emp  <- mean(E_binom_large == 2)

cat("P(X = 2) - Théorique :", round(p_exact_2_theo, 4), "| Empirique :", round(p_exact_2_emp, 4), "\n")

# 5. Probabilité d'avoir au plus 2 succès : P(X <= 2)
p_max_2_theo <- pbinom(2, size = n_trials, prob = p_success) # ou sum(dbinom(0:2, n_trials, p_success))
p_max_2_emp  <- mean(E_binom_large <= 2)

cat("P(X <= 2) - Théorique :", round(p_max_2_theo, 4), "| Empirique :", round(p_max_2_emp, 4), "\n")

# 6. Probabilité d'avoir au moins 3 succès : P(X >= 3)
p_min_3_theo <- 1 - pbinom(2, size = n_trials, prob = p_success) # ou pbinom(2, lower.tail = FALSE)
p_min_3_emp  <- mean(E_binom_large >= 3)

cat("P(X >= 3) - Théorique :", round(p_min_3_theo, 4), "| Empirique :", round(p_min_3_emp, 4), "\n")

