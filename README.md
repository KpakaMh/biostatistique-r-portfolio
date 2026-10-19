# Portfolio de Biostatistique & Programmation en R 📊🧬

Bienvenue sur mon dépôt de travaux pratiques et projets d'analyse de données en biostatistique. Ce portfolio rassemble la mise en œuvre concrète de méthodes statistiques (descriptives, inférentielles et modélisation) appliquées à des problématiques biomédicales, épidémiologiques et environnementales.

---

## 📂 Sommaire des Projets & Compétences

### 01. [Variables Qualitatives & Structure de Données (Titanic)](./01-variables-qualitatives)
- **Objectifs :** Manipulation des structures de données R (`vector`, `data.frame`), gestion des valeurs manquantes (`NA`), calcul des profils lignes/colonnes et représentations bivariées (`barplot`).
- **Application :** Analyse de la répartition de la survie selon la classe et le genre sur les données du Titanic.

### 02. [Variables Quantitatives & Visualisation Exploratoire (Titanic & Ozone)](./02-variables-quantitatives)
- **Objectifs :** Estimation d'indicateurs de position ($\mu, \text{Med}$) et de dispersion ($\sigma, IQR$), fonctions de répartition empiriques (ECDF), histogrammes et boîtes à moustaches.
- **Application :** Stratification de l'âge selon le statut de survie et analyse des concentrations maximales d'ozone.

### 03. [Variables Aléatoires & Simulations Monte-Carlo](./03-variables-aleatoires-lois)
- **Objectifs :** Simulation de tirages stochastiques (`sample`, `rnorm`, `rbinom`), illustration de la Loi des Grands Nombres et modélisation probabiliste.
- **Application :** Modélisation de taux de succès d'interventions chirurgicales via la loi Binomiale.

### 04. [Propriétés des Lois, TCL & Diagnostics Q-Q Plot](./04-lois-probabilites)
- **Objectifs :** Calculs de probabilités sur distributions continues, approximation normale de la loi Binomiale (Moivre-Laplace) et validation graphique de la normalité via `qqnorm()` et `qqplot()`.
- **Application :** Dimensionnement de stocks de vaccins (gestion du risque de pénurie à 90%) et analyse de queues de distribution (Loi de Student).

### 05. [Estimation Ponctuelle & Intervalles de Confiance](./05-estimation-ponctuelle)
- **Objectifs :** Estimation par intervalle de confiance d'une moyenne $\mu$ à variance inconnue ($\sigma$ inconnu), comparaison des formules théoriques (loi $t$) avec la fonction `t.test()`, et impact du niveau de confiance ($1-\alpha$).
- **Application :** Estimation de la taille moyenne des individus et analyse stratifiée de la concentration d'ozone selon la direction du vent.

### 06. [Intervalles de Confiance en Cas Non Gaussien, Proportions & Dimensionnement](./06-intervalles-de-confiance)
- **Objectifs :** Application du Théorème Central Limite (TCL) sur grands échantillons non gaussiens, estimation de proportions (`prop.test`), IC de la variance ($\chi^2$) et calcul de taille d'échantillon $n$.
- **Application :** Analyse du diamètre tumoral (données Ferretti), étude du taux de fertilité d'œufs sous exposition et dimensionnement d'essais cliniques.

### 07. [Principes des Tests d'Hypothèses Statistiques](./07-principes-tests-hypotheses)
- **Objectifs :** Formalisation de $H_0$ et $H_1$, définition des régions critiques unilatérales/bilatérales aux risques $\alpha = 5\%$ et $1\%$, calcul de la statistique $Z_{obs}$ et détermination des $p$-values.
- **Application :** Évaluation de seuils de risque clinique et contrôle qualité de conditionnements pharmaceutiques.

### 08. [Tests de Conformité d'une Moyenne en Population Gaussienne](./08-tests-de-conformite)
- **Objectifs :** Mise en œuvre du test $Z$ (variance connue) et du test $t$ de Student unilatéral/bilatéral (`t.test`), interprétation des $p$-values et stratification par sous-groupes.
- **Application :** Évaluation de l'IMC (BMI) sur cohorte clinique, dosage des D-dimères selon l'âge et hydrophobie des protéines selon le règne biologique.

### 09. [Analyse de Variance (ANOVA 1 & 2 Facteurs avec Interaction)](./09-analyse-de-variance)
- **Objectifs :** Validation des hypothèses (Shapiro-Wilk, test de homoscédasticité de Bartlett), décomposition théorique de la variance ($SCT = SCI + SCE$), ANOVA à 2 facteurs avec terme d'interaction (`alim * dose`) et tests post-hoc de Bonferroni.
- **Application :** Comparaison de la croissance bactérienne (UFC de BCG) selon le milieu et analyse de rendements agronomiques.

---

## 🛠️ Environnement & Outils
- **Langage principal :** R
- **Environnement de développement :** RStudio / Git / GitHub
- **Bibliothèques R clés :** Base R (`stats`, `graphics`), `tinytex`
