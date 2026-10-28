# BioAssayLab — Modélisation Dose-Réponse & Calcul d'IC50

**BioAssayLab** est une application web interactive développée en R Shiny dédiée au traitement des données d'essais cellulaires et d'inhibition enzymatique. Elle automatise l'ajustement de modèles log-logistiques à 4 paramètres (4PL) et l'extraction des métriques pharmacologiques clés.

### 💡 Objectifs & Problématique
En R&D pharmaceutique et en toxicologie, l'analyse manuelle des courbes dose-réponse est source d'erreurs et de perte de temps. BioAssayLab offre un pipeline reproductible pour passer d'un fichier de données brutes à un rapport d'analyse complet en quelques clics.


<img width="1727" height="968" alt="image" src="https://github.com/user-attachments/assets/7c7220b7-db71-4860-b7f4-663035fc2a14" />
Image illustrant l'interface de BioAssayLab 

### ⚙️ Fonctionnalités clés
* **Modélisation non linéaire :** Ajustement robuste par modèle 4PL (`drc`).
* **Estimation de puissance :** Calcul automatique de l'IC10, IC50 et IC90 avec leurs intervalles de confiance (méthode Delta).
* **Visualisation dynamique :** Génération de courbes sigmoïdes sur échelle logarithmique (`ggplot2`) avec repères visuels d'IC50.
* **Jeu de démonstration :** Module de simulation intégré pour tester l'application sans importer de fichier externe.

### 🛠️ Stack technique
* **Langage :** R
* **Framework Web :** Shiny, `bslib`
* **Modélisation & Stats :** `drc`
* **Visualisation :** `ggplot2`
