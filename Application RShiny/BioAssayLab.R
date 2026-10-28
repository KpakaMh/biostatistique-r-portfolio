
library(shiny)
library(bslib)
library(ggplot2)
library(drc)

# ==============================================================================
# 1. INTERFACE UTILISATEUR (UI)
# ==============================================================================
ui <- page_sidebar(
  title = "BioAssayLab — Modélisation Dose-Réponse & IC50",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Données & Paramètres",
    
    # Boutons d'importation
    fileInput("file", "Charger un essai (CSV)", accept = c(".csv")),
    actionButton("demo_data", "Charger un essai démo", class = "btn-primary w-100 mb-3"),
    
    hr(),
    
    # Sélection des variables
    selectInput("dose_col", "Variable Concentration / Dose :", choices = NULL),
    selectInput("resp_col", "Variable Réponse (% viabilité) :", choices = NULL),
    
    hr(),
    
    # Options d'affichage
    checkboxInput("log_scale", "Échelle logarithmique (X)", TRUE)
  ),
  
  # Navigation principale
  navset_card_tab(
    nav_panel("Courbe Sigmoïde & IC50", plotOutput("curve_plot", height = "500px")),
    nav_panel("Paramètres du Modèle (4PL)", verbatimTextOutput("ic50_summary")),
    nav_panel("Aperçu des Données", tableOutput("raw_table"))
  )
)

# ==============================================================================
# 2. LOGIQUE SERVEUR
# ==============================================================================
server <- function(input, output, session) {
  
  # Stockage réactif des données
  values <- reactiveValues(df = NULL)
  
  # Importation fichier CSV
  observeEvent(input$file, {
    values$df <- read.csv(input$file$datapath, header = TRUE, sep = ";")
  })
  
  # Simulation de données démo (Modèle sigmoïde avec bruit)
  observeEvent(input$demo_data, {
    set.seed(42)
    doses <- rep(c(0.001, 0.01, 0.1, 1, 10, 100, 1000), each = 3)
    # IC50 théorique = 1.0 µM, Viabilité min = 5%, max = 100%
    resps <- 5 + (100 - 5) / (1 + (doses / 1.0)^1.2) + rnorm(length(doses), mean = 0, sd = 3.5)
    
    values$df <- data.frame(
      Concentration_uM = doses,
      Viabilite_Cellulaire_pct = resps
    )
  })
  
  # Mise à jour automatique des sélecteurs de colonnes
  observe({
    req(values$df)
    df <- values$df
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    updateSelectInput(session, "dose_col", choices = num_cols, selected = num_cols[1])
    updateSelectInput(session, "resp_col", choices = num_cols, selected = num_cols[2])
  })
  
  # Ajustement du modèle Log-Logistique à 4 paramètres (LL.4)
  fit_model <- reactive({
    req(values$df, input$dose_col, input$resp_col)
    df <- values$df
    
    dose <- df[[input$dose_col]]
    resp <- df[[input$resp_col]]
    
    # Filtrage des doses strictly positives pour l'échelle log
    valid <- dose > 0 & !is.na(dose) & !is.na(resp)
    dose_val <- dose[valid]
    resp_val <- resp[valid]
    
    tryCatch({
      drm(resp_val ~ dose_val, fct = LL.4(names = c("Pente_Hill", "Min", "Max", "IC50")))
    }, error = function(e) { NULL })
  })
  
  # 1. Graphique de la courbe sigmoïde avec ligne de repère IC50
  output$curve_plot <- renderPlot({
    req(values$df, input$dose_col, input$resp_col)
    df <- values$df
    model <- fit_model()
    
    dose_var <- input$dose_col
    resp_var <- input$resp_col
    
    p <- ggplot(df, aes(x = .data[[dose_var]], y = .data[[resp_var]])) +
      geom_point(size = 3.5, color = "darkblue", alpha = 0.7) +
      theme_minimal(base_size = 15) +
      labs(x = paste("Concentration (", dose_var, ")"),
           y = paste("Réponse (", resp_var, ")"),
           title = "Courbe Dose-Réponse (Ajustement Log-Logistique 4PL)")
    
    if (input$log_scale) {
      p <- p + scale_x_log10()
    }
    
    # Superposition de la courbe ajustée et de l'IC50
    if (!is.null(model)) {
      min_dose <- min(df[[dose_var]][df[[dose_var]] > 0])
      max_dose <- max(df[[dose_var]])
      
      dose_seq <- exp(seq(log(min_dose), log(max_dose), length.out = 200))
      pred <- predict(model, newdata = data.frame(dose_val = dose_seq))
      df_pred <- data.frame(dose = dose_seq, resp = pred)
      
      ic50_val <- ED(model, 50, display = FALSE)[1]
      
      p <- p + 
        geom_line(data = df_pred, aes(x = dose, y = resp), color = "firebrick", linewidth = 1.2) +
        geom_vline(xintercept = ic50_val, linetype = "dashed", color = "darkgreen", linewidth = 1) +
        annotate("text", x = ic50_val, y = max(df[[resp_var]]), 
                 label = paste("  IC50 =", round(ic50_val, 4)), 
                 hjust = -0.1, color = "darkgreen", fontface = "bold", size = 5)
    }
    
    p
  })
  
  # 2. Tableau récapitulatif des paramètres pharmacologiques
  output$ic50_summary <- renderPrint({
    model <- fit_model()
    
    if (is.null(model)) {
      cat("Impossible d'ajuster le modèle 4PL. Vérifiez vos données d'entrée.")
    } else {
      cat("=========================================================\n")
      cat("  RÉSULTATS DE L'AJUSTEMENT PHARMACOLOGIQUE (4PL)\n")
      cat("=========================================================\n\n")
      print(summary(model))
      
      cat("\n---------------------------------------------------------\n")
      cat("  ESTIMATION DES CONCENTRATIONS EFFICACES (IC10, IC50, IC90)\n")
      cat("---------------------------------------------------------\n")
      print(ED(model, c(10, 50, 90), interval = "delta"))
    }
  })
  
  # 3. Affichage des données brutes
  output$raw_table <- renderTable({
    req(values$df)
    head(values$df, 20)
  })
}

# ==============================================================================
# 3. LANCEMENT DE L'APPLICATION
# ==============================================================================
shinyApp(ui = ui, server = server)

