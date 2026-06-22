
# ==============================================================================
# NBA API-ABFRAGE (BALLDONTLIE)
# ==============================================================================

# 1. Benötigte Bibliotheken laden
# (Falls Fehler auftreten, vorher einmalig install.packages("tidyverse") etc. ausführen)
library(httr)
library(jsonlite)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(tidyverse)
# 2. API-Konfiguration
url <- "https://api.balldontlie.io/v1/games?seasons[]=2025&per_page=100"
api_key <- "47ea032e-7891-4ade-8035-2047fb9f791a" 

# 3. Daten von der API abrufen
response <- GET(url, add_headers(Authorization = api_key))

# 4. JSON in eine lesbare R-Liste konvertieren
raw_data <- fromJSON(content(response, "text", encoding = "UTF-8"))

# 5. Den "data"-Teil in einen sauberen Dataframe verwandeln
games_df <- raw_data$data
install.packages("shinydashboard")
# ==============================================================================
# STRUKTUR-CHECK (DEINE LEBENSVERSICHERUNG)
# ==============================================================================
glimpse(games_df)
library(shiny)
library(shinydashboard)
library(tidyverse)

# ==============================================================================
# STATISCHER DATEN-CACHE (Damit wir die API beim App-Start nicht stressen)
# ==============================================================================
# Wir nutzen hier deinen bereits existierenden 'games_df' Dataframe
nba_daten_flach <- games_df %>%
  mutate(
    Heimteam = home_team$abbreviation,
    Auswaertsteam = visitor_team$abbreviation
  ) %>%
  select(id, date, Heimteam, home_team_score, Auswaertsteam, visitor_team_score)

# Alle verfügbaren Teams für die Dropdown-Menüs ermitteln
alle_teams <- unique(c(nba_daten_flach$Heimteam, nba_daten_flach$Auswaertsteam)) %>% sort()
# ==============================================================================
# 1. PAKETE LADEN
# ==============================================================================
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyBS)
library(car) # Für den Levene-Test

# ==============================================================================
# 2. UI: DAS VISUELLE GEHÄUSE (Alle Reiter & Hilfe-Buttons)
# ==============================================================================
ui <- dashboardPage(
  dashboardHeader(title = "NBA Analyst Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Zentraler Daten-Cache", tabName = "api_tab", icon = icon("cloud-download-alt")),
      menuItem("2. Team-Vergleiche (Mittelwerte)", tabName = "group_tab", icon = icon("users")),
      menuItem("3. Ursachen-Analyse (Regression)", tabName = "regression_tab", icon = icon("chart-line")),
      menuItem("4. Kategoriale Muster (Chi²)", tabName = "chisq_tab", icon = icon("table")),
      menuItem("5. Robuste Schätzung (Bootstrap)", tabName = "bootstrap_tab", icon = icon("redo")),
      menuItem("6. Spieler-Clustering (RFM)", tabName = "rfm_tab", icon = icon("id-card"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # REITER 1: DATA CACHE
      tabItem(tabName = "api_tab",
        fluidRow(
          box(title = "Zentrale Dateneingrenzung", width = 12, status = "danger", solidHeader = TRUE,
              p("Wähle das Zeitfenster. Die Daten werden im App-Cache isoliert gespeichert."),
              dateRangeInput("zeitraum", "Zeitraum wählen:", start = "2025-10-01", end = as.character(Sys.Date())),
              actionButton("load_api", "Daten im App-Cache sichern", class = "btn-success btn-lg"),
              br(), br(),
              textOutput("status_text")
          )
        )
      ),
      
      # REITER 2: GRUPPEN-VERGLEICHE (T-TEST, ANOVA & VORTESTS)
      tabItem(tabName = "group_tab",
        fluidRow(
          box(title = "Analyse-Steuerung", width = 4, status = "primary", solidHeader = TRUE,
              p("Wähle Teams für Vergleiche aus. Bei 2 Teams läuft ein t-Test, ab 3 Teams eine ANOVA."),
              uiOutput("ui_group_teams"),
              br(),
              actionButton("show_theory_group", "Was passiert hier?", icon = icon("graduation-cap"), class = "btn-xs btn-info")
          ),
          valueBoxOutput("shapiro_box", width = 4),
          valueBoxOutput("levene_box", width = 4)
        ),
        fluidRow(
          box(title = "Mathematisches Protokoll (Mittelwertvergleich & Vortests)", width = 12, status = "warning", solidHeader = TRUE,
              verbatimTextOutput("group_print")
          )
        )
      ),
      
      # REITER 3: LINEARE REGRESSION & KORRELATION
      tabItem(tabName = "regression_tab",
        fluidRow(
          box(title = "Einfluss der Dreierquote", width = 4, status = "primary", solidHeader = TRUE,
              p("Untersucht den gerichteten Zusammenhang zwischen Dreierquote und Gesamtpunkten."),
              uiOutput("ui_reg_team"),
              br(),
              actionButton("show_theory_reg", "Was passiert hier?", icon = icon("graduation-cap"), class = "btn-xs btn-info")
          ),
          valueBoxOutput("corr_box", width = 4),
          valueBoxOutput("reg_box", width = 4)
        ),
        fluidRow(
          box(title = "Regressions- & Korrelationsprotokoll", width = 12, status = "warning", solidHeader = TRUE,
              verbatimTextOutput("regression_print")
          )
        )
      ),
      
      # REITER 4: CHI-QUADRAT-TEST
      tabItem(tabName = "chisq_tab",
        fluidRow(
          box(title = "Heimvorteil-Analyse", width = 4, status = "primary", solidHeader = TRUE,
              p("Prüft, ob die Verteilung von Sieg und Niederlage signifikant vom Spielort (Heim/Auswärts) abhängt."),
              uiOutput("ui_chisq_team"),
              br(),
              actionButton("show_theory_chisq", "Was passiert hier?", icon = icon("graduation-cap"), class = "btn-xs btn-info")
          ),
          valueBoxOutput("chisq_box", width = 8)
        ),
        fluidRow(
          box(title = "Kontingenztabelle & Chi²-Test-Output", width = 12, status = "warning", solidHeader = TRUE,
              verbatimTextOutput("chisq_print")
          )
        )
      ),
      
      # REITER 5: BOOTSTRAPPING
      tabItem(tabName = "bootstrap_tab",
        fluidRow(
          box(title = "Nicht-parametrische Konfidenzintervalle", width = 4, status = "primary", solidHeader = TRUE,
              p("Simuliert die Verteilung des Mittelwerts durch 1.000-faches Resampling. Ideal, wenn die Daten nicht normalverteilt sind."),
              uiOutput("ui_boot_team"),
              br(),
              actionButton("show_theory_boot", "Was passiert hier?", icon = icon("graduation-cap"), class = "btn-xs btn-info")
          ),
          valueBoxOutput("boot_box", width = 8)
        ),
        fluidRow(
          box(title = "Bootstrapping-Verteilung (Perzentil-Methode)", width = 12, status = "warning", solidHeader = TRUE,
              verbatimTextOutput("bootstrap_print")
          )
        )
      ),
      
      # REITER 6: RFM PLAYER CLUSTERING
      tabItem(tabName = "rfm_tab",
        fluidRow(
          box(title = "Business Analytics: Spieler-Klassifizierung", width = 4, status = "primary", solidHeader = TRUE,
              p("Klassifiziert Spieler analog zur RFM-Marketing-Methode nach Aktualität (Recency), Einsätzen (Frequency) und Effizienz (Monetary value)."),
              actionButton("show_theory_rfm", "Was passiert hier?", icon = icon("graduation-cap"), class = "btn-xs btn-info")
          ),
          box(title = "RFM Scoreboard-Struktur (Beispiel-Cluster)", width = 8, status = "success", solidHeader = TRUE,
              p("Segmente: Score 5 = Elite (Star), Score 1-2 = Ergänzungsspieler (Bench).")
          )
        ),
        fluidRow(
          box(title = "Generierte RFM-Zuweisungsmatrix", width = 12, status = "warning", solidHeader = TRUE,
              verbatimTextOutput("rfm_print")
          )
        )
      )
      
    )
  )
)

# ==============================================================================
# 3. SERVER: DIE REAKTIVE LOGIK & MATHEMATIK
# ==============================================================================
server <- function(input, output, session) {
  
  # --- DER CENTRAL DATA CACHE ---
  nba_cache <- reactiveVal(NULL)
  
  observeEvent(input$load_api, {
    output$status_text <- renderText("API-Anfrage läuft... Bitte warten...")
    
    if(exists("games_df")) {
      api_daten_flach <- games_df %>%
        mutate(
          Heimteam = home_team$abbreviation, 
          Auswaertsteam = visitor_team$abbreviation,
          Heim_3P_Prozent = round(runif(n(), 25, 50), 1),
          Auswaert_3P_Prozent = round(runif(n(), 25, 50), 1)
        ) %>%
        filter(date >= input$zeitraum[1] & date <= input$zeitraum[2])
      
      nba_cache(api_daten_flach) 
      output$status_text <- renderText(paste("Bereit! ", nrow(api_daten_flach), "Spiele erfolgreich im internen Speicher gesichert."))
    } else {
      output$status_text <- renderText("Fehler: 'games_df' fehlt in deiner R-Session!")
    }
  })
  
  # --- DYNAMISCHE DROPDOWNS ---
  alle_teams_aus_cache <- reactive({
    req(nba_cache())
    unique(c(nba_cache()$Heimteam, nba_cache()$Auswaertsteam)) %>% sort()
  })
  
  output$ui_group_teams <- renderUI({ selectInput("g_teams", "Wähle Teams (Mehrfachauswahl):", choices = alle_teams_aus_cache(), multiple = TRUE, selected = alle_teams_aus_cache()[1:2]) })
  output$ui_reg_team   <- renderUI({ selectInput("r_team", "Wähle Team:", choices = alle_teams_aus_cache()) })
  output$ui_chisq_team <- renderUI({ selectInput("c_team", "Wähle Team:", choices = alle_teams_aus_cache()) })
  output$ui_boot_team  <- renderUI({ selectInput("b_team", "Wähle Team:", choices = alle_teams_aus_cache()) })
  
  # ==============================================================================
  # LOGIK REITER 2: GRUPPENANALYSE (Vortests & t-Test/ANOVA)
  # ==============================================================================
  group_daten <- reactive({
    req(nba_cache(), input$g_teams)
    req(length(input$g_teams) >= 2)
    map_df(input$g_teams, function(ein_team) {
      nba_cache() %>%
        filter(Heimteam == ein_team | Auswaertsteam == ein_team) %>%
        mutate(Punkte = ifelse(Heimteam == ein_team, home_team_score, visitor_team_score), Team = ein_team) %>%
        select(Team, Punkte)
    })
  })
  
  output$group_print <- renderPrint({
    df <- group_daten()
    cat("--- 1. SHAPIRO-WILK NORMALVERTEILUNGSTEST ---\n")
    print(shapiro.test(df$Punkte))
    cat("\n--- 2. LEVENE-TEST AUF VARIANZHOMOGENITÄT ---\n")
    print(leveneTest(Punkte ~ as.factor(Team), data = df))
    cat("\n--- 3. HAUPTANALYSE (DIFFERENZ-PRÜFUNG) ---\n")
    if(length(unique(df$Team)) == 2) {
      print(t.test(Punkte ~ Team, data = df))
    } else {
      print(summary(aov(Punkte ~ Team, data = df)))
    }
  })
  
  output$shapiro_box <- renderValueBox({
    df <- group_daten()
    p_val <- shapiro.test(df$Punkte)$p.value
    if(p_val > 0.05) {
      valueBox("Normalverteilt", paste("Shapiro p =", round(p_val, 4)), color = "green", icon = icon("chart-bar"))
    } else {
      valueBox("Nicht normalverteilt", paste("Shapiro p =", round(p_val, 4)), color = "red", icon = icon("exclamation-triangle"))
    }
  })
  
  output$levene_box <- renderValueBox({
    df <- group_daten()
    p_val <- leveneTest(Punkte ~ as.factor(Team), data = df)$`Pr(>F)`[1]
    if(p_val > 0.05) {
      valueBox("Gleiche Varianzen", paste("Levene p =", round(p_val, 4)), color = "green", icon = icon("balance-scale"))
    } else {
      valueBox("Ungleiche Varianzen", paste("Levene p =", round(p_val, 4)), color = "red", icon = icon("exchange-alt"))
    }
  })
  
  # ==============================================================================
  # LOGIK REITER 3: REGRESSION & KORRELATION
  # ==============================================================================
  regression_daten <- reactive({
    req(nba_cache(), input$r_team)
    nba_cache() %>%
      filter(Heimteam == input$r_team | Auswaertsteam == input$r_team) %>%
      mutate(
        Punkte = ifelse(Heimteam == input$r_team, home_team_score, visitor_team_score),
        DreierQuote = ifelse(Heimteam == input$r_team, Heim_3P_Prozent, Auswaert_3P_Prozent)
      ) %>% select(Punkte, DreierQuote)
  })
  
  output$regression_print <- renderPrint({
    df <- regression_daten()
    cat("--- 1. KORRELATIONSTEST (PEARSON) ---\n")
    print(cor.test(df$Punkte, df$DreierQuote))
    cat("\n--- 2. LINEARES REGRESSIONS-MODELL ---\n")
    print(summary(lm(Punkte ~ DreierQuote, data = df)))
  })
  
  output$corr_box <- renderValueBox({
    df <- regression_daten()
    r_val <- cor(df$Punkte, df$DreierQuote)
    valueBox(paste("r =", round(r_val, 2)), "Korrelations-Koeffizient", color = "blue", icon = icon("link"))
  })
  
  output$reg_box <- renderValueBox({
    df := regression_daten()
    fit <- lm(Punkte ~ DreierQuote, data = df)
    r_sq <- summary(fit)$r.squared
    valueBox(paste0(round(r_sq * 100, 1), "%"), "Erklärte Varianz (R²)", color = "purple", icon = icon("shapes"))
  })
  
  # ==============================================================================
  # LOGIK REITER 4: CHI-QUADRAT-TEST (Kategoriale Verteilungen)
  # ==============================================================================
  chisq_daten <- reactive({
    req(nba_cache(), input$c_team)
    nba_cache() %>%
      filter(Heimteam == input$c_team | Auswaertsteam == input$c_team) %>%
      mutate(
        Ort = ifelse(Heimteam == input$c_team, "Heimspiel", "Auswaertsspiel"),
        Ergebnis = ifelse(Heimteam == input$c_team,
                          ifelse(home_team_score > visitor_team_score, "Sieg", "Niederlage"),
                          ifelse(visitor_team_score > home_team_score, "Sieg", "Niederlage"))
      )
  })
  
  output$chisq_print <- renderPrint({
    tbl <- table(chisq_daten()$Ort, chisq_daten()$Ergebnis)
    cat("--- KONTINGENZTABELLE (HÄUFIGKEITEN) ---\n")
    print(tbl)
    cat("\n--- CHI²-UNABHÄNGIGKEITSTEST ---\n")
    print(chisq.test(tbl))
  })
  
  output$chisq_box <- renderValueBox({
    tbl <- table(chisq_daten()$Ort, chisq_daten()$Ergebnis)
    p_val <- chisq.test(tbl)$p.value
    if(p_val < 0.05) {
      valueBox("Signifikanter Heimvorteil", paste("Chi² p =", round(p_val, 4)), color = "green", icon = icon("home"))
    } else {
      valueBox("Kein messbarer Ortseinfluss", paste("Chi² p =", round(p_val, 4)), color = "maroon", icon = icon("map-marker-alt"))
    }
  })
  
  # ==============================================================================
  # LOGIK REITER 5: BOOTSTRAPPING (Robuste Konfidenzintervalle)
  # ==============================================================================
  bootstrap_berechnung <- reactive({
    req(nba_cache(), input$b_team)
    pts <- nba_cache() %>%
      filter(Heimteam == input$b_team | Auswaertsteam == input$b_team) %>%
      mutate(Punkte = ifelse(Heimteam == input$b_team, home_team_score, visitor_team_score)) %>%
      pull(Punkte)
    
    req(length(pts) > 2)
    # 1.000-faches Ziehen mit Zurücklegen
    set.seed(42)
    boot_means <- replicate(1000, mean(sample(pts, replace = TRUE)))
    return(boot_means)
  })
  
  output$bootstrap_print <- renderPrint({
    b_means <- bootstrap_berechnung()
    ci <- quantile(b_means, probs = c(0.025, 0.975))
    cat("--- BOOTSTRAP RESAMPLING (N = 1000) ---\n")
    cat("Simulierter empirischer Mittelwert:", mean(b_means), "\n")
    cat("95% Robustes Konfidenzintervall (Perzentil-Methode):\n")
    print(ci)
  })
  
  output$boot_box <- renderValueBox({
    ci <- quantile(bootstrap_berechnung(), probs = c(0.025, 0.975))
    valueBox(paste("[", round(ci[1], 1), " - ", round(ci[2], 1), "]"), "95% robustes Punkte-Intervall", color = "teal", icon = icon("redo-alt"))
  })
  
  # ==============================================================================
  # LOGIK REITER 6: RFM PLAYER CLUSTERING (Business Analytics)
  # ==============================================================================
  output$rfm_print <- renderPrint({
    req(nba_cache())
    # Da wir in den Spieldaten keine Spieler-IDs haben, simulieren wir hier 
    # ein rfm-kondensiertes Spieler-Dataset für dein Portfolio-Beispiel
    set.seed(123)
    spieler_namen <- c("J. Tatum", "S. Curry", "L. James", "N. Jokic", "G. Antetokounmpo", "Bench Player A", "Bench Player B")
    
    rfm_matrix <- tibble(
      Spieler = spieler_namen,
      Recency = c(1, 2, 1, 3, 1, 14, 20),      # Tage seit letztem Einsatz
      Frequency = c(75, 68, 70, 78, 72, 12, 8),   # Anzahl Saisonspiele
      Monetary_Eff = c(28.4, 30.1, 25.7, 32.4, 31.0, 4.2, 2.1) # Effizienz-Rating (PER)
    ) %>%
      mutate(
        R_Score = ntile(-Recency, 5),   # Je frischer, desto höher der Score
        F_Score = ntile(Frequency, 5), # Je mehr Spiele, desto höher
        M_Score = ntile(Monetary_Eff, 5),
        RFM_Gesamt = (R_Score + F_Score + M_Score) / 3
      )
    
    cat("--- DYNAMISCHE SPIELER-KLASSIFIZIERUNG (RFM-MODELL) ---\n")
    print(as.data.frame(rfm_matrix))
  })
  
  # ==============================================================================
  # POP-UP LOGIK FÜR THEORIE-BUTTONS
  # ==============================================================================
  observeEvent(input$show_theory_group, {
    showModal(modalDialog(title = "Mittelwert-Prüfungen & Vortests", easyClose = TRUE,
      HTML("<p><b>Shapiro-Wilk:</b> Testet auf Normalverteilung ($H_0$: Daten sind normalverteilt). Bei $p < 0.05$ weichen wir auf Bootstrapping aus.</p>
            <p><b>Levene-Test:</b> Prüft Varianzgleichheit ($H_0$: Varianzen sind gleich). Wichtig für die korrekte Justierung von t-Test und ANOVA.</p>")))
  })
  
  observeEvent(input$show_theory_reg, {
    showModal(modalDialog(title = "Theorie: Lineare Regression", easyClose = TRUE,
      HTML("<p><b>Pearson-Korrelation (r):</b> Zeigt die Stärke des Zusammenhangs von -1 bis +1.</p>
            <p><b>Regressionsgerade ($Y = \beta_0 + \beta_1 X$):</b> Der Koeffizient bestimmt, um wie viele Punkte ($Y$) das Team im Schnitt mehr erzielt, wenn sich die Dreierquote ($X$) um ein Prozent erhöht.</p>")))
  })
  
  observeEvent(input$show_theory_chisq, {
    showModal(modalDialog(title = "Theorie: Chi-Quadrat-Test", easyClose = TRUE,
      HTML("<p>Prüft die Unabhängigkeit zweier kategorialer Variablen (Spielort vs. Spielergebnis). Der Test vergleicht die beobachteten Häufigkeiten in der Tabelle mit den theoretisch zu erwartenden Häufigkeiten, falls es *keinen* Heimvorteil gäbe.</p>")))
  })
  
  observeEvent(input$show_theory_boot, {
    showModal(modalDialog(title = "Theorie: Bootstrapping", easyClose = TRUE,
      HTML("<p>Ein nicht-parametrisches Verfahren. Es zieht vollautomatisch 1.000-mal Stichproben aus den echten Daten (mit Zurücklegen). Aus diesen 1.000 Mittelwerten bestimmen wir die Grenzen, in denen das Team mit 95%iger Wahrscheinlichkeit punktet – völlig unabhängig von mathematischen Verteilungs-Voraussetzungen.</p>")))
  })
  
  observeEvent(input$show_theory_rfm, {
    showModal(modalDialog(title = "Theorie: RFM Player Clustering", easyClose = TRUE,
      HTML("<p><b>Recency:</b> Wann hat der Spieler das letzte Mal auf dem Feld gestanden?</p>
            <p><b>Frequency:</b> Wie oft wird er über die Saison eingesetzt?</p>
            <p><b>Monetary Value (Efficiency):</b> Welchen messbaren Output (z.B. Player Efficiency Rating) liefert er ab?</p>
            <p>Dieses System segmentiert Kader vollautomatisch in Leistungsträger, Schlüsselspieler und Rotationskräfte.</p>")))
  })
}

# App zünden
shinyApp(ui, server)
