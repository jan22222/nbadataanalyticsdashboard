# ==============================================================================
# Dieses Skript beinhaltet die erste Version der App mit t-test und lokalem 
# Zwischenspeichern der Data - erstmal lokales hosting. 
# Vorhaben: Erst die features lokal umsetzen, dann komplett als shiny app upload 
# ==============================================================================


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
# 1. UI: DIE OBERFLÄCHE (Das Design)
# ==============================================================================
ui <- dashboardPage(
  dashboardHeader(title = "NBA Analytics App"),
  
  # Die Seitenleiste mit unseren Reitern
  dashboardSidebar(
    sidebarMenu(
      menuItem("Team-Vergleich (t-Test)", tabName = "ttest_tab", icon = icon("basketball-ball")),
      menuItem("ANOVA (Feature 1.2)", tabName = "anova_tab", icon = icon("chart-line"), badgeLabel = "bald", badgeColor = "orange")
    )
  ),
  
  # Der Hauptinhalt der Seite
  dashboardBody(
    tabItems(
      # INHALT FÜR REITER 1
      tabItem(tabName = "ttest_tab",
        fluidRow(
          # Filter-Box für den User
          box(title = "Team-Auswahl", width = 4, status = "primary", solidHeader = TRUE,
              selectInput("team1", "Wähle Team 1:", choices = alle_teams, selected = "BOS"),
              selectInput("team2", "Wähle Team 2:", choices = alle_teams, selected = "LAL")
          ),
          # Große Ergebnis-Kachel oben
          valueBoxOutput("urteil_box", width = 8)
        ),
        
        fluidRow(
          # Der mathematische Output des t-Tests
          box(title = "Statistische Abrechnung (t-Test)", width = 12, status = "warning", solidHeader = TRUE,
              verbatimTextOutput("ttest_print")
          )
        ),
        
        fluidRow(
          # Die gefilterte Tabelle ganz unten zum Nachprüfen
          box(title = "Eingeflossene Spiele (Datenbasis)", width = 12, status = "info", solidHeader = TRUE,
              tableOutput("daten_tabelle")
          )
        )
      )
    )
  )
)

# ==============================================================================
# 2. SERVER: DIE LOGIK (Das Gehirn)
# ==============================================================================
server <- function(input, output, session) {
  
  # Reaktive Datenvorbereitung: Reagiert sofort, wenn der User die Teams ändert!
  gefilterte_daten <- reactive({
    req(input$team1, input$team2)
    
    # 1. Spiele filtern
    spiele <- nba_daten_flach %>%
      filter(
        (Heimteam == input$team1 & Auswaertsteam == input$team2) |
        (Heimteam == input$team2 & Auswaertsteam == input$team1) |
        (Heimteam == input$team1 | Auswaertsteam == input$team1) |
        (Heimteam == input$team2 | Auswaertsteam == input$team2)
      )
    
    # 2. Ins Long-Format für den Test bringen
    p_t1 <- spiele %>%
      mutate(Punkte = ifelse(Heimteam == input$team1, home_team_score, 
                             ifelse(Auswaertsteam == input$team1, visitor_team_score, NA))) %>%
      filter(!is.na(Punkte)) %>% mutate(Team = input$team1) %>% select(date, Team, Punkte)
      
    p_t2 <- spiele %>%
      mutate(Punkte = ifelse(Heimteam == input$team2, home_team_score, 
                             ifelse(Auswaertsteam == input$team2, visitor_team_score, NA))) %>%
      filter(!is.na(Punkte)) %>% mutate(Team = input$team2) %>% select(date, Team, Punkte)
      
    bind_rows(p_t1, p_t2)
  })
  
  # AUSGABE 1: Der nackte R-t-Test im Textfeld
  output$ttest_print <- renderPrint({
    daten <- gefilterte_daten()
    req(nrow(daten) >= 2) # t-Test braucht mindestens 2 Beobachtungen
    t.test(Punkte ~ Team, data = daten)
  })
  
  # AUSGABE 2: Das visuelle Urteil als große Kachel (Value Box)
  output$urteil_box <- renderValueBox({
    daten <- gefilterte_daten()
    if(nrow(daten) < 4) {
      return(valueBox("Zu wenig Daten", "Wähle andere Teams", icon = icon("exclamation-triangle"), color = "yellow"))
    }
    
    test <- t.test(Punkte ~ Team, data = daten)
    
    if (test$p.value < 0.05) {
      valueBox("Signifikanter Unterschied!", paste("p-Wert:", round(test$p.value, 4)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox("Kein Unterschied (Zufall)", paste("p-Wert:", round(test$p.value, 4)), icon = icon("arrow-right"), color = "maroon")
    }
  })
  
  # AUSGABE 3: Die Tabelle unten
  output$daten_tabelle <- renderTable({
    # Wir zeigen dem User die Spiele im lesbaren Wide-Format
    nba_daten_flach %>%
      filter(Heimteam %in% c(input$team1, input$input$team2) | Auswaertsteam %in% c(input$team1, input$team2))
  })
}

# App starten
shinyApp(ui, server)