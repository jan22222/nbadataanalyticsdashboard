library(shiny)

# 1. Die Benutzeroberfläche (UI) - Hier kommt später das Design hin
ui <- fluidPage(
  titlePanel("NBA Data Analytics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      p("Willkommen! Dieses Dashboard befindet sich aktuell in der aktiven Entwicklung."),
      p("Hier entstehen in den nächsten Tagen die statistischen Analysen (t-Test, ANOVA, Regression) zu den NBA-API-Daten.")
    ),
    
    mainPanel(
      h3("Projekt-Status: Skelett erfolgreich aufgesetzt"),
      p("Die Live-Datenverbindung und die interaktiven Grafiken werden schrittweise implementiert.")
    )
  )
)

# 2. Die Server-Logik - Hier schlägt später das mathematische Herz
server <- function(input, output, session) {
  # Aktuell noch leer, da noch keine Berechnungen stattfinden
}

# 3. Start-Befehl für die App
shinyApp(ui = ui, server = server)